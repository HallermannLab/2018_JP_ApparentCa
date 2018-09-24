#include <math.h>
#include <string>
#include <fstream>
#include <iostream>
#include <iomanip>
#include "nr.h"	
using namespace std;

//------------------------------------------------------------------------------------------------------------------------
#define importFolderName    "/Users/hallermann/Desktop/twopool EGTA/in/"
#define exportFolderName    "/Users/hallermann/Desktop/twopool EGTA/out/"
//-------------------------------------------------------------------------------------------------------------------------

//number of free parameters
//if this is changed, some stuff in main() and setrates() has to be changed
#define ndim 2

#define howManyExperiments 1    //number of experiments imported from importFolderName with names 1_300Hz.txt, 2_300Hz.txt, 3_300Hz.txt etc.


//pools and rates
double nSupplPerN2;     //number of vesicles that are recruitable for each high-pr vesicle
double n0tot;           //number of vesicles in supply pool
double n1tot;           //number of vesicles in pool1 = number of low-pr release sites
double n2tot;           //number of vesicles in pool2 = number of high-pr release sites
double k0, k1, k2;      //refilling rate constants of pool0, 1 and, 2 in s^-1
#define statesN 3       //number of pools
Vec_DP states(statesN);

//pr model in Markram et al. PNAS 1997
double U10;             //initial pr of vesicles in pool1
double U20;             //initial pr of vesicles in pool2
double tauF;            ///time constant of facilitation

//define arrays for data
float minisize[100];    //size in miniature EPSC in pA for each experiment; is here set constant to 20 pA
int apNumber300;        //number of stimuli. Is set in load().
float apTimes300[1000]; //time of stimuli in s
float apSamp300[1000];  //sampling (weighting) of stimuli for chi2 calculation
float apPostSyn300[1000];   //factor describing postsynaptic depression for each stimulus
float apAmp300[1000];       //amplitude of EPSC in units of released vesicles (= measured phasic EPSC amplitude/mini size of corresponding file)
                            //CAVE this value is contaminated by postsynaptic depression
                            //in export(), this value is multiplied by the mini size of the corresponding file resulting in units pA
float apAmpSim300[1000];    //amplitude of simulated EPSC in units of released vesicles (taking postsyn. depression into account, cf. 275xx)
                            //in output(), this value is multiplied by mini size of the corresponding file resulting in units pA
float apAmpStore300[1000][howManyExperiments];      //corresponding values stored for each experiment
float apAmpSimStore300[1000][howManyExperiments];   //corresponding values stored for each experiment
float export300[1000][5];                           //size of pool0, pool1, pool2, and release probability of pool1 and pool2 for each stimulus
float exportStore300[1000][howManyExperiments][5];  //corresponding values stored for each experiment

//define global arrays used in simplex and chi2
const int mpts = ndim+1;		
double p[mpts][ndim];	    //that are e.g. 4 3-dimensional vectors
double psum[ndim];
double y[mpts];			    //contains the chi values of the points of the simplex
double ptry[ndim];	    	//used in chi2()
double pBest[ndim];	    	//set in simplex()
double pBestStore[ndim][howManyExperiments];
double start[ndim];
double startMerk[ndim];
double startDelta[ndim];
double startDeltaMerk[ndim];
double chi2SumBest;       //set in simplex()
double chi2SumBestStore[howManyExperiments];

//parameters for repeated simplex and odeint
double reduceFactor = 1.5;
double ftol = 1e-5;
const int KMAX=5000;
DP eps=0.0001;              //precision
DP h1=500e-6;               //inital step guess within odeint
DP hmin=0;                  //step should be larger than //1e-10;
int nrhs;                   //counts function evaluations
int nbad,nok;               //count bad and good steps
DP dxsav;                   //defining declarations
int kmax,kount;

// define general function
double sqr(double x){ 	return x*x;
}

/////////////////////////////////////////////////////////////////////////////////////////////
// load data into the global variables //////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
void load(){
	int i,ii,iii,fileCount;
	FILE *fd;
	char filename[300];
	char buf[5];
	
    // aptimes + sampling (weighting) + postsyn. depression
	strcpy(filename,importFolderName);
    strcat(filename,"apTimesSamp300.txt");
	fd=fopen(filename,"r");
	i=0;
	while(fscanf(fd, "%f", &apTimes300[i]) != EOF)	{
		fscanf(fd, "%f", &apSamp300[i]);
		fscanf(fd, "%f", &apPostSyn300[i]);
		apTimes300[i] /= 1000.;//convert ms in s
		i++;
	}
	fclose(fd);
	apNumber300=i;

    // mini size
    for(ii=0;ii<howManyExperiments;ii++) {
        minisize[ii]=20.; //assumung 20 pA mini size
    }

	// currents
	// -----------   300 Hz  ---------------
	fileCount =0;
	for(iii=0;iii<1;iii++) {
		for(ii=1;ii<=howManyExperiments;ii++) {
			strcpy(filename,importFolderName);
			sprintf(buf,"%d",ii);// convert ii to string [buf]
			strcat(filename,buf);
			strcat(filename,"_300Hz.txt");
            cout << "I loaded " << filename << endl;
			fd=fopen(filename,"r");
			for(i=0;i<apNumber300;i++){
				fscanf(fd, "%f", &apAmpStore300[i][fileCount]);
				apAmpStore300[i][fileCount] /= -1. * minisize[fileCount];
			}
			fclose(fd);
			fileCount++;
		}
	}
}

/////////////////////////////////////////////////////////////////////////////////////////////
// output in txt files    ///////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
void output()
{
	int i,ii;
	FILE *fd,*fdSim;
	FILE *fdrates;
	char filename[300];

	//export EPSC amplitude
	strcpy(filename,exportFolderName);
	strcat(filename,"apAmp300.txt");
	fd=fopen(filename,"w");
	strcpy(filename,exportFolderName);
	strcat(filename,"apAmp300Sim.txt");
	fdSim=fopen(filename,"w");
	//experimental data
	for(i = 0; i < apNumber300; i += 1){
		for(ii=0;ii<howManyExperiments;ii++)	fprintf(fd,"%f	",minisize[ii]*apAmpStore300[i][ii]);
		fprintf(fd,"\n");
	}
	//simulated data
	for(i = 0; i < apNumber300; i += 1){
		for(ii=0;ii<howManyExperiments;ii++)	fprintf(fdSim,"%f	",minisize[ii]*apAmpSimStore300[i][ii]);
		fprintf(fdSim,"\n");
	}
	fclose(fd);
	fclose(fdSim);

   //export pool sizes and release probability
	strcpy(filename,exportFolderName);	strcat(filename,"n0_300.txt");	fd=fopen(filename,"w");
	for(i = 0; i < apNumber300; i += 1){		
		for(ii=0;ii<howManyExperiments;ii++)	fprintf(fd,"%e	",exportStore300[i][ii][0]);
		fprintf(fd,"\n");
	}
	fclose(fd);
	strcpy(filename,exportFolderName);	strcat(filename,"n1_300.txt");	fd=fopen(filename,"w");
	for(i = 0; i < apNumber300; i += 1){		
		for(ii=0;ii<howManyExperiments;ii++)	fprintf(fd,"%e	",exportStore300[i][ii][1]);
		fprintf(fd,"\n");
	}
	fclose(fd);
	strcpy(filename,exportFolderName);	strcat(filename,"n2_300.txt");	fd=fopen(filename,"w");
	for(i = 0; i < apNumber300; i += 1){		
		for(ii=0;ii<howManyExperiments;ii++)	fprintf(fd,"%e	",exportStore300[i][ii][2]);
		fprintf(fd,"\n");
	}
	fclose(fd);
	strcpy(filename,exportFolderName);	strcat(filename,"p1_300.txt");	fd=fopen(filename,"w");
	for(i = 0; i < apNumber300; i += 1){		
		for(ii=0;ii<howManyExperiments;ii++)	fprintf(fd,"%e	",exportStore300[i][ii][3]);
		fprintf(fd,"\n");
	}
	fclose(fd);
	strcpy(filename,exportFolderName);	strcat(filename,"p2_300.txt");	fd=fopen(filename,"w");
	for(i = 0; i < apNumber300; i += 1){		
		for(ii=0;ii<howManyExperiments;ii++)	fprintf(fd,"%e	",exportStore300[i][ii][4]);
		fprintf(fd,"\n");
	}
	fclose(fd);

	//export timebase
	strcpy(filename,exportFolderName);
	strcat(filename,"time300.txt");
	fd=fopen(filename,"w");
	for(i = 0; i < apNumber300; i += 1){
		fprintf(fd,"%f	",apTimes300[i]);
		fprintf(fd,"\n");
	}
	fclose(fd);

    //export best rates and chi2
	strcpy(filename,exportFolderName);
	strcat(filename,"rates.txt");
	fdrates=fopen(filename,"w");
	for(ii=0;ii<howManyExperiments;ii++){
		for(i=0;i<ndim;i++)	fprintf(fdrates,"%e	", pBestStore[i][ii]);
		fprintf(fdrates,"%e\n", chi2SumBestStore[ii]);
	}
	fclose(fdrates);

	printf("output into files done!");
}

// differential equation for pools
void derivs(const DP x, Vec_I_DP &yy, Vec_O_DP &dydx)        //use yy and not y to prevent confusion with global y
{
	dydx[0] = k0*(n0tot-yy[0]) -k1*(yy[0]/n0tot)*(n1tot-yy[1]);
	dydx[1] = k1*(yy[0]/n0tot)*(n1tot-yy[1])  -k2*(yy[1]/n1tot)*(n2tot-yy[2]);
	dydx[2] = k2*(yy[1]/n1tot)*(n2tot-yy[2]);
}

void initPools(){
	states[0] = n0tot;
	states[1] = n1tot;
	states[2] = n2tot;
}

void fadvance(double timestep){
    kmax=KMAX;
    NR::odeint(states,0.0,timestep,eps,h1,hmin,nok,nbad,derivs,NR::rkqs);
}

/////////////////////////////////////////////////////////////////////////////////////////////
//  setRates      ///////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
void setRates() 
{
    //varried values
    U20 = ptry[0];
    U10 = ptry[1];
    
    //fixed values
    n2tot = 2.4655;            //from control data of Ritzau-Jost et al. JPhysiol 2018;
    k1 = 52.1365;               //from control data of Ritzau-Jost et al. JPhysiol 2018;
    k0 = 0.031;                 //based on 27% recovery of pool0 after 10 s (Saviane & Silver Nature 2006) => tau=32s => k=0.031
    k2 = 0.5;                   //based on tau slow of recovery of about 2 s (Hallermann et al. Neuron 2010)
    tauF = 0.012;               //see Saviane & Silver Nature 2006
    nSupplPerN2 = 300;          //see Saviane & Silver Nature 2006

    //derived values
    n1tot = 4*n2tot;
    n0tot = n2tot*nSupplPerN2;
}

/////////////////////////////////////////////////////////////////////////////////////////////
//  chi2 ////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
double chi2()
{
	int i;
	double time,nextApTime;
	double U1,U2;
    double chi2Sum;

    setRates();
	//define start conditions
	initPools();
	U1 = U10;
	U2 = U20;
	i=0;
	while(1)	{
		time = apTimes300[i];
		apAmpSim300[i] = (float)(states[1]*U1 + states[2]*U2)*apPostSyn300[i];  //number of released vesciles multipied by postsynaptic depression
        states[1] = (1 - U1)*states[1];     //subtract released vesicles
        states[2] = (1 - U2)*states[2];     //subtract released vesicles
		//for export
		export300[i][0]=(float)states[0];
		export300[i][1]=(float)states[1];
		export300[i][2]=(float)states[2];
		export300[i][3]=(float)U1;
		export300[i][4]=(float)U2;

		if(i>=apNumber300-1) break;         //last AP
        //calculate pr according to Markram et al. PNAS 1997
        nextApTime = apTimes300[i+1];
		U1 = U1*exp(-(nextApTime-time)/tauF)+U10*(1-U1*exp(-(nextApTime-time)/tauF));	//at the last AP this sould lead to nonsence but is not used anymore
		U2 = U2*exp(-(nextApTime-time)/tauF)+U20*(1-U2*exp(-(nextApTime-time)/tauF));	//at the last AP this sould lead to nonsence but is not used anymore
        //calculate the development of the number of vesicles in the pools until next AP
		fadvance(nextApTime - time);
		time = nextApTime;
		i++;
	}
	chi2Sum=0;
	for(i=0;i<apNumber300;i++)
		chi2Sum += apSamp300[i]*sqr(apAmp300[i] - apAmpSim300[i]);
	return(chi2Sum);
}

void start_vectors()
{
	int i,j;
	for(i=0;i<mpts;i++)	{
		for(j=0;j<ndim;j++)
			p[i][j] = (start[j]);
	}
	for(j=0;j<ndim;j++)
		p[j+1][j] = (start[j] + startDelta[j]);
}

void get_psum()
{
	int i,j;
	double sum; 
	//p and psum are global

	for(j=0;j<ndim;j++)	{
		for (sum=0,i=0;i<mpts;i++)
			sum += p[i][j];
		psum[j] = sum;
	}
}

double amotry(int ihi, double fac)
{
	int j;
	double fac1,fac2,ytry;
	fac1 = (1.0-fac)/ndim;
	fac2 = fac1-fac;
	for(j=0;j<ndim;j++)
		ptry[j] = psum[j]*fac1 - p[ihi][j]*fac2;
	//check for broders at the moment only >0 
	for(j=0;j<ndim;j++)
		if(ptry[j] <= 0) return y[ihi];

	ytry = chi2();	//uses ptry
	if(ytry < y[ihi])	{
		y[ihi] = ytry;
		for(j=0;j<ndim;j++)		{
			psum[j] += ptry[j]-p[ihi][j];
			p[ihi][j] = ptry[j];
		}
	}
	return ytry;
}

void simplex()
{
	//see Press et al., Numerical recipes in C++ 2nd ed. p. 415
	const int NMAX = 5000;
	const double TINY=1.0e-10;
	int i,ihi,ilo,inhi,j,k;
	double rtol,ysave,ytry;
	int nfunk = 0;
	for(i=0;i<mpts;i++)
	{
		for(k=0;k<ndim;k++)
			ptry[k] = p[i][k];
		y[i] = chi2();	//uses ptry
	}
	get_psum();
	for(;;)
	{
		//find highest next-highest and lowest
		ilo=0;
		//ihi = y[0]>y[1] ? (inhi=1,0) : (inhi=0,1);
        if (y[0]>y[1])
        {
            ihi=0;
            inhi=1;
        }
        else
        {
            ihi=1;
            inhi=0;
        }
        for(i=0;i<mpts;i++)
		{
			if(y[i] <= y[ilo]) ilo=i;
			if(y[i] > y[ihi])
			{
				inhi=ihi;
				ihi=i;
			}
			else if(y[i] > y[inhi] && i!=ihi) inhi=i;
		}
		rtol = 2.0*fabs(y[ihi]-y[ilo])/(fabs(y[ihi])+fabs(y[ilo])+TINY);

		if(rtol < ftol)         //finish and store best values and best chi2 in global variables
		{
			for(k=0;k<ndim;k++)
				pBest[k] = p[ilo][k];	//used in output()
			chi2SumBest=y[ilo];
			break;
		}

		if(nfunk >= NMAX)
		{
			printf("\n\nNMAX exceeded!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1\n\n");
			for(k=0;k<ndim;k++)
				pBest[k] = p[ilo][k];	//used in output()
			break;
		}
		nfunk += 2;
		//new iteration
		ytry = amotry(ihi,-1.0);
		if(ytry <= y[ilo])	//better than best point, so try aditional extrapol.
		{				
			ytry = amotry(ihi,2.0);
		}
		else
			if(ytry  >= y[inhi])		//worse than 2nd highest so look for intermediate lower point
			{
				ysave = y[ihi];
				ytry = amotry(ihi,0.5);
				if(ytry >= ysave)		//cant get rid of highest point, so contract around the best (lowest) point
				{
					for(i=0;i<mpts;i++)
					{
						if(i != ilo)
						{
							for(j=0;j<ndim;j++)
								p[i][j] = ptry[j] = 0.5*(p[i][j]+p[ilo][j]);
							y[i] = chi2();	//uses ptry
						}
					}
					nfunk += ndim;
					get_psum();
				}
			}
			else --nfunk;
	}
	printf("\n\ndone!\n");
}

/////////////////////////////////////////////////////////////////////////////////////////////
// main /////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[])
{
	int k,m,ii,l,ll;
	double chi2Merk,chi2RelChange;

	//define start values that should be optimized
    startMerk[0] = 0.7;         //U2
    startDeltaMerk[0] = 0.1;
    
    startMerk[1] = 0.3;         //U1
    startDeltaMerk[1] = 0.2;
    
	load();
	for(ii=0;ii<howManyExperiments;ii++){
		printf("\n\nNow file: %d\n",ii);
		for(k=0;k<ndim;k++){
            pBest[k] = startMerk[k];
			startDelta[k] = startDeltaMerk[k];
		}
		//load Experiment into correctly used array
		for(l=0;l<apNumber300;l++) apAmp300[l] = apAmpStore300[l][ii];
		// minimazation
		chi2Merk=1e100;
		chi2RelChange = 1;
		m=0;
		while(m<10 && chi2RelChange > 0.0001){          //restart minimazation with best values + deltas and reduce deltas -> chance of jumping out of local minimum
			for(k=0;k<ndim;k++)	{
				start[k] = pBest[k];
                if(m>0) pBest[k] += startDelta[k];      //do not add deltas when done for the first time
				startDelta[k] /= reduceFactor;
			}
			start_vectors();
			simplex();//will set pBest and chi2SumBest
            
			printf("simplex %d. th time: chi2 = %f\n",m+1,chi2SumBest);
			for(k=0;k<ndim;k++)	{
				printf("%e	",pBest[k]);
			}
			printf("\n");
			chi2RelChange = fabs(chi2Merk-chi2SumBest)/chi2SumBest;
			chi2Merk = chi2SumBest;
			m++;
		}
		//do it again with best paramters
		for(k=0;k<ndim;k++)
			ptry[k] = pBest[k];
		chi2SumBestStore[ii] = chi2();//is also used to generate apAmpSim with the best parameters
		//store current for exort in outStartAndEnd()
		for(l=0;l<apNumber300;l++) apAmpSimStore300[l][ii] = apAmpSim300[l];
        //store pool size and pr for exort in outStartAndEnd()
		for(l=0;l<apNumber300;l++) for(ll=0;ll<5;ll++) exportStore300[l][ii][ll] = export300[l][ll];
        //store parameters for exort in outStartAndEnd()
		for(k=0;k<ndim;k++)	pBestStore[k][ii] = pBest[k];
	}
	output();
}
