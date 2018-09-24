# 2018_JP_ApparentCa

This is code to run the simulations as described in the following publication:

Ritzau-Jost A, Jablonski L, Viotti J, Lipstein N, Eilers J, Hallermann S. Apparent calcium dependence of vesicle recruitment. J Physiol. 2018 [doi: 10.1113/JP275911](https://physoc.onlinelibrary.wiley.com/doi/abs/10.1113/JP275911)

We used two complementary approaches: a simple two-pool model implemented in C++ (Fig. 5; see folder /twopool-model) and a 3D-model implemented in Calcium Calculator modeling software ([CalC](https://web.njit.edu/~matveev/) developed by Victor Matveev, NJIT, Newark, USA) and Wolfram Mathematica (Fig. 6; see folder/3D-model).

## Simple two-pool model

The code to fit the control and EGTA data are provided in two different subfolders (control and EGTA). To execute the code you need to compile main.cpp in folders /xcode/twopool and the other provided .cpp and .h files and run main.cpp. You have to change the import and export folder names in line 10 and 11 of main.cpp to the appropriate paths on your computer.

First, the text file named apTimesSamp300.txt in folder /in is loaded. It contains three columns:  the time of stimuli (in ms), the weighting for chi2, and the factor for the postsynaptic depression as quantified in Hallermann et al. (Neuron, 2010). Next, the text files 1_300Hz.txt, 2_300Hz.txt, etc. in folder /in are loaded. These files contain phasic EPSC amplitudes of different experiments. In the here provided case only one file containing the average phasic EPSC amplitudes for either control or EGTA data are loaded (cf. howManyExperiments = 1, line 18, main.cpp).

The number of free parameters is defined by ndim (line 16, main.cpp) with start values defined in line 446ff. in main.cpp. After optimization, several text files are written in folder /out. In this folder, the Mathematica file plot.nb (also provided as PDF file plot.pdf) imports the data and plots the best-fit simulation results superimposed on the experimental data as well as the best-fit parameters.

To fit the control data, three parameters are free: the number of high-pr vesicles (n2tot), the release probability of the high-pr vesicles (U2), and the recruitment rate of the low-pr vesicles (k1). The remaining parameters were fixed to values obtained in Hallermann et al. (2010; line 246ff, main.cpp).

To fit the EGTA data, the code is identical to the one in folder /control but n2tot and k were fixed to the control value and two parameters are free: U2 and the release probability of the low-pr vesicles (U1). 

Note, that some of the resulting the best-fit parameters are slightly different from the values provided in Ritzau-Jost et al. (J Physiol, 2018), because following publication small errors in the code were corrected. Particularly, for control, U2 is 0.89 instead of 0.88 and k1 is 52.1/s instead of 50.1/s. For EGTA, U2 and U1 are 0.78 and 0.22 instead of 0.76 and 0.24, respectively, resulting in a reduction of U2 and U1 by 12% and 51% instead of 13% and 44%, respectively. We apologize for this error.


## 3D-model

Executing the Mathematica file 3D-model.nb, which requires file in folder /core, will generate and run a CalC script with CalC, import the resulting output from CalC, and analyze and plot the results. 

To run on Windows x64 set CalC["version"]="cwin691x64" in "3D-model.nb"
To run on macOS set CalC["version"]="cmac691" in "3D-model.nb"
CalC executables are provided in "\core\calc\" and should stay there.

Most parameters are constrained to the values provided in [Delvendahl et al. (PNAS, 2015)](http://www.pnas.org/content/112/23/E3075.long). Two types of vesicles with different coupling distance to the calcium channels were considered. To reproduce the control data, the coupling distances were set to 6.5 and 15 nm resulting in a release probability of 0.88 and 0.44 for the high- and low-pr vesicles, respectively, as obtained with the simple two-pool model. To reproduce the EGTA data, the EGTA concentration was increased from 100 µM to 20 mM.

For more information contact hallermann@medizin.uni-leipzig.de or lukasz.jablonski@wp.eu 
