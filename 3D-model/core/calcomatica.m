(* ::Package:: *)

BeginPackage["CalCoMatica`"]
(* CalCoMatica *)
(* version 4.8.1 *)
(* by Lukasz Jablonski *)

(* Licensed under the MIT License (MIT) *)
(* https://doi.org/10.5281/zenodo.1286481 *)
(* https://github.com/lukaszjablonski/CalCoMatica *)

  CalCForm::usage="CalCForm[number,fracMax] returns 'number' converted to CalC format with 'fracMac' number of digits of fractional part."
  AddBuffer::usage="AddBuffer[bufName,bufD,bufKplus,bufKminus,bufTotal,bufBC] adds simple buffer 'bufName'.\nAddBuffer[bufName,bufD,bufTotal,bufBC,Treaction,Rreaction,Vsite] adds cooperative buffer 'bufName'."
  RemoveBuffer::usage="RemoveBuffer[bufName] removes added simple or cooperative buffer 'bufName'."
  RemoveBuffers::usage="RemoveBuffers[] removes all added buffers."
  GenerateBuffer::usage="GenerateBuffer[] generates buffers definitions for CalC."
  GenerateCooperativeBuffer::usage="GenerateCooperativeBuffer[] generates cooperative buffers definitions for CalC."
  Vesicle::usage="Vesicle[x,y,z] sets 'x', 'y' and 'z' coordinates of vesicle."
  Local::usage="Local[x,y,z] sets 'x', 'y' and 'z' coordinates to calculate local Ca concentration at."
  GenerateLocal::usage="GenerateLocal[x,y,z] generates local Ca concentration plot definitions for CalC."
  LoadParams::usage="LoadParams[fileNameSuffix] loads notebook \"params.nb\" with parameters. Optional 'fileNameSuffix' loads \"params.fileNameSuffix.nb\"."
  LoadPlugin::usage="LoadPlugin[pluginName] loads notebook \"pluginName.nb\" with plugin."
  LoadDye::usage="LoadDye[dyeNumber] loads dye 'dyeNumber' parameters."
  PrintDye::usage="PrintDye[dyeNumber] prints dye 'dyeNumber' parameters."
  PrintDyeName::usage="PrintDyeName[dyeNumber] prints dye 'dyeNumber' name."
  DyeName::usage="DyeName[dyeNumber] returns dye 'dyeNumber' name."
  TrainTimeAndInterval::usage="TrainTimeAndInterval[] calculates time of AP and interval."
  TrainTimeAP::usage="TrainTimeAP[APnumber] returns calculated time of AP 'APnumber' in train."
  TrainTimeAPs::usage="TrainTimeAPs[] returns calculated times of all APs in train."
  RecoveryTimeAP::usage="RecoveryTimeAP[APnumber] returns calculated time of recovery pulse 'APnumber'."
  RecoveryTimeAPs::usage="RecoveryTimeAPs[] returns calculated time of all recovery pulses."
  TimeAPs::usage="TimeAPs[] returns time of recovery pulse in recovery."
  GenerateTrain::usage="GenerateTrain[] generates train of APs for CalC."
  GenerateLine::usage="GenerateLine[] generates average line or point for CalC."
  GenerateShell::usage="GenerateShell[] generates shells for CalC."
  GenerateCaEquations::usage"GenerateCaEquations[] generates average line Ca equations for CalC."
  GenerateEquations::usage="GenerateEquations[] generats average line dye equations for CalC. GenerateEquations[] will call GenerateEquations[\"dye\"].\nGenerateEquations[bufferName] generats average line buffer 'bufferName' equations for CalC."
  RunCalC::usage="RunCalC[] runs file \"calc.spl\" for CalC.\nRunCalC[fileNameSuffix] runs file \"calc.fileNameSuffix.spl\" for CalC."
  ImportCalC::usage="ImportCalC[dataName] imports CalC output data 'dataName'."
  Data::usage="Data[dataName] returns imported CalC data 'dataName'."
  ProcessCalC::usage="ProcessCalC[dataName,options] processes imported CalC output data 'dataName' with 'options': 'SetBaseline'->'number' (default: '0')."
  OutFileName::usage="OutFileName[dataName] returns CalCoMatica output file name for exporting data 'dataName' to.\nOutFileName[dataName,parts] returns 'parts' of output file name."
  ExportCalC::usage="ExportCalC[dataName] exports data 'dataName' from CalCoMatica."
  SaveCalC::usage="SaveCalC[dataName] saves CalC data 'dataName' files with sufix \"CalC\" in the file name (cunprocessed CalC output files)."
  PlotCalC::usage="PlotCalC[dataName,options] plots model reults 'dataName' superimposed with measured data (or without depending on set option 'Experiment') with 'options': 'Experiment'->'bool' (default: 'True'); 'PlotX'->'{number,number}' (default: '{0,All}'); 'PlotY'->'{number,number}' (default: '{0,All}'); 'Single'->bool (default: 'False')."
  PlotCalC3D::usage="PlotCalC3D[dataName,options] plots CalC data 'dataName' in 3D with 'options': 'PlotX'->'{number,number}' (default: '{0,All}'); 'PlotY'->'{number,number}' (default: '{0,All}'); 'PlotZ'->'{number,number}' (default: '{0,All}')."
  PrintRunMode::usage="PrintRunMode[] prints the type of current run mode.\nPrintRunMode[runModeNumber] prints the type of number of model run mode 'runModeNumber'."
  CheckRunMode::usage="CheckRunMode[] sets informations about current run mode.\nCheckRunMode[runModeNumber] sets informations about number of model run mode 'runModeNumber'."
  DyeEnd::usage="DyeEnd[] returns last dye number (total number of dyes) for selected mode."
  Plot2D::usage="Plot2D[timePoint] generates plots concentration profile in a 2D plane definitions for CalC at specified 'timePoint'.\nPlot2D[array] generates plots concentration profile in a 2D plane definitions for CalC at specified times 'array'."

  Begin["Global`"]

    thispackagedirectory=$InputFileName//DirectoryName;
    Get["legend.m",Path->thispackagedirectory];
    Get["lukpack.m",Path->thispackagedirectory];

    (* TODO: DEFAULT DIRs *)

    train="% no train";
    customBuffers="";
    cooperativeBuffers="";
    localCa="";
    buffersList={};
    cooperativeBuffersList={};
    CaAverage=LineAverage=LineToAdd=ShellPoint="";
    MaxLineLength=40;
    dyeTotal=dyeKon=dyeKoff=0;
    noDyeName="NO DYE";

    FileNameFractionalDigits=4;
    CalCFractionalDigits=6;

    Adaptive["dtStreach"]=Adaptive["accuracy"]=Adaptive["dtMax"]=Adaptive["steps"]=Adaptive["maxSteps"]="";
	CalC["$PATH"]=True; (* True to use CalC added to system enviroment variable $PATH, False to use CalC from CalCoMatica package directory /core/calc/ *)

    If[Defined["x"]==False,x=0];
    If[Defined["count"]==False,count=0];

    RemoveFloatingPoint=Sequence[NumberFormat->(DisplayForm@RowBox[Join[{StringTrim[#1,RegularExpression["\\.$"]]},If[#3!="",{"\[Times]",SuperscriptBox[#2,#3]},{}]]]&)]; (* from: http://mathematica.stackexchange.com/a/7500 *)

    runMode::Null="\"runMode\" undefined";
    runMode::Unknown="\"runMode\" unknown";

    CalCForm[number_,fracMax_:4]:=(
    (* returns number converted to CalC format *)

      noSci=ToString[NumberForm[RoundFractional[number,CalCFractionalDigits],RemoveFloatingPoint]];
      If[number!=0,
        numberFracTest=number;

        If[numberFracTest<1,
          frac=1;
          While[numberFracTest*10.<1,
            numberFracTest*=10.;
            frac+=1;
            sign="-";
          ];
          numberInt=numberFracTest*10;
        ,
          frac=0;
          While[numberFracTest/10.>=1,
            numberFracTest/=10.;
            frac+=1;
            sign="+";
          ];
          numberInt=numberFracTest;
        ];

        If[frac<fracMax,
          noSci
        ,
          StringJoin[ToString[NumberForm[numberInt,RemoveFloatingPoint]],"e",sign,ToString[frac]]
        ]
      ,
        noSci
      ]
    );

    buffersParamList={{}};
    AddBuffer[bufName_,bufD_,bufKplus_,bufKminus_?NumericQ,bufTotal_,bufBC_]:=(
    (* adds buffer *)

      If[bufTotal!=0,
        buffer={bufName,bufD,bufKplus,bufKminus,bufTotal,bufBC};
        AppendTo[buffersParamList,buffer];
        If[buffersParamList[[1]]=={}, buffersParamList=Delete[buffersParamList,1];];
        For[i=1,i<=Length[buffersParamList]-1,i++,
          If[buffersParamList[[i,1]]==bufName,buffersParamList=Delete[buffersParamList,i];];
        ];
      ];
    );

    cooperativeBuffersParamList={{}};
    AddBuffer[bufName_,bufD_,bufTotal_,bufBC_,Treaction_,Rreaction_,Vsite_:0]:=(
    (* adds cooperative buffer *)

      If[bufTotal!=0,
        bufferCoo={bufName,bufD,bufTotal,bufBC,Treaction,Rreaction,Vsite};
        AppendTo[cooperativeBuffersParamList,bufferCoo];
        If[cooperativeBuffersParamList[[1]]=={}, cooperativeBuffersParamList=Delete[cooperativeBuffersParamList,1];];
        For[i=1,i<=Length[cooperativeBuffersParamList]-1,i++,
          If[cooperativeBuffersParamList[[i,1]]==bufName,cooperativeBuffersParamList=Delete[cooperativeBuffersParamList,i];];
        ];
      ];
    );

    RemoveBuffer[bufName_]:=(
    (* removes buffer or cooperative buffer *)

      If[buffersParamList[[1]]!={},
        For[i=1,i<=Length[buffersParamList],i++,
          If[buffersParamList[[i,1]]==bufName,buffersParamList=Delete[buffersParamList,i];];
          If[Length[buffersParamList]==0,AppendTo[buffersParamList,{}]];
        ];
      ];

      If[cooperativeBuffersParamList[[1]]!={},
        For[i=1,i<=Length[cooperativeBuffersParamList],i++,
          If[cooperativeBuffersParamList[[i,1]]==bufName,cooperativeBuffersParamList=Delete[cooperativeBuffersParamList,i];];
        ];
        If[Length[cooperativeBuffersParamList]==0,AppendTo[cooperativeBuffersParamList,{}]];
      ];
    );

    RemoveBuffers[]:=(
    (* removes all buffers and cooperative buffers *)
      buffersParamList={{}};
      cooperativeBuffersParamList={{}};
    );

    GenerateBuffer[]:=(
    (* generates buffers definitions for Calc *)

      If[buffersParamList[[1]]!={},
        For[i=1,i<=Length[buffersParamList],i+=1,

          bufName=buffersParamList[[i]][[1]];
          bufD=buffersParamList[[i]][[2]];
          bufKplus=buffersParamList[[i]][[3]];
          bufKminus=buffersParamList[[i]][[4]];
          bufTotal=buffersParamList[[i]][[5]];
          bufBC=buffersParamList[[i]][[6]];
          bufKD=N[bufKminus/bufKplus];

          customBuffers=customBuffers<>"buffer "<>ToString[bufName]<>"\n";
          customBuffers=customBuffers<>ToString[bufName]<>".D = "<>ToString[CalCForm[bufD]]<>"\n";
          customBuffers=customBuffers<>ToString[bufName]<>".KD = "<>ToString[CalCForm[bufKD]]<>"\n";
          customBuffers=customBuffers<>ToString[bufName]<>".kminus = "<>ToString[CalCForm[bufKminus]]<>"\n";
          customBuffers=customBuffers<>"%"<>ToString[bufName]<>".kplus = "<>ToString[CalCForm[bufKplus]]<>"\n";
          customBuffers=customBuffers<>ToString[bufName]<>".total = "<>ToString[CalCForm[bufTotal]]<>"\n";
          customBuffers=customBuffers<>ToString[bufName]<>".bc "<>ToString[bufBC]<>"\n\n";

          buffersList=Append[buffersList,bufName];

        ];
      ];
    );

    GenerateCooperativeBuffer[]:=(
    (* generates cooperative buffers [Fass et al., 2007] definitions for Calc *)

      If[cooperativeBuffersParamList[[1]]!={},

          For[i=1,i<=Length[cooperativeBuffersParamList],i+=1,

          bufName=cooperativeBuffersParamList[[i]][[1]];
          bufD=cooperativeBuffersParamList[[i]][[2]];
          bufTotal=cooperativeBuffersParamList[[i]][[3]];
          bufBC=cooperativeBuffersParamList[[i]][[4]];
          Treaction=cooperativeBuffersParamList[[i]][[5]];
          Rreaction=cooperativeBuffersParamList[[i]][[6]];
          Vsite=cooperativeBuffersParamList[[i]][[7]];

          (* ADD: if Treaction and Rreaction \[GreaterEqual]3 and Vsite\[Equal]4 else error *)

          TreactionD=Treaction[[1]];
          TreactionKplus=Treaction[[2]];
          TreactionKminus=Treaction[[3]];
          If[Length[Treaction]==4,TreactionTotal=Treaction[[4]];]; (* for PBS *)
          TreactionKD=N[TreactionKminus/TreactionKplus];

          RreactionD=Rreaction[[1]];
          RreactionKplus=Rreaction[[2]];
          RreactionKminus=Rreaction[[3]];
          If[Length[Rreaction]==4,RreactionTotal=Rreaction[[4]];]; (* for PBS *)
          RreactionKD=N[RreactionKminus/RreactionKplus];

          TRreactionsTotal=bufTotal/.2;

          cooperativeBuffers=cooperativeBuffers<>"buffer cooperative "<>ToString[bufName]<>"\n";
          cooperativeBuffers=cooperativeBuffers<>ToString[bufName]<>".kminus = "<>ToString[CalCForm[TreactionKminus]]<>" % = koff(T)\n";
          cooperativeBuffers=cooperativeBuffers<>"% "<>ToString[bufName]<>".kplus = "<>ToString[CalCForm[2*TreactionKplus]]<>" % => kon(T)="<>ToString[CalCForm[TreactionKplus]]<>" 1/(uM*ms)\n";
          cooperativeBuffers=cooperativeBuffers<>ToString[bufName]<>".KD = "<>ToString[CalCForm[TreactionKD]]<>" % = Kd(T)\n";
          cooperativeBuffers=cooperativeBuffers<>"Ca."<>ToString[bufName]<>".kminus = "<>ToString[CalCForm[2*RreactionKminus]]<>" % => koff(T)="<>ToString[CalCForm[RreactionKminus]]<>" 1/(uM*ms)\n";
          cooperativeBuffers=cooperativeBuffers<>"% Ca."<>ToString[bufName]<>".kplus = "<>ToString[CalCForm[RreactionKplus]]<>" % = kon(R)\n";
          cooperativeBuffers=cooperativeBuffers<>"Ca."<>ToString[bufName]<>".KD = "<>ToString[CalCForm[RreactionKD]]<>" % = Kd(R)\n";
          cooperativeBuffers=cooperativeBuffers<>ToString[bufName]<>".D = "<>ToString[CalCForm[bufD]]<>"\n";
          cooperativeBuffers=cooperativeBuffers<>"Ca."<>ToString[bufName]<>".D = "<>ToString[CalCForm[TreactionD]]<>"\n";
          cooperativeBuffers=cooperativeBuffers<>"Ca2."<>ToString[bufName]<>".D = "<>ToString[CalCForm[RreactionD]]<>"\n";
          cooperativeBuffers=cooperativeBuffers<>ToString[bufName]<>".total = "<>ToString[CalCForm[2*bufTotal]]<>" % = [(BIBII)]total => ["<>ToString[bufName]<>"]total = "<>ToString[CalCForm[bufTotal]]<>" uM\n";
          If[Defined["TreactionTotal"]==True,cooperativeBuffers=cooperativeBuffers<>"Ca."<>ToString[bufName]<>".total = "<>ToString[CalCForm[TreactionTotal]]<>"\n";];
          If[Defined["RreactionTotal"]==True,cooperativeBuffers=cooperativeBuffers<>"Ca2."<>ToString[bufName]<>".total = "<>ToString[CalCForm[RreactionTotal]]<>"\n";];
          cooperativeBuffers=cooperativeBuffers<>ToString[bufName]<>".bc "<>ToString[bufBC]<>"\n";

          cooperativeBuffersList=Append[cooperativeBuffersList,"Ca."<>bufName];
          cooperativeBuffersList=Append[cooperativeBuffersList,"Ca2."<>bufName];
          cooperativeBuffersList=Append[cooperativeBuffersList,bufName];

          noVsite=False;
          If[Vsite==0,noVsite=True;];
          If[noVsite==False,
            VsiteD=Vsite[[1]];
            VsiteKplus=Vsite[[2]];
            VsiteKminus=Vsite[[3]];
            VsiteBC=Vsite[[4]];
            VsiteTotal=bufTotal;
            VsiteKD=N[VsiteKminus/VsiteKplus];

            cooperativeBuffers=cooperativeBuffers<>"buffer "<>ToString[bufName]<>"V\n";
            cooperativeBuffers=cooperativeBuffers<>ToString[bufName]<>"V.D = "<>ToString[CalCForm[VsiteD]]<>"\n";
            cooperativeBuffers=cooperativeBuffers<>ToString[bufName]<>"V.KD = "<>ToString[CalCForm[VsiteKD]]<>" % = Kd(V)\n";
            cooperativeBuffers=cooperativeBuffers<>ToString[bufName]<>"V.kminus = "<>ToString[CalCForm[VsiteKminus]]<>" % = koff(V)\n";
            cooperativeBuffers=cooperativeBuffers<>"% "<>ToString[bufName]<>"V.kplus = "<>ToString[CalCForm[VsiteKplus]]<> " % = kon(V)\n";
            cooperativeBuffers=cooperativeBuffers<>ToString[bufName]<>"V.total = "<>ToString[CalCForm[VsiteTotal]]<>" % = [BV]total\n";
            cooperativeBuffers=cooperativeBuffers<>ToString[bufName]<>"V.bc "<>ToString[VsiteBC]<>"\n";

            cooperativeBuffersList=Append[cooperativeBuffersList,bufName<>"V"];
          ];

        ];
      ];
    );

    vesicleX=vesicleY=vesicleZ=0;
    Vesicle[vpx_,vpy_,vpz_]:=(
    (* sets vesicle position *)

      vesicleX=vpx;
      vesicleY=vpy;
      vesicleZ=vpz;
    );

    localXYZ={};
    Local[cax_,cay_,caz_]:=(
    (* sets loca Ca position *)

      localXYZ=Append[localXYZ,{cax,cay,caz}];
      localXYZ=DeleteDuplicates[localXYZ];

      localX=localXYZ[[All,1]];
      localY=localXYZ[[All,2]];
      localZ=localXYZ[[All,3]];
    );

    GenerateLocal[cax_,cay_,caz_]:=(
    (* generates local Ca to be plotted definitions for Calc *)

      localCa=localCa<>"plot Ca["<>ToString[CalCForm[cax]]<>","<>ToString[CalCForm[cay]]<>","<>ToString[CalCForm[caz]]<>"] % local concentration of Ca2+\n";
      localCa=localCa<>"plot endo["<>ToString[CalCForm[cax]]<>","<>ToString[CalCForm[cay]]<>","<>ToString[CalCForm[caz]]<>"] % local concentration of endo\n";
      For[i=1,i<=Length[buffersList],i+=1,
        localCa=localCa<>"plot "<>ToString[buffersList[[i]]]<>"["<>ToString[CalCForm[cax]]<>","<>ToString[CalCForm[cay]]<>","<>ToString[CalCForm[caz]]<>"] % local concentration of "<>ToString[buffersList[[i]]]<>"\n";
      ];
      For[i=1,i<=Length[cooperativeBuffersList],i+=1,
        localCa=localCa<>"plot "<>ToString[cooperativeBuffersList[[i]]]<>"["<>ToString[CalCForm[cax]]<>","<>ToString[CalCForm[cay]]<>","<>ToString[CalCForm[caz]]<>"] % local concentration of "<>ToString[cooperativeBuffersList[[i]]]<>"\n";
      ];
      If[dye!=0,
        localCa=localCa<>"plot dye["<>ToString[CalCForm[cax]]<>","<>ToString[CalCForm[cay]]<>","<>ToString[CalCForm[caz]]<>"] % local concentration of "<>ToString[dyeParams[[dye,5]]]<>"\n";
      ];
    );

    LoadParams[filenamesuffix_:""]:=(
    (* loads notebook with params *)

      fileNameParams="params"<>If[filenamesuffix!="","."<>ToString[filenamesuffix],""];

      NotebookEvaluate[FileNameJoin[{dir,dirData,fileNameParams<>".nb"}]]; (* load notebook file with parameters *)
    );

    LoadPlugin[pluginName_:""]:=(
    (* loads notebook with plugin *)

      plugin=pluginName;
      workingNotebook=NotebookFileNameOnly[];

      NotebookEvaluate[FileNameJoin[{dirPlugins,pluginName<>".nb"}]]; (* load notebook file with plugin *)
    );
 
    LoadDye[dye_:dye]:=(
    (* loads dye params *)

      LoadDye::dyeNull="\"dye\" undefined";

      If[Defined["dye"]==False,Message[LoadDye::dyeNull];,
        If[dye!=0,
          If[dyeTotal==0,dyeTotal=dyeParams[[dye,1]]]; (* uM, total concentration *)
          If[dyeKon==0,dyeKon=dyeParams[[dye,2]]]; (* 1/(ms*uM), binding rate *)
          If[dyeKoff==0,dyeKoff=dyeParams[[dye,3]]]; (* 1/ms, unbinding rate *)
        ,
          dyeTotal=dyeKon=dyeKoff=1;
        ];
        dyeKd=dyeKoff/dyeKon; (* uM, affinity *)
      ];
    );
 
    PrintDye[dye_:dye]:=(
    (* prints dye params *)

      If[dye!=0,
        Print[dyeParams[[dye,5]]]; (* display dye name *)
        Print["dyeTotal = ", dyeParams[[dye,1]], " [uM]"]; (* display dye total concentration *)
        Print["dyeKon = ", dyeParams[[dye,2]], " [1/(ms*uM)]"]; (* display dye binding rate *)
        Print["dyeKoff = ", dyeParams[[dye,3]], " [1/ms]"]; (* display dye unbinding rate *)
      ,
        Print[noDyeName];
      ];
    );

    PrintDyeName[dye_:dye]:=(
    (* prints dye name *)

      If[dye!=0,
        Print[dyeParams[[dye,4]]]; (* display dye name [4 for dye name and concentration, 5 for only dye name] *)
      ,
        Print[noDyeName];
      ];
    );

    DyeName[dye_:dye]:=(
    (* returns dye name *)

      If[dye!=0,
        dyeName=dyeParams[[dye,4]]; (* dye name to be returned [4 for dye name and concentration, 5 for only dye name] *)
      ,
        dyeName=noDyeName; (* dye name to be returned *)
      ];

      dyeName
    );

    TrainTimeAndInterval[]:=(
	(* calculates time of AP and interval *)

      time=RoundFractional[time,CalCFractionalDigits]; (* time length of AP (hack) *)
      interval=RoundFractional[(number/((frequency/10^3)*number))-time,CalCFractionalDigits]; (* time between APs *)
    );

    TrainTimeAP[APnr_]:=(
	(* returns calculated time of AP in train *)

      TrainTimeAndInterval[];
      RoundFractional[startTrain+((APnr-1)*time)+((APnr-1)*interval),CalCFractionalDigits]
    );

    TrainTimeAPs[]:=(
	(* returns calculated times of all APs in train *)

      return={};
      For[AP=1,AP<=number,AP+=1,
        AppendTo[return,TrainTimeAP[AP]];
      ];
      return
    );

    RecoveryTimeAP[APnr_]:=(
	(* returns calculated time of recovery pulse *)

      TrainTimeAP[number]+interval+time*(Length[recovery[[1;;APnr]]])+Total[recovery[[1;;APnr]]]
    );

    RecoveryTimeAPs[]:=(
	(* returns calculated time of all recovery pulses *)

      return={};
      For[AP=1,AP<=Length[recovery]-1,AP+=1,
        AppendTo[return,RecoveryTimeAP[AP]];
      ];
      return
    );

    TimeAPs[]:=(
	(* returns time of recovery pulse in recovery *)

      Join[TrainTimeAPs[],RecoveryTimeAPs[]]
    );
 
    GenerateTrain[]:=(
    (* generates train of APs for CalC *)

      TrainTimeAndInterval[];

      GenerateTrain::change = "'change' length \"`1`\" is smaller than 'number' of APs \"`2`\". 'change' not used...";

      train="";

      If[Length[change]==0,
        change=Table[change,{ch,0,number}];
      ];

      If[Length[change]<number,
        Message[GenerateTrain::change,Length[change],number];
        change=Table[1,{ch,0,number}];
      ];

      If[startTrain!=0,
        train=train<>"Run adaptive "<>ToString[CalCForm[startTrain]]<>" ; current = 0\n ";
      ];

      If[Defined["FWHM"],
        (* gaussian shape of current in time *)
        sigma=2*((FWHM/2.35482)^2);
        traintime=TrainTimeAP[number]-startTrain+interval+time;
        train=train<>"gaussiantrain := ";
        For[AP=1,AP<=number,AP+=1,
          train=train<>ToString[CalCForm[current*change[[AP]]]]<>" exp(-((t-"<>ToString[CalCForm[TrainTimeAP[AP]+time]]<>")^2)/"<>ToString[CalCForm[sigma]]<>")";
          train=train<>If[AP<number," + ...\n ","\n "];
        ];
        train=train<>"Run adaptive "<>ToString[CalCForm[traintime]]<>" ; current = gaussiantrain pA";

        If[Length[recovery]!=0,
          (* recovery *)
		  (* TODO: RECOVERY *) train=train;
        ,
          (* no recovery *)
          If[recovery!=0,
            train=train<>"\n ";
            train=train<>"Run adaptive "<>ToString[CalCForm[recovery]]<>" ; current = 0"
          ];
        ];

      ,

        (* rectangular shape of current in time *)
        For[AP=1,AP<=number,AP+=1,
          train=train<>"Run adaptive t.AP ; current = "<>ToString[CalCForm[current*change[[AP]]]]<>" pA\n Run adaptive "<>ToString[CalCForm[interval]]<>" ; current = 0\n ";
        ];

        If[Length[recovery]!=0,
          (* recovery *)
          train=train<>"\n ";
          For[rec=1,rec<=Length[recovery],rec+=1,
            If[recovery[[rec]]!=0,
              train=train<>"Run adaptive "<>ToString[CalCForm[recovery[[rec]]]]<>" ; current = 0"<>If[rec!=Length[recovery],"\n Run adaptive t.AP ; current = I.AP pA\n ",""];
            ];
          ];
        ,
          (* no recovery *)
          If[recovery!=0,
            train=train<>"\n ";
            train=train<>"Run adaptive "<>ToString[CalCForm[recovery]]<>" ; current = 0"
          ];
        ];
      ];
    );

    GenerateLine[]:=(
    (* generates average line or point for CalC *)

      If[Defined["pR"]==True,lR1=lR2=pR];
      If[Defined["lR1"]==False,lR1=0];
      If[Defined["lR2"]==False,
        If[lZ>Z,lR2=N[diaAx/2]];
        If[lZ<=Z,lR2=N[dia/2]];
      ];

      If[lR1!=lR2,
        lRtab=Table[RoundFractional[((lR2-lR1)*li/(points-1))+lR1,4],{li,0,points-1}]; (* points on r-axis *)
      ,
        lRtab={pR};
        points=1;
      ];

      lR=ratio=Cadye=Cadyeaverage=Caaverage=Ratioaverage="";
      breakline=1;
      For[ex=1,ex<=points,ex+=1,
        lR=lR<>ToString[CalCForm[lRtab[[ex]]]];
        If[ex<points,
          lR=lR<>" ";
        ];
        If[breakline==4&&ex<points,
          lR=lR<>"... \n";
          breakline=0;
        ];
        breakline+=1;
      ];

      GenerateCaEquations[];
      If[(dye!=0&&dyeTotal!=0),GenerateEquations[]]; (* adding equations for dye if exists *)
      For[i=1,i<=Length[buffersList],i+=1,GenerateEquations[buffersList[[i]]]]; (* adding equations for all defined buffers *)
    );

    GenerateShell[]:=(
    (* generates shells for CalC *)

      If[sZ>Z,rad=N[diaAx/2]];
      If[sZ<=Z,rad=N[dia/2]];

      If[Defined["sP"]==false,sP=0];

      sR=((shell-1)*(rad/nR))+(rad/nR)*sP;

      sRatio="Shell.Ratio := (dye.total - dye["<>ToString[CalCForm[sR]]<>","<>ToString[CalCForm[sZ]]<>"]) / dye.total\n";
      sDye="Shell.dye := dye.KD Shell.Ratio / (1 - Shell.Ratio)\n";

      ShellPoint=sRatio<>"\n"<>sDye<>"\nShell:=Ca["<>ToString[CalCForm[sR]]<>","<>ToString[CalCForm[sZ]]<>"]\nplot Shell\nplot Shell.dye";
    );

    GenerateCaEquations[]:=(
    (* generates average line Ca equations for CalC *)
      
      EqName="Ca.average := ";
      LineLength=StringLength[EqName];
      For[ex=1,ex<=points,ex+=1,
        LineToAdd="Ca[l.r{"<>ToString[ex]<>"},l.z]";
        CaAverage=CaAverage<>LineToAdd;

        LineLength=LineLength+StringLength[LineToAdd];
        If[LineLength<=MaxLineLength,
          If[ex<points,CaAverage=CaAverage<>" + "];
        ,
          If[ex<points,CaAverage=CaAverage<>" + ...\n"];
          LineLength=0;
        ];

      ];
      CaAverage="("<>CaAverage<>") / "<>ToString[points];
      CaAverage=EqName<>CaAverage;
      PlotCaAverage="plot Ca.average";
      LineAverage=LineAverage<>CaAverage<>"\n"<>PlotCaAverage<>"\n";
      CaAverage="";
    );

    GenerateEquations[type_:"dye"]:=(
    (* generats average line dye (default) or buffer equations for CalC *)

      RatioBuf=CaBuf=CaBufAverage=RatioBufAverage=PlotBufAverage="";

      RatioEqName="Ratio."<>ToString[type]<>".average := ";
      CaEqName="Ca."<>ToString[type]<>".average := ";
      RatioLineLength=StringLength[RatioEqName];
      CaLineLength=StringLength[CaEqName];

      For[ex=1,ex<=points,ex+=1,
        RatioBuf=RatioBuf<>"Ratio."<>ToString[type]<>"."<>ToString[ex]<>" := ("<>ToString[type]<>".total - "<>ToString[type]<>"[l.r{"<>ToString[ex]<>"},l.z]) / "<>ToString[type]<>".total\n";
        CaBuf=CaBuf<>"Ca."<>ToString[type]<>"."<>ToString[ex]<>" := "<>ToString[type]<>".KD Ratio."<>ToString[type]<>"."<>ToString[ex]<>" / (1 - Ratio."<>ToString[type]<>"."<>ToString[ex]<>")\n";
        CaLineToAdd="Ca."<>ToString[type]<>"."<>ToString[ex];
        CaBufAverage=CaBufAverage<>CaLineToAdd;
        RatioLineToAdd="Ratio."<>ToString[type]<>"."<>ToString[ex];
        RatioBufAverage=RatioBufAverage<>RatioLineToAdd;

        RatioLineLength=RatioLineLength+StringLength[RatioLineToAdd];
        If[RatioLineLength<=MaxLineLength,
          If[ex<points,RatioBufAverage=RatioBufAverage<>" + "];
        ,
          If[ex<points,RatioBufAverage=RatioBufAverage<>" + ...\n"];
          RatioLineLength=0;
        ];

        CaLineLength=CaLineLength+StringLength[CaLineToAdd];
        If[CaLineLength<=MaxLineLength,
          If[ex<points,CaBufAverage=CaBufAverage<>" + "];
        ,
          If[ex<points,CaBufAverage=CaBufAverage<>" + ...\n"];
          CaLineLength=0;
        ];

      ];
      RatioBufAverage="("<>RatioBufAverage<>") / "<>ToString[points];
      RatioBufAverage=RatioEqName<>RatioBufAverage;
      CaBufAverage="("<>CaBufAverage<>") / "<>ToString[points];
      CaBufAverage=CaEqName<>CaBufAverage;
      PlotRatioBufAverage="plot Ratio."<>ToString[type]<>".average";
      PlotCaBufAverage="plot Ca."<>ToString[type]<>".average";
      PlotBufAverage=PlotBufAverage<>PlotRatioBufAverage<>"\n"<>PlotCaBufAverage<>"\n";
      LineAverage=LineAverage<>"\n"<>RatioBuf<>"\n"<>CaBuf<>"\n"<>RatioBufAverage<>"\n\n"<>CaBufAverage<>"\n\n"<>PlotBufAverage;
      LineAverage=="";
    );

    RunCalC[calcFilesSuffix_:""]:=(
    (* runs CalC *)

      (* check if calc.*.spl file exists *)
      calcSpliceFile="calc"<>If[calcFilesSuffix!="","."<>ToString[calcFilesSuffix],""]<>".spl";
      calcSpliceFileWithPath=FileNameJoin[{dirCore,calcSpliceFile}];
      RunCalC::run="File \"`1`\" does not exist... Quit.";
      If[FileExistsQ[calcSpliceFileWithPath]==False,
       Message[RunCalC::run,calcSpliceFile];
       Quit[]; (* stop if file does not exist *)
      ];

      calcRunFile="calc"<>If[calcFilesSuffix!="","."<>ToString[calcFilesSuffix],""]<>".par";
      calcRunFileWithPath=FileNameJoin[{dirData,"tmp",calcRunFile}];

      (* remove tmp directory *)
      If[DirectoryQ[FileNameJoin[{dirData,"tmp"}]]==True,
       tmpFiles=FileNames["*",FileNameJoin[{dirData,"tmp"}]];
       DeleteFile[tmpFiles];
      ,
       CreateDirectory[FileNameJoin[{dirData,"tmp"}]];
      ];

      GenerateBuffer[];
      GenerateCooperativeBuffer[];

      For[lc=1,lc<=Length[localX],lc+=1,
        GenerateLocal[localX[[lc]],localY[[lc]],localZ[[lc]]];
      ];

      If[Defined["runMode"]==False,
        Message[runMode::Null];Exit[];
      ,
        If[runMode!=(1||2||3||4),Message[runMode::Unknown];Exit[];];
      ];

      If[Defined["number"]==True&&Defined["frequency"]==True&&Defined["time"]==True,GenerateTrain[];];

      If[Defined["dye"]==True,
        LoadDye[];
      ,
        dye=0;
        LoadDye[];
      ];

      (* TODO: set optional parameters to default values if not defined *)

      If[Defined["points"]==True,GenerateLine[];];

      If[Defined["shell"]==True,GenerateShell[];];

      asAdded=False;
      adaptiveSettings="% Adaptive settings";
      If[Adaptive["dtStreach"]!="",adaptiveSettings=adaptiveSettings<>"\n adaptive.dtStreatch="<>ToString[Adaptive["dtStreach"]];asAdded=True;];
      If[Adaptive["accuracy"]!="",adaptiveSettings=adaptiveSettings<>"\n adaptive.accuracy="<>ToString[Adaptive["accuracy"]];asAdded=True;];
      If[Adaptive["dtMax"]!="",adaptiveSettings=adaptiveSettings<>"\n adaptive.dtMax="<>ToString[Adaptive["dtMax"]];asAdded=True;];
      If[Adaptive["steps"]!="",adaptiveSettings=adaptiveSettings<>"\n adaptive.steps="<>ToString[Adaptive["steps"]];asAdded=True;];
      If[Adaptive["maxSteps"]!="",adaptiveSettings=adaptiveSettings<>"\n adaptive.maxSteps="<>ToString[Adaptive["maxSteps"]];asAdded=True;];
      If[asAdded==False,adaptiveSettings=adaptiveSettings<>"\n% CalC default"];
      adaptiveSettings=adaptiveSettings<>"\n% ---";

      (* calculate endo params *)
      endoKd=endoKoff/endoKon; (* uM, affinity *)
      endoTotal=endoKappa*endoKd; (* uM, total concentration *)

      (* convert params to CalC notation form *)
      endoKd=CalCForm[endoKd];
      endoTotal=CalCForm[endoTotal];

      (* splice file *)
      Splice[calcSpliceFileWithPath, calcRunFileWithPath, FormatType -> OutputForm,PageWidth->256];
      currentDirectory=Directory[];
      SetDirectory[FileNameJoin[{dirData,"tmp"}]];

      (* run CalC from shell *)
      Module[{shell},
        Switch[$OperatingSystem,
          "Windows",
            Needs["NETLink`"];
            shell = NETLink`CreateCOMObject["WScript.shell"];
			If[CalC["$PATH"],shell @ Run[StringJoin["calcwin ",ToString[calcRunFile]],0,True],shell@Run[StringJoin["\"",FileNameJoin[{ToString[currentDirectory], "core", "calc", CalC["version"]<>".exe"}],"\" ",ToString[calcRunFile]],0,True]],
          "Unix"|"MacOSX",
			If[CalC["$PATH"],Run @ StringJoin["calc ",ToString[calcRunFile]],shell@Run[StringJoin["'",FileNameJoin[{ToString[currentDirectory], "core", "calc", CalC["version"]}],"' ",ToString[calcRunFile]],0,True]]
         ]
      ];

      SetDirectory[currentDirectory];

      customBuffers="";
      cooperativeBuffers="";
      localCa="";
      buffersList={};
      cooperativeBuffersList={};
      CaAverage=LineAverage=LineToAdd=ShellPoint="";
      dyeTotal=dyeKon=dyeKoff=0;
    );

    ImportCalC[dataname_,id_:"tmp"]:=(
    (* imports CalC output data *)

      ImportCalC::run="CalC returned error... check file \"`1`\" in error directory";

      tmpFileWithPath=FileNameJoin[{dirData,"tmp",dataname}];

      If[FileExistsQ[tmpFileWithPath],
        tmp=Import[tmpFileWithPath,"Table"];
        tmp=DeleteDuplicates[tmp]; (* remove the same elements because Mathematica sometimes doesn't like it *)
        tmp=DeleteCases[tmp,{a_}] (* remove nonpairs elements *)
      , 
        tmp={{0,0}};
        getTime=DateList[];
        ID=StringJoin[Table[If[getTime[[di]]>10,ToString[getTime[[di]]],"0"<>ToString[getTime[[di]]]],{di,1,6}]];
        calcErrorFile=ID<>"."<>calcRunFile;
        CopyFile[calcRunFileWithPath,FileNameJoin[{dirData,"error",calcErrorFile}]];
        Message[ImportCalC::run,calcErrorFile];
      ];
      If[count==0,count=1];
      Data[id][dataname]=data[dataname][count]=Table[{tmp[[di,1]],tmp[[di,2]]},{di,1,Length[tmp]}]; (* no baseline change *)
      xname[dataname][count]=x; (* for plot legend *)
    );

    Data[dataname_]:=(
    (* returns imported CalC data *)

      data[dataname][count]
    );

    Options[ProcessCalC] = {
      SetBaseline->0
    };

    ProcessCalC[dataname_,opts:OptionsPattern[]]:=(
    (* processes imported CalC output data *)

      If[count==0,count=1];
      baselinePosition=tmp[[1,2]]-OptionValue[SetBaseline];
      data[dataname][count]=Table[{tmp[[di,1]],Subtract[tmp[[di,2]],baselinePosition]},{di,1,Length[tmp]}]; (* change baseline to defined value *)
    );

    OutFileName[dataname_]:=(
    (* returns output file name *)

      CheckRunMode[];
      "["<>NotebookFileNameOnly[]<>"] "<>runModeNameShort<>" "<>ToString[dataname]<>If[dye!=0," "<>dyeParams[[dye,4]],""]<>If[x!=0,If[Defined["xName"]==False," x="," "<>ToString[xName]<>"="]<>If[FileNameFractionalDigits!=0,ToString[RoundFractional[x,FileNameFractionalDigits]],ToString[IntegerPart[x]]],""]
    );

    OutFileName[dataname_,parts_]:=(
    (* returns output file name reduced to some parts *)

      StringJoin[Riffle[StringSplit[OutFileName[dataname]," "][[1;;parts]]," "]]
    );

    ExportCalC[dataname_]:=(
    (* exports data from CalCoMatica *)

      CheckRunMode[];

      (*fileNameExport="["<>NotebookFileNameOnly[]<>"] "<>runModeNameShort<>" "<>ToString[dataname]<>If[dye!=0," "<>dyeParams[[dye,4]],""]<>If[x!=0,If[Defined["xName"]==False," x="," "<>ToString[xName]<>"="]<>If[FileNameFractionalDigits!=0,ToString[RoundFractional[x,FileNameFractionalDigits]],ToString[IntegerPart[x]]],""]; (* generates file name *)*)
      fileNameExport=OutFileName[dataname]; (* generates file name *)
      Export[FileNameJoin[{dirData,"out",fileNameExport<>".txt"}],data[dataname][count],"Table"]; (* save to file *)
    );

    SaveCalC[dataname_]:=(
    (* saves CalC data files (copy of CalC output files from tmp directory) *)

      CheckRunMode[];

      (*fileNameSave="["<>NotebookFileNameOnly[]<>"] "<>runModeNameShort<>" "<>ToString[dataname]<>If[dye!=0," "<>dyeParams[[dye,4]],""]<>If[x!=0,If[Defined["xName"]==False," x="," "<>ToString[xName]<>"="]<>If[FileNameFractionalDigits!=0,ToString[RoundFractional[x,FileNameFractionalDigits]],ToString[IntegerPart[x]]],""]; (* generates file name *)*)
      fileNameSave=OutFileName[dataname]; (* generates file name *)
      CopyFileOverwirte[FileNameJoin[{dirData,"tmp",dataname}],FileNameJoin[{dirData,"out",fileNameSave<>".CalC.txt"}]]; (* save to file *)
    );

    Options[PlotCalC] = {
      Experiment->True,
      PlotX->{0,All},
      PlotY->{0,All},
      Single->False
    };
 
    PlotCalC[dataname_,opts:OptionsPattern[]]:=(
    (* plots model reults superimposed with measured data (or without - see Options) *)

      plots={};
      xnames={};

      If[OptionValue[Experiment]==True&&dye!=0,
        tmp=measuredData[[runMode,dye]];

        (* change start time to 0 *)
        If[Length[tmp[[1]]]==2,
          (* single experiment *)
          tmp[[All,1]]-=tmp[[All,1]][[1]];
        ,
          (* multiple experiments *)
          subtract=Min[Table[tmp[[i]][[All,1]][[1]],{i,Length[tmp]}]];
          Do[tmp[[i]][[All,1]]-=subtract,{i,Length[tmp]}];
        ];

        plot0=ListPlot[tmp,PlotRange->{OptionValue[PlotX],OptionValue[PlotY]}];
        plots=Append[plots,plot0];
      ];

      If[count==0,count=1];


      If[OptionValue[Single]==True||count==1,
        tmp=data[dataname][count];
        tmpPlot=ListLinePlot[tmp,PlotRange->{OptionValue[PlotX],OptionValue[PlotY]},PlotStyle->ColorData[1][count]];
        plots=Append[plots,tmpPlot];
        Print[Panel[Show[{plots},PlotRange->{OptionValue[PlotX],OptionValue[PlotY]},PlotLabel->dataname,ImageSize->400]]];
      ,
        For[plot=1,plot<=count,plot+=1,
          tmp=data[dataname][plot];
          tmpPlot=ListLinePlot[tmp,PlotRange->{OptionValue[PlotX],OptionValue[PlotY]},PlotStyle->ColorData[1][plot]];
          plots=Append[plots,tmpPlot];
          xnames=Append[xnames,xname[dataname][plot]];
        ];
        legend=legendMaker[xnames,Background->LightOrange, RoundingRadius->5];
        Print[Panel[Show[{plots},PlotRange->{OptionValue[PlotX],OptionValue[PlotY]},PlotLabel->dataname,ImageSize->400],{legend},{Right}]];
      ];
    );

    Options[PlotCalC3D] = {
      PlotX->{0,All},
      PlotY->{0,All},
      PlotZ->{0,All}
    };

    PlotCalC3D[dataname_,opts:OptionsPattern[]]:=(
	(* plots CalC data in 3D *)

      tmp=Import[FileNameJoin[{dirData,"tmp",dataname}],"Table"];
      tmp=DeleteCases[tmp,{}]; (* delete empty lines *)
      Print[Show[ListPlot3D[tmp,PlotRange->{OptionValue[PlotX],OptionValue[PlotY],OptionValue[PlotZ]}]]];
    );

    PrintRunMode[runMode_:runMode]:=(
    (* prints the type of model run mode *)

      CheckRunMode[runMode];

      Print[runModeName];
    );

    CheckRunMode[runMode_:runMode]:=(
	(* sets informations about run mode *)

      Switch[runMode,
        1,
         runModeName="Single AP";
         runModeNameShort="AP",
        2,
          runModeName="Train ["<>ToString[number]<>"APs @ "<>ToString[frequency]<>"Hz]";
          runModeNameShort=ToString[frequency]<>"Hz",
        3,
          runModeName="Depolarization ["<>ToString[timeDepo]<>"ms]";
          runModeNameShort=ToString[timeDepo]<>"ms",
        4,
          runModeName="Double pulses ["<>ToString[DoublePulse["AP"]]<>"ms, "<>ToString[DoublePulse["interval"]]<>"ms]";
          runModeNameShort="DP"<>ToString[DoublePulse["AP"]]<>"ms_"<>ToString[DoublePulse["interval"]]<>"ms",
        _,
          runModeName="\"runMode\" unknown"
      ];
    );

    DyeEnd[]:=(
	(* returns last dye number *)

    (* TODO: NEEDS AUTOMATIZATION !!! *)
      Switch[runMode,
        1,
          ret=Length[dyeParams],
        2,
          ret=Length[dyeParams]-2,
        3,
          ret=Length[dyeParams]-2,
        4,
          ret=Length[dyeParams],
        _,
          ret=0
      ];

      ret
    );

    plot2D="";
    Plot2D[time_]:=(
	(* generates plots concentration profile in a 2D plane definitions for CalC *)

      time2D=time;
      If[Length[time]==0,
        plot2D=StringJoin["plot 2D.mute Ca z 0 ",ToString[CalCForm[time]]," \" Ca[x,y,0]\""];
      ,
        For[i=1,i<=Length[time],i+=1,
          plot2D=StringJoin[plot2D,"plot 2D.mute Ca z 0 ",ToString[CalCForm[time[[i]]]]," \" Ca[x,y,0]\""];
          If[i!=Length[time],plot2D=plot2D<>"\n"];
        ];
      ];
    );

  End[]

EndPackage[]
