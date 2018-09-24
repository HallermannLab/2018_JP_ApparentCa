(* ::Package:: *)

BeginPackage["LukPack`"]
(* LukPack *)
(* version 1.25.3 *)
(* by Lukasz Jablonski *)

(* Licensed under the MIT License (MIT) *)
(* https://doi.org/10.5281/zenodo.1313587 *)
(* https://github.com/lukaszjablonski/LukPack *)

  ExportXLS::usage="ExportXLS[fileName,dataToExport,sheetName] exports 'dataToExport' to a sheet named 'sheetName' in a new XLS file or append if file 'fileName' exists."
  Defined::usage="Defined[variableName] returns True if 'variableName' exists, and False otherwise."
  NotebookFileNameOnly::usage="NotebookFileNameOnly[] returns only file name and extension of current notebook."
  FileNamesOnly::usage="FileNamesOnly[] returns a list of all files with names and extensions in the current working directory. Alias of FileNames[].\nFileNamesOnly[directory] returns a list of all files with names and extensions in 'directory'.\nFileNamesOnly[pattern] returns a list of all files with names and extensions in the current working directory whose names match 'pattern' (for details on patterns see FileNames[]).\nFileNamesOnly[pattern,directory] returns a list of files with file names and extensions in 'directory' or directories ('directory'={'dir1','dir2',...}) whose names match 'pattern' (for details on patterns see FileNames[])."
  CopyFileOverwirte::usage="CopyFileOverwirte[sourceFile,destinationFile] makes  copy of 'sourceFile' to 'destinationFile'. If file exists, it will be overwritten."
  FractionalLength::usage="FractionalLength[decimalNumber] returns number of digits in the fractional part of 'decimalNumber'."
  RoundFractional::usage="RoundFractional[decimalNumber,numberOfDigitsInFractionalPart] rounds 'decimalNumber' to defined 'numberOfDigitsInFractionalPart'."
  ToScientific::usage="ToScientific[number,numberOfDigitsInIntegerPart] returns scientific notation of 'number' as a string with defined 'numberOfDigitsInIntegerPart'."
  MaxBetween::usage="MaxBetween[xyData,xStart,xEnd] returns '{x,y}' of the two-dimensional array 'xyData' where 'y' is maximal between 'xStart' and 'xEnd'."
  GetX::usage="GetX[xyData,y] returns a value of 'x' at given 'y' from array 'xyData'."
  GetY::usage="GetY[xyData,x] returns a value of 'y' at given 'x' form array 'xyData'.\nGetY[xyData,xList] returns a list of 'y' values at given 'xList' of x values form array 'xyData'."
  MakeCopy::usage="MakeCopy[variableName] makes copy of 'variableName' or list of variables e.g. ('variableName'={'variableName1','variableName2',...}). The copied value is saved into Copy[variableName]."
  Copy::usage="Copy[variableName] returns the value of 'variableName' copied with MakeCopy[variableName]."
  ChiSquare::usage="ChiSquare[modelList,experimentList] returns ChiSquare value between lists of values 'modelList' and 'experimentList' calculated according to Pearson's chi-squared test (ChiSquare=Total[(mod-exp)^2/exp])."
  PearsonTest::usage="PearsonTest[modelList,experimentList] returns 'ChiSquare and 'p-val' as results of performed Pearson's chi-squared test between lists of values 'modelList' and 'experimentList'."
  ChiSquareSH::usage="ChiSquare[modelList,experimentList] returns chi square value between lists of values 'modelList' and 'experimentList' calculated according to SH (ChiSquare=Total[(mod-exp)^2]/Length[mod])."
  RangeN::usage="RangeN[min,max,n] returns list of defined 'n' number of values in the between defined borders 'min' and 'max'."
  GetFWHM::usage="GetFWHM[xyData] returns full width at half maximum (FWHM) of 'xyData'."
  SetDuration::usage="SetDuration[\"Start\"] sets start time point to calculate duration.\nSetDuration[\"End\"] sets end time point to calculate duration.\nSetDuration[] will call SetDuration[\"Start\"]."
  StartDuration::usage="StartDuration[] sets start time point to calculate duration. Alias of SetDuration[\"Start\"]."
  EndDuration::usage="EndDuration[] sets end time point to calculate duration. Alias of SetDuration[\"End\"]."
  PrintDuration::usage="PrintDuration[] prints duration. If EndDuration[] or SetDuration[\"End\"] was not set, actual time (AbsoluteTime[]) will be used as end time. If StartDuration[] or SetDuration[\"Start\"] was not set, the time when package was loaded will be used as start time."

  Begin["`Private`"]

    ExportXLS[file_,datatoexport_,sheetname_]:=(
    (* exports data to XLS file (to a new one or append to existing), based on [http://mathematica.stackexchange.com/a/36902] *)
      If[FileExistsQ[file]==False,
        Export[file, sheetname -> datatoexport, "XLS"];
      ,
        xlsdata = Import[file];
        xlssheets = Import[file, "Sheets"];
        xlsolddata = xlssheets[[#]] -> xlsdata[[#]] & /@ Range[Length[xlssheets]];
        xlsnewdata = Append[xlsolddata, sheetname -> datatoexport];
        Export[file, xlsnewdata, "XLS"];
      ];
    );

    Defined[var_]:=(
    (* checks if variable exists or not, based on [http://stackoverflow.com/a/1446512] *)
      varname=ToString[HoldForm[var]];
      ToExpression["ValueQ["<>varname<>"]"]||
      Head@ToExpression[varname]=!=Symbol||
      ToExpression["Attributes["<>varname<>"]"]=!={}||
      ToExpression["DownValues["<>varname<>"]"]=!={}||
      ToExpression["SubValues["<>varname<>"]"]=!={}
    );

    NotebookFileNameOnly[]:=(
    (* returns only file name and extension of current notebook *)
      Take[FileNameSplit[NotebookFileName[]],-1][[1]]
    );

    FileNamesOnly[]:=(
	(* returns nly list with file names and extension of files in current directory*)
      FileNames[]
    );

    FileNamesOnly[dir_?DirectoryQ(*:Directory[]*)]:=(
    (* returns only list with file names and extension of files in directory/directories *)
      StringReplace[FileNames["*",dir],StringDrop[ToString[FileNameJoin[{dir," "}]],-1]->""]
    );

    FileNamesOnly[namepattern_,dir_:Directory[]]:=(
    (* returns only list with file names and extension matching pattern of files in directory/directories *)
      StringReplace[FileNames[namepattern,dir],StringDrop[ToString[FileNameJoin[{dir," "}]],-1]->""]
    );

    CopyFileOverwirte[src_,dst_]:=(
    (* copies file and overwrite if exists *)
      If[src!=dst,
        If[FileExistsQ[dst]==True,DeleteFile[dst]];
        CopyFile[src,dst];
      ];
    );

    FractionalLength[val_]:=(
    (* returns number of digits in fractional part *)
      StringLength[ToString[FractionalPart[val]]]-2 (* number of digits after decimal point *)
    );

    RoundFractional[val_,fraDig_:0]:=(
    (* returns rounded value to specified number of digits after decimal point *)
      add=0;
      fractionalpart=FractionalPart[val];
      If[N[IntegerPart[FractionalPart[fractionalpart*(10^fraDig)]*10]]<5,add=0,add=1];
      integerpart=IntegerPart[val];
      N[integerpart+(IntegerPart[N[fractionalpart*(10^fraDig)+add]]/(10^fraDig))]
    );
	
	ToScientific[valin_,pwrDig_]:=(
    (* returns scientific notation of value as a string (e.g. "2.1e3" ) *)
      val=If[Precision[valin]==\[Infinity],valin*1.,valin];
      nrIntDig=Length[IntegerDigits[IntegerPart[val]]];
      ei=RoundFractional[FractionalPart[N[val/10^nrIntDig,Infinity]],pwrDig];
      ef=FractionalPart[N[val/10^nrIntDig,Infinity]];
      g=ToString[FullForm[#,NumberMarks->False]]&;
      ToString[ei*10^pwrDig]<>StringTrim[g[ef],g[ei]]<>"e"<>ToString[nrIntDig-pwrDig]
    );

    MaxBetween[xyData_,xStart_,xEnd_]:=(
    (* returns maximum {x,y} between xStart and xEnd in xyData *)
     xData=xyData[[All,1]];
     yData=xyData[[All,2]];
     startPosition=Position[xData,Nearest[xData,xStart][[1]]][[1,1]];
     endPosition=Position[xData,Nearest[xData,xEnd][[1]]][[1,1]];
     yDataRange=yData[[startPosition;;endPosition]];
     yMax=Max[yDataRange];
     maxPositionRange=Flatten[Position[yDataRange,yMax]];
     xyDataRange=xyData[[startPosition;;endPosition]];
     xyMax=xyDataRange[[maxPositionRange]];
     If[Length[xyMax]>1,xyMax,Flatten[xyMax]]
    );

    GetX[where_,y_]:=(
    (* returns a value of x at given y *)
      whereT=Transpose[{where[[All,2]],where[[All,1]]}];
      GetY[whereT,y] (* return x *)
    );

    GetY[where_,x_]:=(
    (* returns a value of y at given x *)
      Off[InterpolatingFunction::dmval]; (* kill error messages generated by Interpolation *)
      nrstX=where[[All,1]];
      nrstTest=Nearest[nrstX,x,Length[nrstX]];

      return={};

      For[i=1,i<=Length[Position[nrstX,nrstTest[[1]]]],i++,
        nrstPos=Position[nrstX,If[nrstTest[[1]]>=x,nrstTest[[1]],nrstTest[[2]]]][[i]][[1]];

        before=If[nrstPos!=1,1,0];
        after=If[nrstPos!=Length[nrstX],1,0];

        AppendTo[return,Interpolation[where[[nrstPos-before;;nrstPos+after]],x,InterpolationOrder->1]] (* return y *)
      ];

      If[Length[return]==1,return[[1]],return]
    );

    GetY[where_,x_List]:=(
    (* returns a list of y values at given x values *)
      returnList={};
      For[j=1,j<=Length[x],j++,
        AppendTo[returnList,GetY[where,x[[j]]]];
      ];
      returnList
    );

    MakeCopy[varname_]:=(
    (* copies variables *)
      If[Length[varname]==0,Copy[varname]=ToExpression[varname]];
      For[i=1,i<=Length[varname],i++,
        copies[varname[[i]]]=ToExpression[varname[[i]]];
      ];
    );

    Copy[varname_]:=(
    (* returns copied variable *)
      copies[varname]
    );    

    ChiSquare[mod_List,exp_List]/;Length[mod]==Length[exp]:=(
    (* returns calculated chi square according to Pearson's chi-squared test [https://en.wikipedia.org/wiki/Pearson's_chi-squared_test#Calculating_the_test-statistic] *)
      Total[(mod-exp)^2/exp]//N
    );

    PearsonTest[mod_List,exp_List]/;Length[mod]==Length[exp]:=Block[{t},
    (* returns results of Pearson's chi-squared test, based on [https://mathematica.stackexchange.com/a/5590] *)
      t=ChiSquare[mod,exp];
      {Rule["ChiSquare",t],Rule["p-val",SurvivalFunction[ChiSquareDistribution[Length[exp]-1],t]]}
    ];

    ChiSquareSH[mod_List,exp_List]/;Length[mod]==Length[exp]:=(
    (* returns calculated chi square according to SH *)
      Total[(mod-exp)^2]/Length[mod]//N
    );

    RangeN[imin_,imax_,Num_]:=(
    (* returns generated defined number of values between defined borders *)
      N[Range[imin,imax,(imax-imin)/((Num)-1)]]
    );

    GetFWHM[data_]:=(
    (* returns calculated FWHM *)
      MaxY=Max[data[[All,2]]];
      MaxYCount=Count[data[[All,2]],MaxY];
      If[MaxYCount==Length[data],MaxYCount=0];
      If[MaxYCount>0,
        MaxPos=Flatten[Position[data[[All,2]],MaxY]];
        MaxElem=data[[MaxPos]];
        If[MaxYCount==1,
          MaxXa=MaxXb=Flatten[MaxElem][[1]];
        ,
          MaxXb=Max[MaxElem[[All,1]]];
          MaxXa=Min[MaxElem[[All,1]]];
        ];
        MinY=Min[data[[All,2]]];
        MaxXanr=Flatten[Position[data[[All,1]],MaxXa]][[1]];
        MaxXbnr=Flatten[Position[data[[All,1]],MaxXb]][[1]];
        HM=MinY+(Abs[MaxY-MinY]/2);
        FWHMax=GetX[data[[1;;MaxXbnr]],HM];
        FWHMbx=GetX[data[[MaxXanr;;-1]],HM];
        FWHM=Abs[FWHMbx-FWHMax];
      ,
        FWHM=0;
      ];
      FWHM
    );

    startTime=AbsoluteTime[]

    SetDuration[type_:0]:=(
    (* sets time point *)
      SetDuration::badArg = "\"`1`\" is bad argument";
      SetDuration::nrArg = "argument can't be a Number";
      If[type!=0,
        If[NumberQ[type]==False,
          If[ToLowerCase[type]=="start",
		  (* set start time point *)
            startTime=AbsoluteTime[];
          ,
            If[ToLowerCase[type]=="end",
			(* set end time point *)
              endTime=AbsoluteTime[];
            ,
              Message[SetDuration::badArg,type];
            ];
          ];
        ,
          Message[SetDuration::nrArg];
        ];
      ,
	  (* default: set start time point *)
        startTime=AbsoluteTime[];
      ];
    );

    StartDuration[]:=(
    (* sets start time point *)
      startTime=AbsoluteTime[];
    );

    EndDuration[]:=(
    (* sets end time point *)
      endTime=AbsoluteTime[];
    );

    PrintDuration[]:=(
    (* prints duration of calculations *)
      If[Defined["endTime"]==False,endTime=AbsoluteTime[];];
      Print["Done in ",endTime-startTime,"s"];
    );

  End[]

EndPackage[]
