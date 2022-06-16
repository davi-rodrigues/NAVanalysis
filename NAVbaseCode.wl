(* ::Package:: *)

(* ::Title:: *)
(*Base Code for the Normalized Additional Velocity (NAV)*)


(* ::Author:: *)
(*Davi C. Rodrigues*)
(*July 2021 - March 2022*)
(*Version 1.1*)

(* 
Examples of variables names conventions (inspired on the Hungarian convention):
  listDFunctionName :  a list of dimension D .
  plotFunctionName : a plot .
  isFunctionName : a boolean variable, it is either True or False .
  distributionFunctionName : a DataDistribution .
  statusFunctionName : a function that prints miscelaneous data for status purposes.
*)

BeginPackage["NAVbaseCode`"];

{globalData, headerGlobalData, YDcentral, YBcentral, clearOwnValues, addCol, squareSign, sqrtSign, medianUncertainty, equal0, colReff, 
colh, colLD, colLB, colL, colD, colYD, colYB,  colMHI, colMgas, coldf2, colV, colQ, cola0, colLoga0B, colMD, 
colMB, colMbar, colS1min, colS1plus,colS2min, colS2plus,  colS3min, colS3plus, colS5min, colS5plus, colHtype, colSeff, 
colS0, coli, colHIr, colChi2, colChired, colDataPoints, col\[Delta], col\[Delta]i, col\[Delta]D, colrc, collogRhoc, GalaxiesOutsideRAR, 
globalDataRAR, RotmodDir, Rotdata, colGRad, colGVobs, colG\[Delta]Vobs, colGVgas, colGVdisk, colGVbulge, colGSBdisk, colGSBbul,
galaxy, galNumbers, distance, galdata, putcolG, gd, hRadlist, normalRadlist, normalVobslist, galdataExt, colGhRad, colGnRad,
colGnVobs, colGVbar, colGVmiss, colGnVmiss, colGVmissLinear, colGnVmissLinear, colGVmiss2, colGVms2, colGnVmiss2, 
colG\[Delta]Vms, colGAobs, colGAms, colGnAms galdataRAR, gdR, definedFunctions, silvermanBw, gdRBulgeless, galdataRARBulgeless, 
Vbulge, exportBurkertIndividualResultsGaussian, exportBurkertIndividualResultsFixed, kpc, G0, ckpc, globalDataNfwFixed, globalDataNfwGY,
headerGlobalDataNfwFixed, headerGlobalDataNfwGY};

Begin["Private`"];


(* ::Subsection:: *)
(*Loading data*)


(*The following loads Burkert data. The only effect is to define TableResults.*)
If[Global`isBurkertWithGaussianPriors == True,
  Get["Burkert-GY-05-06-MAGMAtableResults.m", Path -> "AuxiliaryData"] ,
  (*else*)
  Get["Burkert-Fixed-MAGMAtableResults.m", Path -> "AuxiliaryData"] ,
  (*Neither True or False*)
  Print["The variable isBurkertWithGaussianPriors need to be set as True or False before starting BaseCode"];
  Abort[]
];

globalData = Drop[TableResults,1];
headerGlobalData = TableResults[[1]];

Get["NFW-Fixed-05-06-MAGMAtableResults.m", Path -> "AuxiliaryData"];
globalDataNfwFixed = Drop[TableResults,1];
headerGlobalDataNfwFixed = TableResults[[1]];

Get["NFW-GY-05-06-MAGMAtableResults.m", Path -> "AuxiliaryData"];
globalDataNfwGY = Drop[TableResults,1];
headerGlobalDataNfwGY = TableResults[[1]];

Clear[TableResults];

(* 
  General purpose constants, from PDG 2011.
  The main unit for distance is kpc. Secondarily, km is also used.
  The single unit for mass is the solar mass (Msun).
  The single unit for time is the second (s).
*)
kpc = 3.08568025*10^16 (*km*)
G0 = 6.67428 10^-11 /(3.08568025 10^19)^3  1.98892 10^30;(* kpc^3 / (Msun \ s^2) *)
ckpc = 299792.458 (* km/s *) / kpc; (* kpc / s *)


(*Central values for YD and YB, these values should match the imported files *)
YDcentral = 0.5; 
YBcentral = 0.6;


(* ::Subsection:: *)
(*General purpose definitions*)


SetAttributes[clearOwnValues, {HoldAll}];
clearOwnValues[x_] :=  Last[Map[Clear,First[OwnValues[x]], {2}]];

addCol[data_, new_?ListQ]:= Join[data, Partition[new,1],2]; (*Adds a column to a table*)

(*The square of a real quanity, without changing its sign. Useful for velocity rotation curves*)
squareSign[x_] := Sign[x]x^2; 
(*The squared root of a real quantity in a special sense used for galaxy rotation curves. *)
sqrtSign[x_] := Sign[x]Sqrt@RealAbs[x]; 

(*From Muller 2000, J.Res.Natl.Inst.Stand.Technol, as used in Gentile et al, Things about mond.*)
medianUncertainty[x_, n_] := 1.858/Sqrt[n-1] MedianDeviation[x]; 

SetAttributes[equal0, Listable];
equal0[x_] := Equal[x,0];


(* ::Subsection:: *)
(*Global data related definitions*)


(* ::Subsubsection:: *)
(*Column variables and numbers*)


If[Head[correspondingcolnumbers]=== Symbol, Null,clearOwnValues[correspondingcolnumbers]]; (*useful for running twice this cell, without needing to quit the kernel.*)

specialheadernames ={"Reff", "h", "LD", "LB", "L", "D", "YD", "YB", "MHI", "Mgas", "df2", "V_flat","Q flag", "a0", "a0_mBayes", "MD", "MB", "Mbar","a0 S1-","a0 S1+","a0 S2-","a0 S2+", "a0 S3-","a0 S3+", "a0 S5-","a0 S5+" , "H. type", "Sigma eff", "Sigma0", "inclination","HI radius at 1Msun/pc2", "Chi2", "Chi2red", "N.data points", "delta D", "Delta inc.", "delta D", "rc", "logRhoc"};
correspondingcolnumbers = {colReff, colh, colLD, colLB, colL, colD, colYD, colYB,  colMHI, colMgas, coldf2, colV, colQ, cola0, colLoga0B, colMD, colMB, colMbar, colS1min, colS1plus,colS2min, colS2plus,  colS3min, colS3plus, colS5min, colS5plus, colHtype, colSeff, colS0, coli, colHIr, colChi2, colChired, colDataPoints, col\[Delta], col\[Delta]i, col\[Delta]D, colrc, collogRhoc};

MapThread[If[Position[headerGlobalData, #1] == {},Null,  #2 = Position[headerGlobalData, #1][[1,1]]]&,{specialheadernames, correspondingcolnumbers}];



(* ::Subsubsection:: *)
(*The RAR subsample with 153 galaxies*)


badinc = Select[globalData,#[[coli]]<30 &][[All,1]];
badQ = Select[globalData,#[[colQ]]==3 &][[All,1]];
bad=Union[badinc,badQ];

(*These are the galaxy numbers that do not belog to the RAR*)
GalaxiesOutsideRAR = Flatten[(Position[globalData[[All,1]], #] & /@ bad),1];

(*dataRAR is useful in general, but at the moment it is not not used in this code.*)
globalDataRAR = Delete[globalData,GalaxiesOutsideRAR]; 


(* ::Subsection:: *)
(*Rotmod data (individual galaxy data)*)


(*Directory of rotmod files*)
RotmodDir = FileNameJoin[{"AuxiliaryData", "SPARC_Rotmod"}]; 

files = FileNames["*_rotmod.dat", RotmodDir]; 

Rotdata = Join[Import[files[[#]], "Data"] & /@ Range[Length[files]]] ;

(*Column numbers for individual galaxy data*)
colGRad = 1; 
colGVobs = 2;
colG\[Delta]Vobs = 3;
colGVgas = 4;
colGVdisk=5;
colGVbulge = 6;
colGSBdisk = 7;
colGSBbul = 8;



(* ::Subsection:: *)
(*Individual galaxy functions: all the data come from the Rotmod files*)


galaxy::usage = "galaxy[i] provides the name of the i-th galaxy, in alphabetical order, extracted from the file names.";
galaxy[i_] := 
  Block[{startname, endname, j},
    startname[j_] := StringPosition[files[[j]],RotmodDir][[1,2]] + 2;
    endname[j_] := StringPosition[files[[j]], "_rotmod"][[1,1]] - 1; 
    StringTake[files[[i]], {startname[i], endname[i]}] 
  ];

galNamesTable = Table[galaxy[i], {i,1,Length[files]}];

(*A simple but important cross check. It only prints in case of error.*)
If[ 
  galNamesTable == globalData[[All,1]],   
  Null (*Echo["Names from the files match those from the data table."]*), 
  (*else*)
  Echo["Names from the files DO NOT match! Aborting."]; 
  Abort[]
]; 

galNumbers::usage = "galNumbers[{list of galaxy names}]. Gives a list of the corresponding  galaxy numbers from their names. These numbers are always in reference to the complete sample of 175 galaxies.";
galNumbers[listofNames_] := Flatten[Position[galNamesTable,#] & /@ listofNames ] ;

(*distance of the ith galaxy, in Mpc*)
distance[i_] := Rotdata[[i,1,4]]; 

(* Rotdata for the i-th galaxy without the header*)
galdata[i_] := galdata[i] =Select[Rotdata[[i]], \[Not]#[[1]] == "#" & ] ; 

(*putcolG inserts "colG" in front of the argument, and it is listable. *)
SetAttributes[putcolG, { Listable, HoldFirst}];
putcolG[x_] := ToExpression["colG"<>ToString[HoldForm[x]]]; 

ClearAll[gd];
gd::usage = 
	"gd is a shortcut for calling specifc data from the function galdata.\n"<>
	"gd[list] returns a table composed by the columns given by list and for all the galaxies.\n"<>
	"gd[list,i] returns a table composed by the columns given by list of the galaxy i. \n"<>
	"gd[list,i,j] is the equivalent of gd[list,i][[j]].\n"<>
	"`list' can include any combination of the folowing strings that match columns: Rad, Vobs, \[Delta]Vobs, Vgas, Vdisk, Vbulge, SBdisk, SBbulge."; 

SetAttributes[gd, HoldFirst];
gd[listcols_, galn_] := If[listcols === All, galdata[galn], galdata[galn][[All,putcolG@listcols]]];
gd[listcols_, galn_, line_] := gd[listcols, galn][[line]];
gd[listcols_]:=Table[gd[listcols, galn], {galn, 175}];



(* ::Subsection:: *)
(*Extended Individual galaxy functions: defines new functions and extend the table output*)


(* ::Subsubsection:: *)
(*Extended data*)


(*Galaxy i radii list divided by its h value.*)
hRadlist[i_] := gd[Rad,i]/globalData[[i,colh]]; 

(*Galaxy i radii list divided by its maximum radius.*)
normalRadlist[i_] := gd[Rad,i]/gd[Rad,i,-1]; 

normalVobslist[i_] := gd[Vobs,i]/gd[Vobs,i,-1];

(*Provides a list for the total baryonic velocity for Galaxy i*)
vbarlist[i_, YD_:YDcentral, YB_:YBcentral]:= 
  sqrtSign[YD squareSign@gd[Vdisk, i] + YB squareSign@gd[Vbulge,i] + squareSign@gd[Vgas,i]]; 

(*The missing velocity of Galaxy i: observed velocity minus baronic velocity*)
VmissLinearlist[i_,YD_:YDcentral, YB_:YBcentral] :=  
  gd[Vobs, i] - vbarlist[i, YD, YB];

normalVmissLinearlist[i_,YD_:YDcentral, YB_:YBcentral] := 
  VmissLinearlist[i,YD,YB]/Last@VmissLinearlist[i,YD,YB] ;

Vmisslist[i_,YD_:YDcentral, YB_:YBcentral] :=  
  sqrtSign[squareSign@gd[Vobs, i] - squareSign@vbarlist[i, YD, YB]];

normalVmisslist[i_,YD_:YDcentral, YB_:YBcentral] := 
  Vmisslist[i,YD,YB]/Last@Vmisslist[i,YD,YB] ;

Vmiss2list[i_,YD_:YDcentral, YB_:YBcentral] :=  
  squareSign@gd[Vobs, i] - squareSign@vbarlist[i, YD, YB];

normalVmiss2list[i_,YD_:YDcentral, YB_:YBcentral] := 
  Vmiss2list[i,YD,YB]/Last@Vmiss2list[i,YD,YB] ;

aobslist[i_,YD_:YDcentral, YB_:YBcentral] := squareSign@gd[Vobs, i]/gd[Rad,i]/ kpc;

amisslist[i_,YD_:YDcentral, YB_:YBcentral] := 
  (squareSign@gd[Vobs, i]/gd[Rad,i] - squareSign@vbarlist[i, YD, YB]/gd[Rad,i] )/ kpc;

normalamisslist[i_,YD_:YDcentral, YB_:YBcentral] :=   amisslist[i,YD,YB]/Last@amisslist[i,YD,YB] ;

ClearAll[galdataExt];
galdataExt[i_] := galdataExt[i]= Fold[
  addCol, 
  galdata[i], 
  {
    hRadlist[i], 
    normalRadlist[i], 
    normalVobslist[i], 
    vbarlist[i], 
    Vmisslist[i], 
    normalVmisslist[i], 
    VmissLinearlist[i],
    normalVmissLinearlist[i], 
    Vmiss2list[i], 
    normalVmiss2list[i] , 
    aobslist[i], 
    amisslist[i],
    normalamisslist[i]
  }
];

(*To execute all the galdataExt, speeding latter computations*)
Do[galdataExt[i],{i, 175}];

(*Here the names of the additional columns*)
colGhRad = 9;
colGnRad = 10;
colGnVobs = 11;
colGVbar = 12;
colGVmiss = 13; (*Vmiss stands for missing velocity*)
colGnVmiss = 14;
colGVmissLinear = 15;
colGnVmissLinear = 16;
colGVmiss2 = 17;
colGVms2 = 17; (*Another name for the same thing*)
colGnVmiss2 = 18;
colG\[Delta]Vms = 18; (*Another name for the same thing. Note that with \[Delta] the 2 is not used.*)
colGAobs = 19;
colGAms = 20; (*Ams stands for missing acceleration. I am using ms instead of miss. Later the notation needs to be normalized.*)
colGnAms = 21; 


(* ::Subsubsection:: *)
(*Extended gd implementation: gd is redefined*)


(*The following redefines gd, now defined from to galdataExt*)
Clear[gd];
gd[listcols_, galn_] :=
  If[
    listcols === All, 
    galdataExt[galn], 
    galdataExt[galn][[All, putcolG@listcols]]
  ];
gd[listcols_, galn_, line_] := gd[listcols, galn][[line]];
gd[listcols_] := gd[listcols] = Table[gd[listcols, galn], {galn, 175}]



(* ::Subsubsection:: *)
(*galdataRAR and gdR: same as galdata and gd, but returns {} if the galaxy is outside the RAR sample.*)


Clear[galdataRAR];
galdataRAR::usage = 
  "galdataRAR[i] returns galdataExt[i] if the i-th galaxy is inside the RAR sample, otherwise it returns {}.";
galdataRAR[i_] :=  galdataRAR[i] = 
  Block[{isOutsideRAR, j},
    isOutsideRAR[j_] := Intersection[{j}, Flatten@GalaxiesOutsideRAR] != {};
    If[isOutsideRAR[i], {},  galdataExt[i]] 
  ];

Clear[gdR];
SetAttributes[gdR, HoldFirst];
gdR::usage = 
  "gdR is a shortcut for calling specifc data from the function galdataRAR.\n" <>
  "It is the equivalent of gd, but it only displays galaxies in the RAR sample (3168 data points)."; 
gdR[listcols_, galn_] := gdR[listcols, galn] = If[listcols === All, galdataRAR[galn], galdataRAR[galn][[All, putcolG@listcols]]];
gdR[listcols_, galn_, line_] := gdR[listcols, galn, line] = gdR[listcols, galn][[line]];
gdR[listcols_] := gdR[listcols] = Table[gdR[listcols, galn], {galn, 175}];


(* ::Subsubsection:: *)
(*galdataRARBulgeless and gdRBulgeless: same as galdata and gd, but returns {} if the galaxy is outside the RAR sample or it has a bulge. *)


Clear[galdataRARBulgeless];
galdataRARBulgeless::usage = 
  "galdataRARBulgeless[i] returns galdataRAR[i] unless the galaxy has a bulge, in the latter case it returns {}.";
galdataRARBulgeless[i_] :=  galdataRARBulgeless[i] = 
  Block[{hasBulge},
    hasBulge = If[
      Total[gdR[Vbulge, i]^2] > 0, 
      True, 
      False
    ];
    If[hasBulge, {},  galdataRAR[i]] 
  ];

Clear[gdRBulgeless];
SetAttributes[gdRBulgeless, HoldFirst];
gdRBulgeless::usage = 
  "gdRBulgeless is a shortcut for calling specifc data from the function galdataRARBulgeless.\n" <>
  "It is the equivalent of gd, but it only displays galaxies in the RAR sample and without a Bulge."; 
gdRBulgeless[listcols_, galn_] := gdRBulgeless[listcols, galn] = If[listcols === All, galdataRARBulgeless[galn], galdataRARBulgeless[galn][[All, putcolG@listcols]]];
gdRBulgeless[listcols_, galn_, line_] := gdRBulgeless[listcols, galn, line] = gdRBulgeless[listcols, galn][[line]];
gdRBulgeless[listcols_] := gdRBulgeless[listcols] = Table[gdRBulgeless[listcols, galn], {galn, 175}];



(* ::Subsubsection:: *)
(*galdata\[Delta]Vms definition*)


  Clear[galdata\[Delta]Vms, galdata\[Delta]VmsAux]
  mean\[Delta]Vms[i_, Rcut_ : RRmaxcut] := Mean[Select[gdR[{nRad, \[Delta]Vms}, i], #[[1]] < Rcut &][[All, 2]]];

  (*The following function removes the oulier galaxies:*)
  galdata\[Delta]Vms[i_, start_ : Xmin, end_ : Xmax, Rcut_ : RRmaxcut] := galdata\[Delta]Vms[i, start, end, Rcut] = 
    If[
      Length[galdataRAR[i]] > 0 && Xmin < mean\[Delta]Vms[i, Rcut] < Xmax,
      galdataRAR[i],
      {}
    ];

  (*The following data compilation does not consider data points with R/Rmax \[LessEqual] 0.05 and inserts the point {0,0} if the galaxy was not eliminated alread.*)
  Clear[galdata\[Delta]VmsCrop];
  galdata\[Delta]VmsCrop[i_, start_ : Xmin, end_ : Xmax, Rcut_ : RRmaxcut] := 
    galdata\[Delta]VmsCrop[i, start, end, Rcut] =
    Select[galdata\[Delta]Vms[i, start, end, Rcut], #[[colGnRad]] > 0.05 &];


(* ::Subsection:: *)
(*Silverman bandwidth definition*)
(*I am not using the Mathetica's built in version, but they agree.*)


Clear[silvermanBw];
silvermanBw::usage = 
  "silvermanBw[data] finds the Silverman bandwidth for the provided data." <> 
  "It works for arbitrary dimensions and uses the expression... See " <>
  "https://mathematica.stackexchange.com/questions/255281/2d-kernel-density-estimation-smoothkerneldistribution-with-bandwidth-estimatio?noredirect=1#comment637566_255281 "<> 
  "and https://mathematica.stackexchange.com/questions/25423/the-default-bandwidth-of-the-smoothkerneldistribution-function";

silvermanBw[dataX_?ListQ] := silvermanBw[dataX] = Block[
  {std, interquartile, A, n, d, k},
  d = If[Length@First @ dataX == 0, 1, Length@First @ dataX];
  n = Length @ dataX;
  std = StandardDeviation @ dataX;
  interquartile = (Quantile[dataX, 0.75] - Quantile[dataX, 0.25]) / 1.34;
  k = (9 * 3^(1/5))/(10 * 2^(2/5)) * (4/(d+2))^(1/(d+4));
  If[d==1, std = {std}; interquartile = {interquartile}];
  A= MapThread[ Min @ {#1,#2} & , {std, interquartile}];
  If[d==1, A = First @ A];
  k * n^(-1/(d+4)) A
];

(* ::Subsection:: *)
(*Difines functions to export Burkert results in tsv format*)

exportBurkertIndividualResultsGaussian:= Block[
  {dataBurkertFitsExport},
  If[Global`isBurkertWithGaussianPriors == True, Null, Print["This requires isBurkertWithGaussianPriors==True. Try exportBurkertIndividualResultsFixed."]; Abort[]];
  Export["headerAux.txt", {
    "# Additional velocity distribution: a fast sample analysis for dark matter or modified gravity models",
    "# by A. Hernandez-Arboleda, D. C. Rodrigues, A. Wojnar",
    "# ",
    "# Table 3 data: Burkert profile results for 153 SPARC galaxies with Gaussian priors on YD and YB.",
    "# First column: galaxy name.",
    "# Second column: best-fit core radius (rc).",
    "# Third column: best-fit of the logarithm of the central halo density (logRhoc).",
    "# Fourth column: best-fit of the stellar disk mass-to-light ratio (YD).",
    "# Fifth column: best-fit of the stellar bulge mass-to-light ratio (YB).",
    "# Sixth column: Minimum chi-squared value (Chi2).",
    "# Seventh column: the number of galaxy data points that were used for the fit (DataPoints).", 
    "# ",
    "# "
  }];

  dataBurkertFitsExport = globalDataRAR[[All, {1, colrc, collogRhoc, colYD, colYB, colChi2, colDataPoints}]];
  PrependTo[dataBurkertFitsExport, {"Galaxy", "rc", "logRhoc", "YD", "YB", "Chi2", "DataPoints"}];
  Export["BurkertFits-05-06-GaussianAux.tsv",dataBurkertFitsExport , Alignment-> Left, "TextDelimiters"-> None];

  Run["cat headerAux.txt BurkertFits-05-06-GaussianAux.tsv > BurkertFits-05-06-Gaussian.tsv"]; (*It is only guaranteed to work in Unix systems. Sorry Windows...*)

  DeleteFile["headerAux.txt"];
  DeleteFile["BurkertFits-05-06-GaussianAux.tsv"]
];

exportBurkertIndividualResultsFixed:= Block[
  {dataBurkertFitsExport},
  If[Global`isBurkertWithGaussianPriors == False, Null, Print["This requires isBurkertWithGaussianPriors==False. Try exportBurkertIndividualResultsGaussian."]; Abort[]];
  Export["headerAux.txt", {
    "# Additional velocity distribution: a fast sample analysis for dark matter or modified gravity models",
    "# by A. Hernandez-Arboleda, D. C. Rodrigues, A. Wojnar",
    "# ",
    "# Table 2 data: Burkert profile results for 153 SPARC galaxies with fixed YD=0.5 and YB=0.6.",
    "# First column: galaxy name.",
    "# Second column: best-fit core radius (rc).",
    "# Third column: best-fit of the logarithm of the central halo density (logRhoc).",
    "# Fourth column: Minimum chi-squared value (Chi2).",
    "# Fifth column: the number of galaxy data points that were used for the fit (DataPoints).", 
    "# ",
    "# "
  }];

  dataBurkertFitsExport = globalDataRAR[[All, {1, colrc, collogRhoc, colChi2, colDataPoints}]];
  PrependTo[dataBurkertFitsExport, {"Galaxy", "rc", "logRhoc", "Chi2", "DataPoints"}];
  Export["BurkertFits-05-06-FixedAux.tsv",dataBurkertFitsExport , Alignment-> Left, "TextDelimiters"-> None];

  Run["cat headerAux.txt BurkertFits-05-06-FixedAux.tsv > BurkertFits-05-06-Fixed.tsv"]; (*It is only guaranteed to work in Unix systems. Sorry Windows...*)

  DeleteFile["headerAux.txt"];
  DeleteFile["BurkertFits-05-06-FixedAux.tsv"]
];



(* ::Subsection:: *)
(*Prints all the introduced functions in this package*)


definedFunctions = Select[
  Names["NAVbaseCode`"<>"*"], 
  ToExpression[#, InputForm, DownValues] =!= {} &
];

Print@Information[StringExpression[Alternatives@@definedFunctions]];

End[]
EndPackage[]
