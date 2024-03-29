(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



saveAllPlots = False; (*Change this to True to save ALL the plots that are generated in THIS notebook. It does not include NAVobservational plots. Saves for specific models are in their respective sections.*)
saveNAVobservationalPlots = False;
saveNAVobservationalTables = False;

pathBaseDirectory = NotebookDirectory[];
pathOutputDirectory = FileNameJoin[{pathBaseDirectory, "Output"}];
pathAuxDirectory = FileNameJoin[{pathBaseDirectory, "AuxiliaryData"}];

SetDirectory @ pathBaseDirectory;

Needs @ "NAVbaseCode`";
Get @ "NAVoptions.wl";
Get @ "NAVobservational.wl";
Get @ "NAVauxiliaryFunctions.wl";
Get["MDC14aux.mx", Path -> pathAuxDirectory] // Quiet; (*If something goes wrong, this file will be generated*)

SetDirectory @ pathOutputDirectory;

savePreviousPlot[fileName_] := If[saveThisPlot || saveAllPlots, 
  Echo @ Export[ToString @ fileName, %]; saveThisPlot = False
];


saveThisPlot = False;

dataDeltaVobs = Block[{rawData},
  rawData = Re[{Log10[#1] , Log10[#2]} & @@@ Flatten[gdR[{Rad, Vmiss2}],1]];
  Select[rawData, #[[1]] > 0 &]
];

Clear[b, loga, model, logR, loga0];
model0 = logR + loga0;
loga0 = loga0 /. First@FindFit[dataDeltaVobs,  model0 ,  {loga0} , logR]

Clear[loga];
model = b logR + loga;
{b , loga} = {b, loga} /. FindFit[dataDeltaVobs,  model , {b , loga} , logR]

Show[
  plotBlueZero[
    {Log10[#1], Log10[#2]} & @@@ Flatten[gdR[{Rad, Vmiss2}], 1], 
    PlotRange-> {{-0.0,2.1}, {1.5,5.5}}
  ],
  Plot[
    {
      model
    }, 
    {logR, -1, 3}, 
    PlotStyle->{{Black, Thick, Dashed}, {Black, Thick}, {Black, DotDashed}},
    PlotRange->All
  ]
]

savePreviousPlot["plotDeltaV2analysis.pdf"];


saveThisPlot = False;

Clear[ac];
list2restrictedRARRot = Select[list2RARRot, 0.2 <  #[[1]] < 0.9 &] ;
{ac} ={ac} /. FindFit[list2restrictedRARRot,  r^ac , ac , r]

Clear[a, b, c, d, e, f, g, h];

list = Table[{r, list1InterpCurvesRAR[[4]][r]}, {r,0.2, 0.9, 0.05}];
{c, d} = {c , d}/. FindFit[list, {2 r^c -  r^d},  {c, d}, r]

list = Table[{r, list1InterpCurvesRAR[[3]][r]}, {r,0.2, 0.9, 0.05}];
{b} = {b}/. FindFit[list, r^b,  {b}, r]

list = Table[{r, list1InterpCurvesRAR[[2]][r]}, {r,0.2, 0.9, 0.05}];
{e, f} = {e, f}/. FindFit[list, {2 r^e -  r^f},  {e, f}, r]

list = Table[{r, list1InterpCurvesRAR[[1]][r]}, {r,0.2, 0.9, 0.05}];
{g} = {g}/. FindFit[list, r^g ,  {g}, r]

plotPowerLawModel = Show[
  {
    plotBackground[1.5],
    plotSigmaRegionsRAR,
    Plot[
      {
        2 rn^c - rn^d, 
        2 rn^e -  rn^f,  
        rn^b, 
        rn^g
      },
      {rn, 0, 1},
      PlotStyle -> {
        {Darker[Red, 0.2], Thickness @ 0.003},
        {Lighter[Red, 0.5], Thickness @ 0.003}
      },
      Filling -> {
        1 -> {{3}, Directive[Lighter[Red, 0.5], Opacity @ 0.2]},
        2 -> {{4}, Directive[Lighter[Red, 0.2], Opacity @ 0.2]}
      },
      PlotRange -> All
    ],
    Plot[rn^ac, {rn,0,1}, PlotStyle -> {Black, Dashed}]
  }
]

savePreviousPlot["plotPowerLawModel.pdf"];


efficiencyNAV[#^g &, 2 #^e - #^f &, 1]
efficiencyNAV[#^b &, 2 #^c - #^d &, 2]
efficiencyNAVtotal[#^g &, 2 #^e - #^f &, #^b &, 2 #^c - #^d &]


\[Delta]Varctan[rn_, rtn_] = ArcTan[rn/rtn]^2/ArcTan[1/rtn]^2;

\[Delta]VarctanLimit1[rn_] = Limit[\[Delta]Varctan[rn, rtn], rtn -> \[Infinity]];
\[Delta]VarctanLimit2[rn_] = Limit[\[Delta]Varctan[rn, rtn], rtn -> 0];
  

plotArctanGrayRed = Show[
  {
    plotBackground[1.5],
    plotSigmaRegionsRAR,
    Plot[
      {
        \[Delta]VarctanLimit1[rn], 
        \[Delta]VarctanLimit2[rn]
      },
      {rn, 0, 1},
      PlotRange -> All,
      PlotStyle -> {{Thickness[0.005], Black, Dashed}}
    ]
  }
];

Echo["plotArctanGrayRed:"];
Print@plotArctanGrayRed;




saveThisPlot = False;

(* DEFINITIONS *)

rnStart = 0.2;
rnEnd = 0.9;

lowerBound[1] = list1InterpCurvesRAR[[1]];
upperBound[1] = list1InterpCurvesRAR[[2]];
lowerBound[2] = list1InterpCurvesRAR[[3]];
upperBound[2] = list1InterpCurvesRAR[[4]];

Clear[chi2Upper, chi2Lower];
chi2Upper[rtn_?NumberQ, n\[Sigma]_] := NIntegrate[
  (upperBound[n\[Sigma]][rn] - \[Delta]Varctan[rn,rtn])^2, 
  {rn, rnStart, rnEnd}, 
  Method-> {Automatic, "SymbolicProcessing" -> 0},
  WorkingPrecision->10, 
  PrecisionGoal->3, 
  AccuracyGoal->Infinity, 
  MaxRecursion->10
];

chi2Lower[rtn_?NumberQ, n\[Sigma]_] := NIntegrate[
  (lowerBound[n\[Sigma]][rn] - \[Delta]Varctan[rn,rtn])^2, 
  {rn, rnStart, rnEnd}, 
  Method-> {Automatic, "SymbolicProcessing"-> 0},
  WorkingPrecision->10, 
  PrecisionGoal->3, 
  AccuracyGoal->Infinity, 
  MaxRecursion->10
];


(* EXECUTION *)

Echo["Performing the optimization."];

rtnUpper[2] = rtn /. NMinimize[{chi2Upper[rtn,2],rtn>0.001},{rtn,0,10}][[2]];
rtnUpper[1] = rtn /. NMinimize[{chi2Upper[rtn,1],rtn>0.001},{rtn,0,10}][[2]];
rtnLower[2] = rtn /. NMinimize[{chi2Lower[rtn,2],rtn>0.001},{rtn,0,10}][[2]];
rtnLower[1] = rtn /. NMinimize[{chi2Lower[rtn,1],rtn>0.001},{rtn,0,10}][[2]];

Echo[{rtnUpper[1], rtnLower[1]}, "rtn 1\[Sigma] bounds: "];
Echo[{rtnUpper[2], rtnLower[2]}, "rtn 2\[Sigma] bounds: "];

plotArctanGlobalBestFit = Show[
  {
    plotArctanGrayRed,
    Plot[
      {
        \[Delta]Varctan[rn, rtnUpper @ 2],
        \[Delta]Varctan[rn, rtnUpper @ 1],
        \[Delta]Varctan[rn, rtnLower @ 2],
        \[Delta]Varctan[rn, rtnLower @ 1]
      },
      {rn, 0, 1},
      PlotStyle -> {
        {Darker[Blue, 0.2], Thickness @ 0.003},
        {Lighter[Blue, 0.5], Thickness @ 0.003}
      },
      Filling -> {
        1 -> {{3}, Directive[Lighter[Blue, 0.5], Opacity @ 0.2]},
        2 -> {{4}, Directive[Lighter[Blue, 0.2], Opacity @ 0.2]}
      },
      PlotRange -> All
    ]
  }
];

Echo["plotArctanGlobalBestFit:"];
Print@plotArctanGlobalBestFit;

savePreviousPlot["plotArctanGlobalBestFit.pdf"];


efficiencyNAV[\[Delta]Varctan[#, rtnLower@ 1] &, \[Delta]Varctan[#, rtnUpper@ 1] &, 1]
efficiencyNAV[\[Delta]Varctan[#, rtnLower@ 2] &, \[Delta]Varctan[#, rtnUpper@ 2] &, 2]
efficiencyNAVtotal[\[Delta]Varctan[#, rtnLower@ 1] &, \[Delta]Varctan[#, rtnUpper@ 1] &, \[Delta]Varctan[#, rtnLower@ 2] &, \[Delta]Varctan[#, rtnUpper@ 2] &]


saveThisPlot = False;

resultsArctan = Get["../AuxiliaryData/arctan-GY-1-MAGMAtableResults.m"]; (*These results only inlcude the 153 RAR galaxies*)
headerArctan = First @ resultsArctan;
resultsArctanData = Drop[resultsArctan, 1];
colRt = First @ Flatten @ Position[headerArctan, "Rt"];
listRtn = resultsArctanData[[All, colRt]] / (rmax153 /@ Range @ 153);
rectangle = {
  EdgeForm[{Lighter[Blue, 0.5], Thickness @ 0.003}],
  Lighter[Blue, 0.5],
  Opacity @ 0.2,
  Rectangle[{Log10@rtnUpper @ 1, 0}, {Log10@rtnLower @ 1, 100}]
};

Histogram[
  Log10 @ listRtn, 
  {0.2}, 
  PlotRange -> All,
  Frame -> True, 
  Axes -> False, 
  Epilog -> {rectangle},
  histoOptions
]

savePreviousPlot["histogramArctan.pdf"];


colChi2Arctan = First @ Flatten @ Position[headerArctan, "Chi2"];
colVChi2Arctan = First @ Flatten @ Position[headerArctan, "V-Chi2"];(*Chi2 only due to velocity, no priors, standard chi2*)
listArctanChi2 = resultsArctanData[[All, colChi2Arctan]];
listArctanVChi2 = resultsArctanData[[All, colVChi2Arctan]];
Echo[{Median @ listArctanChi2, Median @ listArctanVChi2}, "Median {Chi2, Chi2Eff}: "];
Echo[{Total @ listArctanChi2, Total @listArctanVChi2}, "Total {Chi2, Chi2Eff}: "];


\[Delta]VarctanHalf[rn_, rtn_] = ArcTan[rn/rtn]/ArcTan[1/rtn];

\[Delta]VarctanHalfLimit1[rn_] = Limit[\[Delta]VarctanHalf[rn, rtn], rtn -> \[Infinity]];
\[Delta]VarctanHalfLimit2[rn_] = Simplify[Limit[\[Delta]VarctanHalf[rn, rtn], rtn -> 0], rn > 0];
  

plotArctanHalfGrayRed = Show[
  {
    plotBackground[1.5],
    plotSigmaRegionsRAR,
    Plot[
      {
        \[Delta]VarctanHalfLimit1[rn], 
        \[Delta]VarctanHalfLimit2[rn]
      },
      {rn, 0, 1},
      PlotRange -> All,
      PlotStyle -> {{Thickness[0.005], Black, Dashed}}
    ]
  }
];

Echo["plotArctanHalfGrayRed:"];
Print@plotArctanHalfGrayRed;




saveThisPlot = False;

(* DEFINITIONS *)

rnStart = 0.2;
rnEnd = 0.9;

lowerBound[1] = list1InterpCurvesRAR[[1]];
upperBound[1] = list1InterpCurvesRAR[[2]];
lowerBound[2] = list1InterpCurvesRAR[[3]];
upperBound[2] = list1InterpCurvesRAR[[4]];

Clear[chi2Upper, chi2Lower];
chi2Upper[rtn_?NumberQ, n\[Sigma]_] := NIntegrate[
  (upperBound[n\[Sigma]][rn] - \[Delta]VarctanHalf[rn,rtn])^2, 
  {rn, rnStart, rnEnd}, 
  Method-> {Automatic, "SymbolicProcessing" -> 0},
  WorkingPrecision->10, 
  PrecisionGoal->3, 
  AccuracyGoal->Infinity, 
  MaxRecursion->10
];

chi2Lower[rtn_?NumberQ, n\[Sigma]_] := NIntegrate[
  (lowerBound[n\[Sigma]][rn] - \[Delta]VarctanHalf[rn,rtn])^2, 
  {rn, rnStart, rnEnd}, 
  Method-> {Automatic, "SymbolicProcessing"-> 0},
  WorkingPrecision->10, 
  PrecisionGoal->3, 
  AccuracyGoal->Infinity, 
  MaxRecursion->10
];


(* EXECUTION *)

Echo["Performing the optimization."];

rtnUpper[2] = rtn /. NMinimize[{chi2Upper[rtn,2],rtn>0.001},{rtn,0,10}][[2]];
rtnUpper[1] = rtn /. NMinimize[{chi2Upper[rtn,1],rtn>0.001},{rtn,0,10}][[2]];
rtnLower[2] = rtn /. NMinimize[{chi2Lower[rtn,2],rtn>0.001},{rtn,0,10}][[2]];
rtnLower[1] = rtn /. NMinimize[{chi2Lower[rtn,1],rtn>0.001},{rtn,0,10}][[2]];

Echo[{rtnUpper[1], rtnLower[1]}, "rtn 1\[Sigma] bounds: "];
Echo[{rtnUpper[2], rtnLower[2]}, "rtn 2\[Sigma] bounds: "];

plotArctanHalfGlobalBestFit = Show[
  {
    plotArctanHalfGrayRed,
    Plot[
      {
        \[Delta]VarctanHalf[rn, rtnUpper @ 2],
        \[Delta]VarctanHalf[rn, rtnUpper @ 1],
        \[Delta]VarctanHalf[rn, rtnLower @ 2],
        \[Delta]VarctanHalf[rn, rtnLower @ 1]
      },
      {rn, 0, 1},
      PlotStyle -> {
        {Darker[Blue, 0.2], Thickness @ 0.003},
        {Lighter[Blue, 0.5], Thickness @ 0.003}
      },
      Filling -> {
        1 -> {{3}, Directive[Lighter[Blue, 0.5], Opacity @ 0.2]},
        2 -> {{4}, Directive[Lighter[Blue, 0.2], Opacity @ 0.2]}
      },
      PlotRange -> All
    ]
  }
];

Echo["plotArctanHalfGlobalBestFit:"];
Print@plotArctanHalfGlobalBestFit;

savePreviousPlot["plotArctanHalfGlobalBestFit.pdf"];


Off[NIntegrate::izero];
efficiencyNAV[\[Delta]VarctanHalf[#, rtnLower@ 1] &, \[Delta]VarctanHalf[#, rtnUpper@ 1] &, 1]
efficiencyNAV[\[Delta]VarctanHalf[#, rtnLower@ 2] &, \[Delta]VarctanHalf[#, rtnUpper@ 2] &, 2]
Mean[{%, %%}]
On[NIntegrate::izero];


saveThisPlot = False;

resultsArctanHalf = Get["../AuxiliaryData/arctanHalf-GY-1-MAGMAtableResults.m"]; (*These results only inlcude the 153 RAR galaxies*)
headerArctanHalf = First @ resultsArctanHalf;
resultsArctanHalfData = Drop[resultsArctanHalf, 1];
colRt = First @ Flatten @ Position[headerArctanHalf, "Rt"];
listRtnHalf = resultsArctanHalfData[[All, colRt]] / (rmax153 /@ Range @ 153);
rectangle = {
  EdgeForm[{Lighter[Blue, 0.5], Thickness @ 0.003}],
  Lighter[Blue, 0.5],
  Opacity @ 0.2,
  Rectangle[{Log10@rtnUpper @ 1, 0}, {Log10@rtnLower @ 1, 100}]
};

Histogram[
  Log10 @ listRtnHalf, 
  {0.2}, 
  PlotRange -> All,
  Frame -> True, 
  Axes -> False, 
  Epilog -> {rectangle},
  histoOptions
]

savePreviousPlot["histogramArctanHalf.pdf"];


colChi2ArctanHalf = First @ Flatten @ Position[headerArctanHalf, "Chi2"];
colVChi2ArctanHalf = First @ Flatten @ Position[headerArctanHalf, "V-Chi2"];(*Chi2 only due to velocity, no priors, standard chi2*)
listArctanHalfChi2 = resultsArctanHalfData[[All, colChi2ArctanHalf]];
listArctanHalfVChi2 = resultsArctanHalfData[[All, colVChi2ArctanHalf]];
Echo[{Median @ listArctanHalfChi2, Median @ listArctanHalfVChi2}, "Median {Chi2, Chi2Eff}: "];
Echo[{Total @ listArctanHalfChi2, Total @listArctanHalfVChi2}, "Total {Chi2, Chi2Eff}: "];


\[Rho]brkt[rn_, rcn_, \[Rho]0_] = \[Rho]0/((1+rn/rcn)(1+rn^2/rcn^2));
Mbrkt[rn_, rcn_, \[Rho]0_] = 4 \[Pi] Rmax^3  Integrate[\[Rho]brkt[rnprime,rcn,\[Rho]0] rnprime^2,{rnprime,0,rn}, Assumptions-> {rn>0, rcn>0}];

VVbrkt[rn_, rcn_, \[Rho]0_, Rmax_] = (G / Rmax) * Mbrkt[rn, rcn, \[Rho]0] / rn;

\[Delta]Vbrkt[rn_, rcn_] = FullSimplify[
  VVbrkt[rn, rcn, \[Rho]0, Rmax] / VVbrkt[1, rcn, \[Rho]0, Rmax],
  Assumptions -> {0 < rn < 1, 0 < rcn < 1, 0 < Rmax}
];

plotBurkertGrayRed = Show[
  {
    plotBackground[1.5],
    plotSigmaRegionsRAR,
    Plot[
      {
        rn^2, (*These are the limiting curves for Burkert.*)
        1/rn
      },
      {rn, 0, 1},
      PlotRange -> All,
      PlotStyle -> {{Thickness[0.005], Black, Dashed}}
    ]
  }
];

Echo["plotBurkertGrayRed:"];
Print@plotBurkertGrayRed;


saveThisPlot = False;

Clear[chi2Upper, chi2Lower];
chi2Upper[rcn_?NumberQ, n\[Sigma]_]:= (*chi2Upper[rcn, n\[Sigma]] =*) NIntegrate[
  (upperBound[n\[Sigma]][rn] - \[Delta]Vbrkt[rn,rcn])^2, 
  {rn,rnStart,rnEnd}, 
  Method-> {Automatic, "SymbolicProcessing" -> 0},
  WorkingPrecision->10, 
  PrecisionGoal->3, 
  AccuracyGoal->Infinity, 
  MaxRecursion->10
];

chi2Lower[rcn_?NumberQ, n\[Sigma]_]:= chi2Lower[rcn, n\[Sigma]] = NIntegrate[
  (lowerBound[n\[Sigma]][rn]- \[Delta]Vbrkt[rn,rcn])^2, 
  {rn,rnStart,rnEnd}, 
  Method-> {Automatic, "SymbolicProcessing"-> 0},
  WorkingPrecision->10, 
  PrecisionGoal->3, 
  AccuracyGoal->Infinity, 
  MaxRecursion->10
];


(* SPECIFIC DEFINITIONS *)

rnStart = 0.2;
rnEnd=0.9;

lowerBound[1] = list1InterpCurvesRAR[[1]];
upperBound[1] = list1InterpCurvesRAR[[2]];
lowerBound[2] = list1InterpCurvesRAR[[3]];
upperBound[2] = list1InterpCurvesRAR[[4]];


(* EXECUTION *)

Echo["Performing the optimization."];

rcnUpper[2] = rcn /. NMinimize[{chi2Upper[rcn,2],rcn>0},{rcn,0,10}][[2]];
rcnUpper[1] = rcn /. NMinimize[{chi2Upper[rcn,1],rcn>0},{rcn,0,10}][[2]];
rcnLower[2] = rcn /. NMinimize[{chi2Lower[rcn,2],rcn>0},{rcn,0,10}][[2]];
rcnLower[1] = rcn /. NMinimize[{chi2Lower[rcn,1],rcn>0},{rcn,0,10}][[2]];

Echo[{rcnUpper[1], rcnLower[1]}, "rcn 1\[Sigma] bounds: "];
Echo[{rcnUpper[2], rcnLower[2]}, "rcn 2\[Sigma] bounds: "];

plotBurkertGlobalBestFit = Show[
  {
    plotBurkertGrayRed,
    Plot[
      {
        \[Delta]Vbrkt[rn, rcnUpper @ 2],
        \[Delta]Vbrkt[rn, rcnUpper @ 1],
        \[Delta]Vbrkt[rn, rcnLower @ 2],
        \[Delta]Vbrkt[rn, rcnLower @ 1]
      },
      {rn, 0, 1},
      PlotStyle -> {
        {Darker[Blue, 0.2], Thickness @ 0.003},
        {Lighter[Blue, 0.5], Thickness @ 0.003}
      },
      Filling -> {
        1 -> {{3}, Directive[Lighter[Blue, 0.5], Opacity @ 0.2]},
        2 -> {{4}, Directive[Lighter[Blue, 0.2], Opacity @ 0.2]}
      },
      PlotRange -> All
    ]
  }
];

Echo["plotBurkertGlobalBestFit:"];
Print @ plotBurkertGlobalBestFit;

savePreviousPlot["plotBurkertGlobalBestFit.pdf"];


efficiencyNAV[\[Delta]Vbrkt[#, rcnLower@ 1] &, \[Delta]Vbrkt[#, rcnUpper@ 1] &, 1]
efficiencyNAV[\[Delta]Vbrkt[#, rcnLower@ 2] &, \[Delta]Vbrkt[#, rcnUpper@ 2] &, 2]
Mean[{%, %%}]


saveThisPlot = False;

resultsBurkert = Get["../AuxiliaryData/Burkert-GY-05-06-MAGMAtableResults.m"]; (*These results include all 175 galaxies*)
headerBurkert = First @ resultsBurkert;
resultsBurkertData = Drop[resultsBurkert, 1];
colRc = First @ Flatten @ Position[headerBurkert, "rc"];
listRcn = DeleteCases[resultsBurkertData[[All, colRc]] / (rmax /@ Range @ 175), _/Last[{}]] // Quiet;
rectangle = {
  EdgeForm[{Lighter[Blue, 0.5], Thickness @ 0.003}],
  Lighter[Blue, 0.5],
  Opacity @ 0.2,
  Rectangle[{Log10@rcnUpper @ 1, 0}, {Log10@rcnLower @ 1, 100}]
};

Histogram[
  Log10 @ listRcn, 
  {0.2}, 
  PlotRange -> {All, All}, (*There are a few galaxies beyond 1*)
  Frame -> True, 
  Axes -> False, 
  Epilog -> {rectangle},
  histoOptions
]

savePreviousPlot["histogramBurkert.pdf"];


colChi2Burkert = First @ Flatten @ Position[headerBurkert, "Chi2"];
colVChi2Burkert = First @ Flatten @ Position[headerBurkert, "V-Chi2"];(*Chi2 only due to velocity, no priors, standard chi2*)
listBurkertChi2 = resultsBurkertData[[All, colChi2Burkert]];
listBurkertVChi2 = resultsBurkertData[[All, colVChi2Burkert]];
Echo[{Median @ listBurkertChi2, Median @ listBurkertVChi2}, "Median {Chi2, Chi2Eff}: "];
Echo[{Total @ listBurkertChi2, Total @listBurkertVChi2}, "Total {Chi2, Chi2Eff}: "];


\[Rho]nfw[rn_, rsn_, \[Rho]s_] = \[Rho]s/(rn/rsn (1+rn/rsn)^2);
Mnfw[rn_, rsn_, \[Rho]s_] = 4 \[Pi] Rmax^3  Integrate[\[Rho]nfw[rnprime, rsn, \[Rho]s] rnprime^2, {rnprime, 0, rn}, Assumptions-> {rn>0, rsn>0}];

VVnfw[rn_, rsn_, \[Rho]s_, Rmax_] = (G / Rmax) * Mnfw[rn, rsn, \[Rho]s] / rn;

\[Delta]Vnfw[rn_, rsn_] = FullSimplify[
  VVnfw[rn, rsn, \[Rho]s, Rmax] / VVnfw[1, rsn, \[Rho]s, Rmax],
  Assumptions -> {0 < rn < 1, 0 < Rmax}
];

\[Delta]VnfwLargeRsn[rn_] = Limit[\[Delta]Vnfw[rn, rsn], rsn -> \[Infinity]]
\[Delta]VnfwSmallRsn[rn_] = Limit[\[Delta]Vnfw[rn, rsn], rsn -> 0]
plotNFWGrayRed = Show[
  {
    plotBackground[1.5],
    plotSigmaRegionsRAR,
    Plot[
      {
        \[Delta]VnfwLargeRsn[rn], 
        \[Delta]VnfwSmallRsn[rn]
      },
      {rn, 0, 1},
      PlotRange -> All,
      PlotStyle -> {{Thickness[0.005], Black, Dashed}}
    ]
  }
];

Echo["plotNFWGrayRed:"];
Print@plotNFWGrayRed;


saveThisPlot = False;

Clear[chi2Upper, chi2Lower];
chi2Upper[rsn_?NumberQ, n\[Sigma]_]:= (*chi2Upper[rcn, n\[Sigma]] =*) NIntegrate[
  (upperBound[n\[Sigma]][rn] - \[Delta]Vnfw[rn,rsn])^2, 
  {rn,rnStart,rnEnd}, 
  Method-> {Automatic, "SymbolicProcessing" -> 0},
  WorkingPrecision->10, 
  PrecisionGoal->3, 
  AccuracyGoal->Infinity, 
  MaxRecursion->10
];

chi2Lower[rsn_?NumberQ, n\[Sigma]_]:= chi2Lower[rsn, n\[Sigma]] = NIntegrate[
  (lowerBound[n\[Sigma]][rn]- \[Delta]Vnfw[rn,rsn])^2, 
  {rn,rnStart,rnEnd}, 
  Method-> {Automatic, "SymbolicProcessing"-> 0},
  WorkingPrecision->10, 
  PrecisionGoal->3, 
  AccuracyGoal->Infinity, 
  MaxRecursion->10
];


(* SPECIFIC DEFINITIONS *)

rnStart = 0.2;
rnEnd = 0.9;

lowerBound[1] = list1InterpCurvesRAR[[1]];
upperBound[1] = list1InterpCurvesRAR[[2]];
lowerBound[2] = list1InterpCurvesRAR[[3]];
upperBound[2] = list1InterpCurvesRAR[[4]];


(* EXECUTION *)

Echo["Performing the optimization."];

Clear[rsnUpper, rsnLower];

rsnUpper[2] = rsn /. NMinimize[{chi2Upper[rsn,2], rsn > 0}, {rsn, 0, 1}][[2]];
rsnUpper[1] = rsn /. NMinimize[{chi2Upper[rsn,1], rsn > 0}, {rsn, 0, 1}][[2]];
rsnLower[2] = rsn /. NMinimize[{chi2Lower[rsn,2], rsn > 0}, {rsn, 10, 10000}][[2]];
rsnLower[1] = rsn /. NMinimize[{chi2Lower[rsn,1], rsn > 0}, {rsn, 10, 10000}][[2]];

Echo[{rsnUpper[1], rsnLower[1]}, "rsn 1\[Sigma] bounds: "];
Echo[{rsnUpper[2], rsnLower[2]}, "rsn 2\[Sigma] bounds: "];

plotNFWGlobalBestFit = Show[
  {
    plotNFWGrayRed,
    Plot[
      {
        \[Delta]Vnfw[rn, rsnUpper @ 2],
        \[Delta]Vnfw[rn, rsnUpper @ 1],
        \[Delta]Vnfw[rn, rsnLower @ 2],
        \[Delta]Vnfw[rn, rsnLower @ 1]
      },
      {rn, 0, 1},
      PlotStyle -> {
        {Darker[Blue, 0.2], Thickness @ 0.003},
        {Lighter[Blue, 0.5], Thickness @ 0.003}
      },
      Filling -> {
        1 -> {{3}, Directive[Lighter[Blue, 0.5], Opacity @ 0.2]},
        2 -> {{4}, Directive[Lighter[Blue, 0.2], Opacity @ 0.2]}
      },
      PlotRange -> All
    ]
  }
];

Echo["plotNFWGlobalBestFit:"];
plotNFWGlobalBestFit

savePreviousPlot["plotNFWGlobalBestFit.pdf"];


efficiencyNAV[\[Delta]Vnfw[#, rsnLower@ 1] &, \[Delta]Vnfw[#, rsnUpper@ 1] &, 1]
efficiencyNAV[\[Delta]Vnfw[#, rsnLower@ 2] &, \[Delta]Vnfw[#, rsnUpper@ 2] &, 2]
Mean[{%, %%}]


saveThisPlot = False;

resultsNFW = Get["../AuxiliaryData/NFW-GY-05-06-v2-MAGMAtableResults.m"]; (*These results include all 175 galaxies*)
headerNFW = First @ resultsNFW;
resultsNFWData = Drop[resultsNFW, 1];
colRs = First @ Flatten @ Position[headerNFW, "rS"];
resultsNFWDataRAR = Delete[resultsNFWData,  GalaxiesOutsideRAR];
listRsnRAR = resultsNFWDataRAR[[All, colRs]] / (rmax153 /@ Range @ 153);
rectangle = {
  EdgeForm[{Lighter[Blue, 0.5], Thickness @ 0.003}],
  Lighter[Blue, 0.5],
  Opacity @ 0.2,
  Rectangle[{Log10@rsnUpper @ 1, 0}, {Log10@rsnLower @ 1, 100}]
};

Histogram[
  Log10 @ listRsnRAR, 
  {0.2}, 
  PlotRange -> {All, All}, 
  Frame -> True, 
  Axes -> False, 
  Epilog -> {rectangle},
  histoOptions
]

savePreviousPlot["histogramNFW.pdf"];


colChi2NFW = First @ Flatten @ Position[headerNFW, "Chi2"];
colVChi2NFW = First @ Flatten @ Position[headerNFW, "V-Chi2"];(*Chi2 only due to velocity, no priors, standard chi2*)
listNFWChi2 = resultsNFWData[[All, colChi2NFW]];
listNFWVChi2 = resultsNFWData[[All, colVChi2NFW]];
Echo[{Median @ listNFWChi2, Median @ listNFWVChi2}, "Median {Chi2, Chi2Eff}: "];
Echo[{Total @ listNFWChi2, Total @listNFWVChi2}, "Total {Chi2, Chi2Eff}: "];


saveThisPlot = False;

Clear[a0, \[CapitalDelta]VVmodel, VVmodel, \[Delta]VVmodel];

saveThisPlot = False; (*Change this to True to save it*)

v2MondRaw[R_, gal_] := R kpc aBar[R, gal]/(1 - E^-Sqrt[RealAbs[aBar[R, gal]]/a0]);

\[CapitalDelta]v2MondRaw[R_, gal_] := v2MondRaw[R, gal] - aBar[R, gal] R kpc ;

\[Delta]v2MondRaw[rn_, gal_] := If[gdR["Rad", gal]=={}, 
  {}, 
  \[CapitalDelta]v2MondRaw[rn rmax[gal], gal] / \[CapitalDelta]v2MondRaw[rmax[gal], gal]
];

Echo[a0 = 1, "a0 = "];
Show[
  plotBackground[1.5],
  plotSigmaRegionsRAR,
  Plot[
    Evaluate[\[Delta]v2MondRaw[rn, #]& /@ Range@175], {rn,0,1}, 
    PlotStyle-> Directive[Opacity[0.1],Blue, Thick], PlotRange -> All
  ]
]

savePreviousPlot["plotdeltaVmonda01.pdf"];


saveThisPlot = False;

Echo[a0 = 1. 10^-15, "a0 = "];
Show[
  plotBackground[1.5],
  plotSigmaRegionsRAR,
  Plot[
    Evaluate[\[Delta]v2MondRaw[rn, #]& /@ Range@175], {rn,0,1},  
    PlotStyle -> Directive[Opacity[0.1], Blue, Thick], PlotRange -> All
  ]
]

savePreviousPlot["plotdeltaVmonda015.pdf"];


saveThisPlot = False;

a0 = 1.2 10^-13;
Clear @ list2\[Delta]v2MondRaw;
list2\[Delta]v2MondRaw[gal_] := If[
  gdR["Rad", gal] == {},
  {},
  Table[{rn, \[Delta]v2MondRaw[rn, gal]}, {rn, RandomReal[1,70]}]
];
list2\[Delta]v2MondRawAll = Flatten[DeleteCases[list2\[Delta]v2MondRaw /@ Range @ 175, {}], 1];

distMondRaw = distributionSilverman @ list2\[Delta]v2MondRawAll;

pdfValuenSigmaMondRaw[n_?NumberQ] := FindHDPDFValues[distMondRaw, nSigmaProbability[n]];

plotMondRawSigma[n_] := plotMondRawSigma[n] = Block[{pdfValue, contourStyle},
  pdfValue = pdfValuenSigmaMondRaw[n];
  Which[
    n == 1, contourStyle = Directive[Purple, Dashed, Thick], 
    n == 2, contourStyle = Directive[Lighter @ Purple, Dashed],
    True, Automatic
  ];
  ContourPlot[
    PDF[distMondRaw, {x,y}] == pdfValue, 
    {x, 0, 1}, {y, -1, 5},
    PerformanceGoal -> "Quality", 
    ContourStyle -> contourStyle
  ]
];

plotMondRawCurves = Plot[
  Evaluate[\[Delta]v2MondRaw[rn, #] & /@ Range@122], 
  {rn, 0, 1}, 
  PlotRange -> All, 
  PlotStyle -> Directive[Opacity[0.1],
  Blue, 
  Thick]
];

plotMondRawContours = Show[
  {plotMondRawSigma[1], 
  plotMondRawSigma[2]}
];
  
Show[
  plotBackground[1.5],
  plotSigmaRegionsRAR,
  plotMondRawCurves,
  plotMondRawContours,
  Plot[{rn, 1}, {rn, 0, 1}, PlotStyle -> {{Thickness[0.003], Orange}}]
]

savePreviousPlot["plotdeltaVmondPrincipal.pdf"];


DistributeDefinitions["NAVbaseCode`"];
DistributeDefinitions["NAVbaseCode`Private`"];

EchoTiming[
{rI[1], rD[1], rI[2], rD[2]} = Parallelize[{
  regionIntersection[plotMondRawSigma[1],1], 
  regionDifference[plotMondRawSigma[1],1],
  regionIntersection[plotMondRawSigma[2],2],
  regionDifference[plotMondRawSigma[2],2]
  }]
];

efficiencyNAV[1]
efficiencyNAV[2]
efficiencyNAVtotal[]


saveThisPlot = False;

resultsMond = Get["../AuxiliaryData/MONDRAR-Fxa0-GY-05-06-MAGMAtableResults.m"]; (*These results include 153 galaxies*)
headerMond = First @ resultsMond;
resultsMondData = Drop[resultsMond, 1];
colYDMond = First @ Flatten @ Position[headerMond, "YD"];
listYDMond = resultsMondData[[All, colYDMond]] ;

Histogram[
  Log10@listYDMond, 
  {0.05}, 
  PlotRange -> {All, All}, 
  Frame -> True, 
  Axes -> False
]

savePreviousPlot["histogramMond.pdf"];


Median[Log10@listYDMond]
Mean[Log10@listYDMond]
StandardDeviation[Log10@listYDMond]


colChi2Mond = First @ Flatten @ Position[headerMond, "Chi2"];
colVChi2Mond = First @ Flatten @ Position[headerMond, "V-Chi2"];(*Chi2 only due to velocity, no priors, standard chi2*)
listMondChi2 = resultsMondData[[All, colChi2Mond]];
listMondVChi2 = resultsMondData[[All, colVChi2Mond]];
Echo[{Median @ listMondChi2, Median @ listMondVChi2}, "Median {Chi2, Chi2Eff}: "];
Echo[{Total @ listMondChi2, Total @listMondVChi2}, "Total {Chi2, Chi2Eff}: "];


saveThisPlot = False;

resultsMondGD = Get["../AuxiliaryData/MONDRAR-Fxa0-GY-05-06-GD-MAGMAtableResults.m"]; (*These results include 153 galaxies*)
headerMondGD = First @ resultsMondGD;
resultsMondDataGD = Drop[resultsMondGD, 1];
colYDMondGD = First @ Flatten @ Position[headerMondGD, "YD"];
coldf2MondGD = First @ Flatten @ Position[headerMondGD, "df2"];
listYDMondGD = resultsMondDataGD[[All, colYDMond]] ;
listdf2MondGD = resultsMondDataGD[[All, coldf2MondGD]] ;

Histogram[
  Log10@listYDMondGD, 
  {0.05}, 
  PlotRange -> {All, All}, 
  Frame -> True, 
  Axes -> False
]

Histogram[
  Log10@listdf2MondGD, 
  {0.05}, 
  PlotRange -> {All, All}, 
  Frame -> True, 
  Axes -> False
]

savePreviousPlot["histogramMondGD.pdf"];


Median[Log10@listYDMondGD]
Mean[Log10@listYDMondGD]
StandardDeviation[Log10@listYDMondGD]


Median[Log10@listdf2MondGD]
Mean[Log10@listdf2MondGD]
StandardDeviation[Log10@listdf2MondGD]


colChi2MondGD = First @ Flatten @ Position[headerMondGD, "Chi2"];
colVChi2MondGD = First @ Flatten @ Position[headerMondGD, "V-Chi2"];(*Chi2 only due to velocity, no priors, standard chi2*)
listMondGDChi2 = resultsMondDataGD[[All, colChi2MondGD]];
listMondGDVChi2 = resultsMondDataGD[[All, colVChi2MondGD]];
Echo[{Median @ listMondGDChi2, Median @ listMondGDVChi2}, "Median {Chi2, Chi2Eff}: "];
Echo[{Total @ listMondGDChi2, Total @listMondGDVChi2}, "Total {Chi2, Chi2Eff}: "];


Clear[X, \[Rho], \[Alpha],\[Gamma],\[Beta]]
\[Alpha] = 2.94 - Log10[(10^(X + 2.33))^-1.08 + (10^(X + 2.33))^2.29];
\[Beta] = 4.23 + 1.34 X + 0.26 X^2;
\[Gamma] = - 0.06 + Log10[(10^(X + 2.56))^-0.68 + (10^(X+2.56))];
\[Rho]DC14[rn_, rsn_, \[Rho]s_, X_] = \[Rho]s/((rn/rsn)^\[Gamma] (1+ (rn/rsn)^\[Alpha])^((\[Beta]-\[Gamma])/\[Alpha]));


X100min = -410;
X100max = -130;
Xmin = X100min/100.;
Xmax = X100max/100.;

(*Clear[MDC14aux]; *) (*Uncomment this to recompute and export MDC14aux.*)
If[DownValues@MDC14aux === {}, 
  SetSharedFunction[MDC14aux];
  ParallelTable[
    MDC14aux[rn_, rsn_, \[Rho]s_, X100] = 4 \[Pi] Rmax^3 Integrate[
      \[Rho]DC14[rnprime, rsn, \[Rho]s, X100 / 100] rnprime^2, {rnprime, 0, rn}, 
      Assumptions -> {rn > 0, rsn > 0}
    ], 
    {X100, X100min, X100max, 1}
  ];
  DumpSave["../AuxiliaryData/MDC14aux.mx", MDC14aux]
];


Clear[MDC14, VVDC14, \[Delta]VDC14];

iRound[x_] = IntegerPart[Round[x, 0.01] 100];

MDC14[rn_, rsn_, \[Rho]s_, X_] := MDC14aux[rn, rsn, \[Rho]s, iRound[X]];

VVDC14[rn_, rsn_, \[Rho]s_, X_] := G/(rn Rmax) MDC14[rn, rsn, \[Rho]s, X] ;

\[Delta]VDC14[rn_, rsn_, X_] := \[Delta]VDC14[rn, rsn, X] = Simplify[
  VVDC14[rn, rsn, \[Rho]s, X]/VVDC14[1, rsn, \[Rho]s, X],
  Assumptions -> {0 <= rn <= 1,  rsn > 0, Rmax > 0}
];



saveThisPlot = False;

Clear[plotDC14GrayRed1];
plotDC14GrayRed1[X_] := 
Show[
  {
    Plot[
      {
        \[Delta]VDC14[rn,1,X], 
        \[Delta]VDC14[rn,0.1,X]
      },
      {rn, 0, 1},
      PlotRange -> All,
      PlotStyle -> {{Opacity[0.5], Thickness[0.005], ColorData[ "SolarColors"][(X - Xmin)/(Xmax - Xmin)]}}
    ]
  }
];

Show[{plotBackground[1.5],
    plotSigmaRegionsRAR,Table[plotDC14GrayRed1[X], {X, Xmin, Xmax, 0.05}]}]
    
savePreviousPlot["plotDC14SunColor.pdf"];
    


Clear[chi2Upper, chi2Lower];
chi2Upper[rsn_?NumberQ, X_, n\[Sigma]_]:= chi2Upper[rsn, X, n\[Sigma]] =  NIntegrate[
  (upperBound[n\[Sigma]][rn] - \[Delta]VDC14[rn, rsn, X])^2,
  {rn, rnStart, rnEnd}, 
  Method-> {Automatic, "SymbolicProcessing" -> 0},
  PrecisionGoal -> 4, 
  AccuracyGoal -> Infinity, 
  MaxRecursion -> 10
];

chi2Lower[rsn_?NumberQ, X_, n\[Sigma]_] := chi2Lower[rsn, X, n\[Sigma]] = NIntegrate[
  (lowerBound[n\[Sigma]][rn]- \[Delta]VDC14[rn,rsn, X])^2, 
  {rn, rnStart, rnEnd}, 
  Method-> {Automatic, "SymbolicProcessing" -> 0},
  PrecisionGoal -> 4, 
  AccuracyGoal -> Infinity, 
  MaxRecursion -> 10
];


(* SPECIFIC DEFINITIONS *)

rnStart = 0.2;
rnEnd = 0.9;

lowerBound[1] = list1InterpCurvesRAR[[1]];
upperBound[1] = list1InterpCurvesRAR[[2]];
lowerBound[2] = list1InterpCurvesRAR[[3]];
upperBound[2] = list1InterpCurvesRAR[[4]];


(* EXECUTION *)

Echo["Performing the optimization."];

ClearAll[rsnUpper, rsnLower];
rsnUpper[2] :=  {rsn, X} /. NMinimize[{chi2Upper[rsn, X, 2], 10^3>rsn>0.01, Xmin < X < Xmax}, {rsn, X}, 
  MaxIterations -> 500, 
  PrecisionGoal -> 4, 
  AccuracyGoal -> \[Infinity]
][[2]];
rsnUpper[1] :=  {rsn, X} /. NMinimize[{chi2Upper[rsn, X, 1], 10^3>rsn>0.01, Xmin < X < Xmax}, {rsn, X},
  MaxIterations -> 500, 
  PrecisionGoal -> 4, 
  AccuracyGoal -> \[Infinity]
][[2]];
rsnLower[2] :=  {rsn, X} /. NMinimize[{chi2Lower[rsn, X, 2], 10^3>rsn>0.01, Xmin < X < Xmax}, {rsn, X},
  MaxIterations -> 500, 
  PrecisionGoal -> 4, 
  AccuracyGoal -> \[Infinity]
][[2]];
rsnLower[1] :=  {rsn, X} /. NMinimize[{chi2Lower[rsn, X, 1], 10^3>rsn>0.01, Xmin < X < Xmax}, {rsn, X},
  MaxIterations -> 500, 
  PrecisionGoal -> 4, 
  AccuracyGoal -> \[Infinity]
][[2]];

{rsnUpperR[2], rsnUpperR[1], rsnLowerR[2], rsnLowerR[1]} = Parallelize[
  {rsnUpper[2], rsnUpper[1], rsnLower[2], rsnLower[1]}
];

Echo[{rsnUpperR[1], rsnLowerR[1]}, "{rsn, X} 1\[Sigma] bounds: "];
Echo[{rsnUpperR[2], rsnLowerR[2]}, "{rsn, X} 2\[Sigma] bounds: "];


saveThisPlot = False;
plotDC14GlobalBestFit = Show[
  {
    plotBurkertGrayRed /. {Dashed -> Dashing[.01], Black -> Red},
    plotNFWGrayRed /. {Dashed-> DotDashed, Black -> Orange},
    Plot[
      {
        \[Delta]VDC14[rn, Sequence@@ rsnUpperR @ 2],
        \[Delta]VDC14[rn, Sequence@@ rsnUpperR @ 1],
        \[Delta]VDC14[rn, Sequence@@ rsnLowerR @ 2],
        \[Delta]VDC14[rn, Sequence@@ rsnLowerR @ 1]
      },
      {rn, 0, 1},
      PlotStyle -> {
        {Darker[Blue, 0.2], Thickness @ 0.003},
        {Lighter[Blue, 0.5], Thickness @ 0.003}
      },
      Filling -> {
        1 -> {{3}, Directive[Lighter[Blue, 0.5], Opacity @ 0.2]},
        2 -> {{4}, Directive[Lighter[Blue, 0.2], Opacity @ 0.2]}
      },
      PlotRange -> All
    ]
  }
];

Echo["plotDC14GlobalBestFit:"];
plotDC14GlobalBestFit
savePreviousPlot["plotDC14GlobalBestFit.pdf"];


saveThisPlot = True;

minNFW = 0.5;
min = \[Delta]VDC14[0.5, 1.91, -1.6];
max = \[Delta]VDC14[0.5, 0.24, -3.76];
max2 = \[Delta]VDC14[0.5, 0.14, -3.88];


plotRegionDC14lowNFW = RegionPlot[
  \[Delta]VDC14[0.5, 10^logrsn, X] < minNFW, {logrsn, -1.2, 1.2}, {X, -4.1, -1.3}, 
  PlotPoints-> 100, 
  MaxRecursion->4, 
  Evaluate[generalOptions], 
  BoundaryStyle->None, 
  PlotStyle-> ColorData["SolarColors"][0.0]
];

plotRegionDC14low = RegionPlot[
  min > \[Delta]VDC14[0.5, 10^logrsn, X] > minNFW, {logrsn, -1.2, 1.2}, {X, -4.1, -1.3}, 
  PlotPoints-> 100, 
  MaxRecursion->4, 
  Evaluate[generalOptions], 
  BoundaryStyle->None, 
  PlotStyle-> White
];

plotRegionDC14mid = RegionPlot[
  min < \[Delta]VDC14[0.5, 10^logrsn, X] < max, {logrsn, -1.2, 1.2}, {X, -4.1, -1.3}, 
  PlotPoints-> 100, 
  MaxRecursion->4, 
  Evaluate[generalOptions], 
  BoundaryStyle->None, 
  PlotStyle-> ColorData["SolarColors"][0.5]
];

plotRegionDC14high = RegionPlot[
  max2 > \[Delta]VDC14[0.5, 10^logrsn, X] > max, {logrsn, -1.2, 1.2}, {X, -4.1, -1.3}, 
  PlotPoints-> 100, 
  MaxRecursion->4, 
  Evaluate[generalOptions], 
  BoundaryStyle->None, 
  PlotStyle-> ColorData["SolarColors"][0.99]
];

plotRegionDC14high2 = RegionPlot[
  \[Delta]VDC14[0.5, 10^logrsn, X] > max2, {logrsn, -1.2, 1.2}, {X, -4.1, -1.3}, 
  PlotPoints-> 100, 
  MaxRecursion->4, 
  Evaluate[generalOptions], 
  BoundaryStyle->None, 
  PlotStyle-> White
];

Show[
  plotRegionDC14high, 
  plotRegionDC14mid, 
  plotRegionDC14low, 
  plotRegionDC14high2, 
  plotRegionDC14lowNFW, 
  AspectRatio -> 1/GoldenRatio
]

savePreviousPlot["plotRegionsDC14.pdf"];


efficiencyNAV[\[Delta]VDC14[#,Sequence@@ rsnLowerR@ 1] &, \[Delta]VDC14[#, Sequence@@rsnUpperR@ 1] &, 1]
efficiencyNAV[\[Delta]VDC14[#, Sequence@@rsnLowerR@ 2] &, \[Delta]VDC14[#, Sequence@@rsnUpperR@ 2] &, 2]
Mean[{%, %%}] 



saveThisPlot = False;

SmoothHistogram[{
  Log10@listBurkertVChi2, 
  Log10@listArctanVChi2, 
  Log10@listNFWVChi2, 
  Log10@listArctanHalfVChi2,
  Log10@listMondGDVChi2,
  Log10@listMondVChi2
},
  0.000001, "CDF", 
  PlotRange->{{-2, 4}, {-0.05, 1.05}}, Frame -> True, Axes->False,
  PlotLegends->Placed[{"Burkert", "\!\(\*SubscriptBox[\(Arctan\), \(\[Alpha] = 1\)]\)", "NFW", "\!\(\*SubscriptBox[\(Arctan\), \(\[Alpha] = 1/2\)]\)", "\!\(\*SubscriptBox[\(MOND\), \(\(dist\)\(.\)\)]\)", "MOND"}, {Left, Center}],   
  PlotStyle-> {{Thickness[0.01], Opacity[0.6]}},
  PlotTheme->{"Scientific", "Frame", "BoldColor"},
  AspectRatio-> 1/GoldenRatio,
  Evaluate[generalOptions]
]

savePreviousPlot["plotCDFcomparison.pdf"];



