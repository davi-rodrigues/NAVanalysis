(* ::Package:: *)

(* ::Chapter:: *)
(*Observational NAV *)


(* ::Text:: *)
(*To generate the plots, uncomment the proper lines below.*)


(* ::Section:: *)
(*I.1. Plots for Kernel Density Estimation for the distribution*)

  
Print["Starting NAVobservational..."];

Clear[plotBlueZero, statusKDE];

exportPlotObservational[fileName_, plotToExport_] := If[Global`saveNAVobservationalPlots, 
  Block[{fileNameWithPath},
    fileNameWithPath = FileNameJoin[{pathOutputDirectory, ToString @ fileName}];
    Export[fileNameWithPath, plotToExport];
    Echo[fileNameWithPath, "Exported"]
  ]
];

exportTableObservational[fileName_, plotToExport_] := If[Global`saveNAVobservationalTables, 
  Block[{fileNameWithPath},
    fileNameWithPath = FileNameJoin[{pathOutputDirectory, ToString @ fileName}];
    Export[fileNameWithPath, plotToExport];
    Echo[fileNameWithPath, "Exported"]
  ]
];


plotBlueZero[dataForKDE_, opts:OptionsPattern[]] := nListPlot[
  {dataForKDE}, 
  PlotMarkers -> {
    Graphics[{Darker[Blue],Opacity[0.05],Disk[]}, ImageSize -> 10], 
    Graphics[{Darker[Red],Opacity[0.3],Disk[]}, ImageSize -> 10]
  },
  opts,
  PlotRange -> {{0,1}, All},
  AspectRatio -> 1
];

statusKDE[dataForKDE_, {{xmin_, xmax_}, {ymin_, ymax_}}]:= Block[{},
  Echo[silvermanBw[dataForKDE], 
    "Gaussian bandwidths from Silverman rule of thumb: "
  ];
  Print@GraphicsRow[
    {
      plotBlueZero[dataForKDE], 
      ContourPlot[PDF[distributionSilverman[dataForKDE], {x, y}], {x, xmin, xmax}, {y, ymin, ymax}]
    }
  ]
];


(* SPECIFIC DEFINITIONS *)

Clear[gdRList, gdRListFlat];

gdRList[gdFunction_] := gdRList[gdFunction] = gdFunction[{nRad, \[Delta]Vms}][[#]] & /@ Range[175];
gdRListFlat[gdFunction_] := Flatten[gdRList[gdFunction], 1];

EchoTiming[
  list2RARRot = gdRListFlat @ gdR;
  list2RARRotNoBulge = gdRListFlat @ gdRBulgeless; , "list2RARRot"
];

(*EXECUTION*)
EchoTiming[
  distRARRot = distributionSilverman[list2RARRot];
  distRARRotNoBulge = distributionSilverman[list2RARRotNoBulge]; , "distRAR"
];

(*Echo["All RAR galaxies case:"];
statusKDE[list2RARRot, {{0, 1}, {-0.5, 1.5}}];
Echo["Only the bulgeless RAR galaxies:"];
statusKDE[list2RARRotNoBulge, {{-0.01,1}, {-0.5, 1.5}}];*)



(* ::Section:: *)
(*I.2. The 1\[Sigma] and 2\[Sigma] Highest Density Regions*)


Clear[plotBlue, plotSigmaContours, plotBlueFunction];
(*list1LimitingPDFValues::usage = 
  "limitingPDFValues[data, {{xmin, xmax, xstep}, {ymin, ymax, ystep}}] provides {pdf1Sigma, pdf2sigma}}, \n " <>
  "where pdf1Sigma and pdf2Sigma are respectively the pdf values that limit the 1 or 2 Sigma highest denstiy region found from the KDE of the provided data. \n"<>
  "xmin, xmax, ymin and ymax set the region to be considered, while xstep and ystep the step size to be used to explore the distribution.";
*)  

nSigmaProbability[n_?NumberQ] := nSigmaProbability[n] = NProbability[Less[-n, x, n], Distributed[x, NormalDistribution[]]];
oneSigmaProbability = nSigmaProbability[1];
twoSigmaProbability = nSigmaProbability[2];  
  
FindHDPDFValues::usage = "FindHDPDFValues[dist, probability] yields the lowest PDF value that delimits the Highest Density Probability region, if the variable probability is a number. If the variable probability is a list of numbers, the result is the same of acting FindHDPDFValues on each of the list values. FindHDPDFValues works for DataDistribution of any dimensions.";

FindHDPDFValues[dist_DataDistribution, probability_?NumberQ] := Block[
  {pdfList, pdfSortList, cdfSortList, positionAtCdf, positionsAtPdf},
  pdfList = dist["PDFValues"];
  pdfSortList = Reverse @ Sort @ pdfList;
  cdfSortList = Accumulate[pdfSortList] / Total[pdfSortList];
  positionAtCdf[prob_] := FirstPosition[cdfSortList, p_ /; p >= prob];
  positionsAtPdf = Flatten[positionAtCdf @  probability];
  First @ pdfSortList[[positionsAtPdf]]
];

FindHDPDFValues[dist_DataDistribution, probability_List] := FindHDPDFValues[dist, #] & /@ probability;

plotSigmaContours[dataForContours_, limitingPdfValues_, {{xmin_, xmax_}, {ymin_, ymax_}}, options___]:= Block[
  {pdf}, 
  pdf[x_,y_] = PDF[distributionSilverman[dataForContours], {x, y}]; 
  ContourPlot[
    pdf[x, y],
    {x, xmin, xmax},
    {y, ymin, ymax},
    Contours -> limitingPdfValues, (*Can be the output from FindHDPDFValues*)
    ContourShading -> None, 
    ContourStyle -> {
      {Thickness[0.003], Lighter[Gray, 0.2]},
      {Thickness @ 0.005, Gray}
    },
    options
  ]
];

plotBlue[dataForBluePlot_, limitingPdfValeus_, {{xmin_, xmax_}, {ymin_, ymax_}}, options___] := Show[
  plotBlueZero[dataForBluePlot, options],
  plotSigmaContours[dataForBluePlot, limitingPdfValeus, {{xmin, xmax}, {ymin, ymax}}, PlotRange -> All]
];

(* SPECIFIC DEFINITIONS *)

ymin = -1.5;
ymax = 2.5;
xmin= 0;
xmax = 1;
xstep = 0.005;
ystep = 0.005;

oneAndTwoSigma = {oneSigmaProbability, twoSigmaProbability};
list1Limits = FindHDPDFValues[distRARRot, oneAndTwoSigma];
list1LimitsNoBulge = FindHDPDFValues[distRARRotNoBulge, oneAndTwoSigma];

plotBlueFunction = plotBlue[Sequence@@#, {{xmin, xmax - 0.001}, {-0.5, 1.5}}, PlotRange -> {{0, 1}, {-1, 2}}] & ;

(* EXECUTION *)

EchoTiming[
  {plotBlueRAR, plotBlueRARNoBulge} = plotBlueFunction /@ {{list2RARRot, list1Limits}, {list2RARRotNoBulge, list1LimitsNoBulge}}; , "plotBlueRAR"
];

Clear[plotObsSigma];
plotObsSigma[n_] := plotObsSigma[n] = Block[{pdfValue},
  pdfValue = FindHDPDFValues[distRARRotNoBulge, nSigmaProbability[n]];
  ContourPlot[
    PDF[distRARRotNoBulge, {x,y}] == pdfValue, 
    {x, 0, 1}, {y, -0.5, 2.5},
    PerformanceGoal -> "Quality"
  ]
];


exportPlotObservational["plotBlueRAR.pdf", plotBlueRAR];

exportPlotObservational["plotBlueRARNoBulge.pdf", plotBlueRARNoBulge];


(* ::Text::GrayLevel[0]::Bold::Closed:: *)
(*Cell group comment: *)
(*Considers the removal of Vobs data points with uncertainty larger than 10%.*)
(*Conclusion: no relevant impact and adds complications*)


(* ::Text:: *)
(*Case with Bulge:*)


(* ::Input:: *)
(*Clear[gdRListVel, gdRListVelFlat]*)
(*gdRListVel[gdFunction_] := gdRListVel[gdFunction] = gdFunction[{nRad, \[Delta]Vms, Vobs, \[Delta]Vobs}][[#]] & /@ Range[175];*)
(*gdRListVelFlat[gdFunction_] := Flatten[gdRListVel[gdFunction], 1];*)
(**)
(*list2RARRotVelAux = Select[gdRListVelFlat[gdR], #[[4]]/#[[3]] <  0.1 &] ; (*Includes only the velocity points with uncertainty lower than 10% of Vobs*)*)
(*list2RARRotVel = Drop[list2RARRotVelAux\[Transpose], -2]\[Transpose]; (*Removes the columns on Vobs and \[Delta]Vobs*)*)
(**)
(*distRARRotVel = distributionSilverman[list2RARRotVel];*)
(**)
(*list1LimitsVel = FindHDPDFValues[distRARRotVel, oneAndTwoSigma];*)
(**)
(*plotBlueRARVel = plotBlueFunction[{list2RARRotVel, list1LimitsVel}] ; *)
(*Show[plotBlueRARVel, plotBlueRAR]*)


(* ::Text:: *)
(*Case without Bulge*)


(* ::Input:: *)
(**)
(*list2RARRotVelAuxNoBulge = Select[gdRListVelFlat[gdRBulgeless], #[[4]]/#[[3]] <  0.1 &] ; (*Includes only the velocity points with uncertainty lower than 10% of Vobs*)*)
(*list2RARRotVelNoBulge = Drop[list2RARRotVelAuxNoBulge\[Transpose], -2]\[Transpose]; (*Removes the columns on Vobs and \[Delta]Vobs*)*)
(**)
(*distRARRotVelNoBulge = distributionSilverman[list2RARRotVelNoBulge];*)
(**)
(*list1LimitsVelNoBulge = FindHDPDFValues[distRARRotVelNoBulge, oneAndTwoSigma];*)
(**)
(*plotBlueRARVelNoBulge = plotBlueFunction[{list2RARRotVelNoBulge, list1LimitsVelNoBulge}] ; *)
(*Show[plotBlueRARVelNoBulge, plotBlueRARNoBulge]*)


(* ::Text:: *)
(*The plot below shows that the  excluded points most dense region is close to the galaxy center (as expectected, since there the observational velocity is typically close to zero, while the Vobs errors do not go to zero).*)


(* ::Input:: *)
(*ListPlot[{ list2RARRot, list2RARRotVel}, PlotStyle-> {Blue, Red}]*)


(* ::Text:: *)
(*The main differences appear in the lower bounds of the 1\[Sigma] and 2\[Sigma] regions, which get raised, in particular at Subscript[r, n]<0.2. The overall impact for our results is negligible. Since most of the excluded data points are close to the galactic center, in the bulgeless case the 1\[Sigma] upper and lower limits get connected, which adds a nonnecessary computational complication and such case is artificial, since it depends on the choice of the 10% rejection limit). *)


Clear[list1InterpSigmaCurves, plotSigmaRegions];
list1InterpSigmaCurves::usage = "list1InterpSigmaCurves[blueplot] provides a list of 4 interpolated curves in the following order:"<> 
    "{1\[Sigma]LowerLimit, 1\[Sigma]UpperLimit, 2\[Sigma]LowerLimit, 2\[Sigma]UpperLimit}. /n"<>
    "The plot to be provided (blueplot) can be generated by the function plotBlue.";
list1InterpSigmaCurves[plotblue_] := Block[
  {
    sigmaContoursList,
    curveSigma, 
    curveOrder, 
    curve2SigmaNeg, 
    curve1SigmaNeg, 
    curve1SigmaPlus, 
    curve2SigmaPlus
  },
  sigmaContoursList = Cases[
    Normal @ FullForm @ First @ plotblue,
    Line[pts_] :> pts,
    Infinity
  ];
  curveSigma[n_] := Interpolation[
    Sort@sigmaContoursList[[n]],
    Method -> "Spline", 
    InterpolationOrder-> 2
  ];
  curveOrder = Ordering @ Table[curveSigma[n][0.5], {n, 1, 4}];
  curve2SigmaNeg = curveSigma[curveOrder[[1]]];
  curve1SigmaNeg = curveSigma[curveOrder[[2]]];
  curve1SigmaPlus = curveSigma[curveOrder[[3]]];
  curve2SigmaPlus = curveSigma[curveOrder[[4]]];
  {
    curve1SigmaNeg,
    curve1SigmaPlus,
    curve2SigmaNeg, 
    curve2SigmaPlus
  }
];

plotSigmaRegions::usage = 
  "plotSigmaRegions[curvesSigma, xLimit1, xLimit2] yields a plot with the 1 and 2 sigma regions from xLimit1 to xLmit2. \n" <>
  "The standard values of xLimit1 and xLimit2 are 0.2 and 0.9. \n" <>
  "The input to be provided, curvesSigma, can be generated by list1InterpSigmaCurves.";
  
plotSigmaRegions[curvesSigma_, xLimit1_:0.01, xLimit2_:0.99] := Plot[
  {
    curvesSigma[[3]][r],
    curvesSigma[[1]][r],
    curvesSigma[[2]][r],
    curvesSigma[[4]][r]
  },
  {r, xLimit1, xLimit2},
  Filling -> {{1 -> {3}, GrayLevel[0.7]}, {2 -> {4}}},
  PlotStyle -> {{Thickness[0.002], Gray}, {Thickness @ 0.002, Gray}},
  FillingStyle -> {GrayLevel[0.7], Opacity @ 0.05}
];

(*EXECUTION*)

EchoTiming[
  {plotSigmaRegionsRAR, plotSigmaRegionsRARNoBulge} = plotSigmaRegions[list1InterpSigmaCurves[#]] & /@ {plotBlueRAR, plotBlueRARNoBulge};, "plotSigmaRegions"
];

plotBackground[upperLimit_] := nPlot[
  100,
  {rn, 0, 1},
  PlotRange -> {{0, 1}, {-0.25, upperLimit}},
  AspectRatio -> (1 / 1.3),
  GridLines -> None, 
  Prolog -> {
    Dotted,
    Line @ {{0, 0}, {1, 0}},
    Gray,
    Opacity[0.2], 
    Rectangle[{0.001, -0.5},{0.2, upperLimit}],
    Rectangle[{0.9, -0.5},{0.999, upperLimit}]
  }
];

(*GraphicsRow[
  {plotSigmaRegionsRAR, plotSigmaRegionsRARNoBulge},
  ImageSize \[Rule] 800
]*)


(* ::Subsection:: *)
(*I.2.1. Exporting the \[Sigma] regions into a almost-paper-ready table (uncomment to export the files)*)


Clear[exportSigmaList];
exportSigmaList::usage = "curvesSigmaList[listOfCurves, rnMin, rnMax, step] generates 1\[Sigma] and 2\[Sigma] limiting curves in a close-to-paper-ready formated table, /n"<>
  "with rMin < r < rMax, in steps provided by step (optional, 0.02 being standard). The listOfCurves can be generated by list1InterpSigmaCurves./n"<>
  "exportSigmaList does not exports any file, but prepares the list to be exported.";

exportSigmaList[list1IpSigmaCurves_, rnMin_, rnMax_, step_:0.025] := Block[
  {header, tab, rn, nF, tabAux},
  header = {"# rn", "1\[Sigma]Lower", "1\[Sigma]Upper", "2\[Sigma]Lower", "2\[Sigma]Upper"};
  nF := NumberForm[#, {4, 3}] &;
  tabAux = Table[list1IpSigmaCurves[[n]]@rn, {n, 4}];
  tab = Table[
    nF /@ Flatten[{rn, tabAux}],
    {rn, rnMin, rnMax, step} 
  ];
  Join[{header}, tab]
];

list1InterpCurvesRAR = list1InterpSigmaCurves[plotBlueRAR];
list1InterpCurvesRARNoBulge = list1InterpSigmaCurves[plotBlueRARNoBulge];

exportTableObservational["sigmaRegionsRAR.csv", exportSigmaList[list1InterpCurvesRAR, 0.025, 1]];
exportTableObservational["sigmaRegionsRARNoBulge.csv", exportSigmaList[list1InterpCurvesRARNoBulge, 0.025, 1]];


