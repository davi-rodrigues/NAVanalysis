(* ::Package:: *)

(* ::Title:: *)
(*NAVoptions*)
(*Based on SetOptions5a.wl by D.C.Rodrigues*)


(* ::Subtitle:: *)
(*Davi C. Rodrigues*)


(* ::Text:: *)
(*Requires the CustomTicks package.*)


Needs["CustomTicks`"]

(* Shortcuts *)
  TF = TableForm;
  MF = MatrixForm;

(* Ticks options *)

  SetOptions[LinTicks, 
    MajorTickLength->{0.015, 0.015},
    MinorTickLength->{0.010, 0.010},  
    MajorTickStyle-> Thickness[0.0030],
    MinorTickStyle-> Thickness[0.0015] 
  ]; (* depends on CustomTicks.m *)
  
  ClearAll[LinTicks2];
  Options[LinTicks2] = Options[LinTicks];
  LinTicks2[x__] := LinTicks[x, 
    MajorTickLength->{0.015, 0},
    MinorTickLength->{0.010, 0},  
    MajorTickStyle-> Thickness[0.0030],
    MinorTickStyle-> Thickness[0.0015]
  ];

  SetOptions[LogTicks, 
    MajorTickLength->{0, 0.020},
    MinorTickLength->{0, 0.010}, 
    MajorTickStyle-> Thickness[0.0020],
    MinorTickStyle-> Thickness[0.0015] 
  ]; (* Depends on CustomTicks.m *)



  plotstyleoptions[thickness_] =  {
    Directive[GrayLevel[0],Thickness[thickness]],
    Directive[RGBColor[0.34398, 0.49112, 0.89936],Thickness[thickness]], 
    Directive[RGBColor[0.97, 0.606, 0.081],Thickness[thickness]], 
    Directive[RGBColor[0.448, 0.69232, 0.1538],Thickness[thickness]],
    Directive[RGBColor[0.62168, 0.2798, 0.6914],Thickness[thickness]],
    Directive[RGBColor[0.91, 0.318, 0.243],Thickness[thickness]],
    Directive[GrayLevel[0],Thickness[thickness]] (*It is important that the last color is equal to the first one*)
  };

  colorplot[i_] :=  plotstyleoptions[0.002][[i,1]]; (*Calls the i-th color used in plotstyleoptions*)

  plotMarkersOptions =  Graphics[{colorplot @ #, Circle[]}, ImageSize -> 12] & /@ Range[7];

(* General options *)
  
  generalOptions = {
    PlotRangePadding -> Scaled[0.001],
    Frame-> True, 
    Axes -> False, 
    (*FrameStyle -> Scaled[0.02], *)
    GridLinesStyle -> Directive[Dashed, LightGray, Thickness[0.0001]], (* Use GridLines -> Auto, for instance, to show them*)
    LabelStyle -> {FontFamily -> "Times", FontSize -> 15}, (*The font size is fixed.*)
    FrameTicks -> {{LinTicks, StripTickLabels@LinTicks2}, {LinTicks, StripTickLabels@LinTicks2}},
    AspectRatio -> 1
  };

(* The new functions *)

  ClearAll[nPlot];
  SetAttributes[nPlot, HoldFirst];
  Options[nPlot] = Options[Plot];
  SetOptions[nPlot, Sequence@@generalOptions, PlotStyle -> plotstyleoptions[0.005]];
  nPlot[args__, opts:OptionsPattern[]] := Plot[args, opts, Evaluate@Options[nPlot]];

  ClearAll[nListPlot];
  Options[nListPlot] = Options[ListPlot];
  SetOptions[nListPlot, Sequence@@generalOptions, PlotMarkers -> plotMarkersOptions];
  nListPlot[args__, opts:OptionsPattern[]] := ListPlot[args, opts, Evaluate@Options[nListPlot]];

  ClearAll[nParametricPlot];
  SetAttributes[nParametricPlot, HoldFirst];
  Options[nParametricPlot] = Options[ParametricPlot];
  SetOptions[nParametricPlot, Sequence@@generalOptions, PlotStyle -> plotstyleoptions[0.005]];
  nParametricPlot[args__, opts:OptionsPattern[]] := ParametricPlot[args, opts, Evaluate@Options[nParametricPlot]];

(* ToCode definition*)
  DeclarePackage["GeneralUtilities`", "ToCode"];
  SetAttributes[ToCode, Attributes@HoldPrettyForm];
  ToCode[x_] := Block[
    {replaceTabs, replacePatterns},
    replaceTabs = Replace[#, s_String :> StringReplace[s, "\t" -> "  "], All] &;
    replacePatterns = ReplaceAll[#, InterpretationBox[string_String, somethingelse__] :>  string] & ;
    Print[RawBoxes@replaceTabs@replacePatterns@ToBoxes@HoldPrettyForm[x]];
    SelectionMove[SelectedNotebook[], Next, GeneratedCell];
    FrontEndExecute@FrontEndToken[SelectedNotebook[], "Style", "Code"];
  ];
