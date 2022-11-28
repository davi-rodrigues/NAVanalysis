
(* 
  TITLE: NAVauxiliaryFunctions 
  TYPE: auxiliary fle to NAVanalysis code.
  AUTHOR: Davi C Rodrigues
*)

(* 
  PURPOSE: 
  Specific functions for dealing with galaxy data that develops further NAVanalysisBaseCode and NAVobservational functions.
  This is a natural file to add new model-independent functions.
  Model dependent functions are expected to be defined in the NAVanalysis notebook. 

  NOTES:
  phiExp and vExp come from Binney & Tremaine 2nd Ed., eq.(2.164a)
*)

Clear[rmax, rmin]; 
rmax[gal_] := Last[gdR["Rad", gal]]; (*gal here is the galaxy number for the complete sample with 175 galaxies, galaxies outside the RAR return {}. *)
rmin[gal_] := First[gdR["Rad", gal]];
rmax122[gal_] := datasetExpVdiskNoBulge[gal, "rMax"]; (* same as rmax, but for the sample with 122 galaxies.*)
rmax153Table = DeleteCases[
  rmax /@ Range @ 175, 
  Last[{}]
] // Quiet;
rmax153[gal_] := rmax153Table[[gal]]; (* same as rmax, but for the sample with 122 galaxies.*)

listVBar[gal_] := gdR[{"Rad", "Vbar"}, gal] // Prepend[#, {0, 0}] &; (*Includes YD contribution, as set in NAVbaseCode.*)
vBar[R_, gal_] := Interpolation[listVBar[gal], Method -> "Spline", InterpolationOrder -> 2][R]; (*Baryonic circular velocity*)

listABar[gal_] := {#1, squareSign[#2]/ (kpc #1)} & @@@ gdR[{"Rad", "Vbar"}, gal] // Prepend[#, {0, 0}] & ;
aBar[R_, gal_] := Interpolation[listABar[gal], Method -> "Spline", InterpolationOrder -> 2][R]; (*Baryonic acceleration*)

vExp[R_, logSigma0_, h_]= Block[{y}, 
  y = R/(2 h);
  kpc Sqrt[4 \[Pi] G0 10^logSigma0 h y^2 (BesselI[0,y] BesselK[0,y] - BesselI[1,y] BesselK[1,y])]
];
phiExp[R_, logSigma0_, h_] = Block[{y}, 
  y = R/(2 h);
  - 4 \[Pi] G0 10^logSigma0 R (BesselI[0,y] BesselK[1,y] - BesselI[1,y] BesselK[0,y])
];

hExpStar[gal_] := datasetExpVdiskNoBulge[gal, "h"];
logSigma0ExpStar[gal_] :=  Log10[YDcentral] + datasetExpVdiskNoBulge[gal, "logSigma0"]; (*The exponential results include use YDcentral=1, hence the correction here.*)
hExpGas[gal_] := datasetExpVgasNoBulge[gal, "hGas"];
logSigma0ExpGas[gal_] := datasetExpVgasNoBulge[gal, "logSigma0Gas"];

sigmaStarExp[R_, gal_] := 10^logSigma0ExpStar[gal] Exp[-R / hExpStar[gal]]; 
sigmaGasExp[R_, gal_] := 10^logSigma0ExpGas[gal] Exp[-R / hExpGas[gal]];
sigmaBarExp[R_, gal_] := sigmaStarExp[R, gal] + sigmaGasExp[R, gal];
sigmaBarExpf[R_, gal_, fgas_] := sigmaStarExp[R, gal] + fgas sigmaGasExp[R, gal];

vBarExp[R_, gal_] := Sqrt[
  vExp[R, logSigma0ExpStar[gal], hExpStar[gal]]^2+ vExp[R, logSigma0ExpGas[gal], hExpGas[gal]]^2
];
phiBarExp[R_, gal_] := phiExp[R, logSigma0ExpStar[gal], hExpStar[gal]] + phiExp[R, logSigma0ExpGas[gal], hExpGas[gal]];

massExpStar[gal_, rEnd_] := 2 \[Pi] sigmaStarExp[0, gal] hExpStar[gal] (hExpStar[gal] - E^(-(rEnd/hExpStar[gal])) (hExpStar[gal]+rEnd) ); (*From Integrate[2 \[Pi] \[CapitalSigma]0 \[ExponentialE]^(-r/h) r, {r,0, rEnd}]*)
massExpStar[gal_] := massExpStar[gal, rmax122[gal]];
massExpInftyStar[gal_] := 2 \[Pi] sigmaStarExp[0, gal] hExpStar[gal]^2 ; (*From Integrate[2 \[Pi] \[CapitalSigma]0 \[ExponentialE]^(-r/h) r, {r,0, \[Infinity]}]*)

massExpGas[gal_, rEnd_] := 2 \[Pi] sigmaGasExp[0, gal] hExpGas[gal] (hExpGas[gal] - E^(-(rEnd/hExpGas[gal])) (hExpGas[gal]+rEnd) ); 
massExpInftyGas[gal_] := 2 \[Pi] sigmaGasExp[0, gal] hExpGas[gal]^2 ; (*From Integrate[2 \[Pi] \[CapitalSigma]0 \[ExponentialE]^(-r/h) r, {r,0, \[Infinity]}]*)
massExpGas[gal_] := massExpGas[gal, rmax122[gal]];

massExpInftyBar[gal_] := massExpInftyStar[gal] + massExpInftyGas[gal];
massExpBar[gal_] :=  massExpStar[gal] + massExpGas[gal];
massExpBar[gal_, rEndStar_, rEndGas_] := massExpStar[gal, rEndStar] + massExpGas[gal, rEndGas];
massExpBar[gal_, rEndStar_, rEndGas_, fgas_] := massExpStar[gal, rEndStar] + fgas massExpGas[gal, rEndGas];

(*The definition below depends on rI and rD, which are defined, for each model, 
in their corresponding NAV efficiency section *)
efficiencyNAV[nSigma_] := (Area @ rI[nSigma] - Area @ rD[nSigma])/areaSigma @ plotObsSigma[nSigma];
efficiencyNAVtotal[] := Mean[{efficiencyNAV[1], efficiencyNAV[2]}];
