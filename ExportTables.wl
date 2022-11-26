(* ::Title:: *)
(*Export individual fits as tsv files*)


(* ::Author:: *)
(*Davi C. Rodrigues*)
(*Nov/ 2022*)


(* ::Section:: *)
(*Prelude*)


pathBaseDirectory = NotebookDirectory[];
pathOutputDirectory = FileNameJoin[{pathBaseDirectory, "Output"}];
pathAuxDirectory = FileNameJoin[{pathBaseDirectory, "AuxiliaryData"}];

SetDirectory @ pathBaseDirectory;

Block[{Print}, Needs @ "NAVbaseCode`"];
Get @ "NAVauxiliaryFunctions.wl";

SetDirectory @ pathOutputDirectory;


(* ::Section:: *)
(*Definitions*)


exportNFWtable := Block[
  {
    results, header, resultsData, resultsDataRAR, colRs, colRho, colYD, colYB, 
    colChi2eff, colChi2, colN, dataExportAux0, listRsn, dataExportAux1, dataExport
  },
    
  results = Get[FileNameJoin[{pathAuxDirectory, "NFW-GY-05-06-v2-MAGMAtableResults.m"}]];
  header = First @ results;
  resultsData = Drop[results, 1];
  resultsDataRAR = Delete[resultsData,  GalaxiesOutsideRAR]; (*153 galaxies*)

  colRs = First @ Flatten @ Position[header, "rS"];
  colRho = First @ Flatten @ Position[header, "logRhoS"];
  colYD = First @ Flatten @ Position[header, "YD"];
  colYB = First @ Flatten @ Position[header, "YB"];
  colChi2eff = First @ Flatten @ Position[header, "Chi2"];
  colChi2 = First @ Flatten @ Position[header, "V-Chi2"];
  colN = First @ Flatten @ Position[header, "N.data points"];

  dataExportAux0 = resultsDataRAR[[All, {1, colRs, colRho, colYD, colYB, colChi2, colChi2eff, colN}]];
  listRsn = resultsDataRAR[[All, colRs]] / (rmax153 /@ Range @ 153);
  dataExportAux1 = Join[dataExportAux0\[Transpose], {listRsn\[Transpose]}]\[Transpose]; (* dataExportAux0 with a column for Rsn*)
  dataExport = dataExportAux1[[All, {1, 2, 9, 3, 4, 5, 6, 7, 8}]]; (*Reorder the columns*)
  PrependTo[dataExport, {"Galaxy", "rs", "rsn", "log rhos", "YD", "YB", "Chi2", "Chi2eff", "N.Data"}];
  
  Export["dataExportAux.tsv", dataExport, Alignment-> Left, "TextDelimiters"-> None];
  Export["headerAux.txt", 
    {
      "# Normalized additional velocity distribution: ",
      "#   testing the radial profile of dark matter halos and MOND.",
      "# by D. C. Rodrigues, A. Hernandez-Arboleda, A. Wojnar",
      "# ",
      "# NFW individual fits for 153 SPARC galaxies with Gaussian priors on YD and YB.",
      "# 1st column: galaxy name.",
      "# 2nd column: scale radius (rs) [kpc].",
      "# 3rd column: normalized scale radius (rsn = rs / rmax).",
      "# 4th column: logarithm of the density rho_s (log rhos) [Msun / kpc^3].",
      "# 5th column: stellar disk mass-to-light ratio (YD).",
      "# 6th column: stellar bulge mass-to-light ratio (YB).",
      "# 7th column: minimum chi-squared value (Chi2) (- 2 log likelihood + constant).",
      "# 8th column: minimum effective chi-squared value (- 2 log posterior + constant).",
      "# 9th column: number of data points for the galaxy circular velocity (N.Data).", 
      "# ",
      "# "
    }
  ];
  Run["cat headerAux.txt dataExportAux.tsv > NFWindividualFits.tsv"];
  DeleteFile["headerAux.txt"];
  DeleteFile["dataExportAux.tsv"];
];



exportBurkertTable := Block[
  {
    results, header, resultsData, resultsDataRAR, colR, colRho, colYD, colYB, 
    colChi2eff, colChi2, colN, dataExportAux0, listRsn, dataExportAux1, dataExport
  },
    
  results = Get[FileNameJoin[{pathAuxDirectory, "Burkert-GY-05-06-MAGMAtableResults.m"}]];
  header = First @ results;
  resultsData = Drop[results, 1];
  resultsDataRAR = Delete[resultsData,  GalaxiesOutsideRAR]; (*153 galaxies*)

  colR = First @ Flatten @ Position[header, "rc"];
  colRho = First @ Flatten @ Position[header, "logRhoc"];
  colYD = First @ Flatten @ Position[header, "YD"];
  colYB = First @ Flatten @ Position[header, "YB"];
  colChi2eff = First @ Flatten @ Position[header, "Chi2"];
  colChi2 = First @ Flatten @ Position[header, "V-Chi2"];
  colN = First @ Flatten @ Position[header, "N.data points"];

  dataExportAux0 = resultsDataRAR[[All, {1, colR, colRho, colYD, colYB, colChi2, colChi2eff, colN}]];
  listRsn = resultsDataRAR[[All, colR]] / (rmax153 /@ Range @ 153);
  dataExportAux1 = Join[dataExportAux0\[Transpose], {listRsn\[Transpose]}]\[Transpose]; (* dataExportAux0 with a column for Rsn*)
  dataExport = dataExportAux1[[All, {1, 2, 9, 3, 4, 5, 6, 7, 8}]]; (*Reorder the columns*)
  PrependTo[dataExport, {"Galaxy", "rc", "rcn", "log rhos", "YD", "YB", "Chi2", "Chi2eff", "N.Data"}];
  
  Export["dataExportAux.tsv", dataExport, Alignment-> Left, "TextDelimiters"-> None];
  Export["headerAux.txt", 
    {
      "# Normalized additional velocity distribution: ",
      "#   testing the radial profile of dark matter halos and MOND.",
      "# by D. C. Rodrigues, A. Hernandez-Arboleda, A. Wojnar",
      "# ",
      "# Burkert individual fits for 153 SPARC galaxies with Gaussian priors on YD and YB.",
      "# 1st column: galaxy name.",
      "# 2nd column: core radius (rc) [kpc].",
      "# 3rd column: normalized scale radius (rcn = rc / rmax).",
      "# 4th column: logarithm of the density rho_c (log rhoc) [Msun / kpc^3].",
      "# 5th column: stellar disk mass-to-light ratio (YD).",
      "# 6th column: stellar bulge mass-to-light ratio (YB).",
      "# 7th column: minimum chi-squared value (Chi2) (- 2 log likelihood + constant).",
      "# 8th column: minimum effective chi-squared value (- 2 log posterior + constant).",
      "# 9th column: number of data points for the galaxy circular velocity (N.Data).", 
      "# ",
      "# "
    }
  ];
  Run["cat headerAux.txt dataExportAux.tsv > BurkertIndividualFits.tsv"];
  DeleteFile["headerAux.txt"];
  DeleteFile["dataExportAux.tsv"];
];



exportArctanTable := Block[
  {
    results, header, resultsData, resultsDataRAR, colR, colRho, colYD, colYB, 
    colChi2eff, colChi2, colN, dataExportAux0, listRsn, dataExportAux1, dataExport
  },
    
  results = Get[FileNameJoin[{pathAuxDirectory, "arctan-GY-1-MAGMAtableResults.m"}]];
  header = First @ results;
  resultsDataRAR = Drop[results, 1]; (*The original table for Arctan is already for the RAR sample*)
  
  colR = First @ Flatten @ Position[header, "Rt"];
  colRho = First @ Flatten @ Position[header, "Vc"];
  colYD = First @ Flatten @ Position[header, "YD"];
  colYB = First @ Flatten @ Position[header, "YB"];
  colChi2eff = First @ Flatten @ Position[header, "Chi2"];
  colChi2 = First @ Flatten @ Position[header, "V-Chi2"];
  colN = First @ Flatten @ Position[header, "N.data points"];

  dataExportAux0 = resultsDataRAR[[All, {1, colR, colRho, colYD, colYB, colChi2, colChi2eff, colN}]];
  listRsn = resultsDataRAR[[All, colR]] / (rmax153 /@ Range @ 153);
  dataExportAux1 = Join[dataExportAux0\[Transpose], {listRsn\[Transpose]}]\[Transpose]; (* dataExportAux0 with a column for Rsn*)
  dataExport = dataExportAux1[[All, {1, 2, 9, 3, 4, 5, 6, 7, 8}]]; (*Reorder the columns*)
  PrependTo[dataExport, {"Galaxy", "rt", "rtn", "Vc", "YD", "YB", "Chi2", "Chi2eff", "N.Data"}];
  
  Export["dataExportAux.tsv", dataExport, Alignment-> Left, "TextDelimiters"-> None];
  Export["headerAux.txt", 
    {
      "# Normalized additional velocity distribution: ",
      "#   testing the radial profile of dark matter halos and MOND.",
      "# by D. C. Rodrigues, A. Hernandez-Arboleda, A. Wojnar",
      "# ",
      "# Arctan_1 individual fits for 153 SPARC galaxies with Gaussian priors on YD and YB.",
      "# 1st column: galaxy name.",
      "# 2nd column: rt value [kpc].",
      "# 3rd column: normalized scale radius (rtn = rt / rmax).",
      "# 4th column: Vc value [km/s].",
      "# 5th column: stellar disk mass-to-light ratio (YD).",
      "# 6th column: stellar bulge mass-to-light ratio (YB).",
      "# 7th column: minimum chi-squared value (Chi2) (- 2 log likelihood + constant).",
      "# 8th column: minimum effective chi-squared value (- 2 log posterior + constant).",
      "# 9th column: number of data points for the galaxy circular velocity (N.Data).", 
      "# ",
      "# "
    }
  ];
  Run["cat headerAux.txt dataExportAux.tsv > ArctanIndividualFits.tsv"];
  DeleteFile["headerAux.txt"];
  DeleteFile["dataExportAux.tsv"];
];



exportArctanHalfTable := Block[
  {
    results, header, resultsData, resultsDataRAR, colR, colRho, colYD, colYB, 
    colChi2eff, colChi2, colN, dataExportAux0, listRsn, dataExportAux1, dataExport
  },
    
  results = Get[FileNameJoin[{pathAuxDirectory, "arctanHalf-GY-1-MAGMAtableResults.m"}]];
  header = First @ results;
  resultsDataRAR = Drop[results, 1]; (*The original table for Arctan is already for the RAR sample*)
  
  colR = First @ Flatten @ Position[header, "Rt"];
  colRho = First @ Flatten @ Position[header, "Vc"];
  colYD = First @ Flatten @ Position[header, "YD"];
  colYB = First @ Flatten @ Position[header, "YB"];
  colChi2eff = First @ Flatten @ Position[header, "Chi2"];
  colChi2 = First @ Flatten @ Position[header, "V-Chi2"];
  colN = First @ Flatten @ Position[header, "N.data points"];

  dataExportAux0 = resultsDataRAR[[All, {1, colR, colRho, colYD, colYB, colChi2, colChi2eff, colN}]];
  listRsn = resultsDataRAR[[All, colR]] / (rmax153 /@ Range @ 153);
  dataExportAux1 = Join[dataExportAux0\[Transpose], {listRsn\[Transpose]}]\[Transpose]; (* dataExportAux0 with a column for Rsn*)
  dataExport = dataExportAux1[[All, {1, 2, 9, 3, 4, 5, 6, 7, 8}]]; (*Reorder the columns*)
  PrependTo[dataExport, {"Galaxy", "rt", "rtn", "Vc", "YD", "YB", "Chi2", "Chi2eff", "N.Data"}];
  
  Export["dataExportAux.tsv", dataExport, Alignment-> Left, "TextDelimiters"-> None];
  Export["headerAux.txt", 
    {
      "# Normalized additional velocity distribution: ",
      "#   testing the radial profile of dark matter halos and MOND.",
      "# by D. C. Rodrigues, A. Hernandez-Arboleda, A. Wojnar",
      "# ",
      "# Arctan_1/2 individual fits for 153 SPARC galaxies with Gaussian priors on YD and YB.",
      "# 1st column: galaxy name.",
      "# 2nd column: rt value [kpc].",
      "# 3rd column: normalized scale radius (rtn = rt / rmax).",
      "# 4th column: Vc value [km/s].",
      "# 5th column: stellar disk mass-to-light ratio (YD).",
      "# 6th column: stellar bulge mass-to-light ratio (YB).",
      "# 7th column: minimum chi-squared value (Chi2) (- 2 log likelihood + constant).",
      "# 8th column: minimum effective chi-squared value (- 2 log posterior + constant).",
      "# 9th column: number of data points for the galaxy circular velocity (N.Data).", 
      "# ",
      "# "
    }
  ];
  Run["cat headerAux.txt dataExportAux.tsv > ArctanHalfIndividualFits.tsv"];
  DeleteFile["headerAux.txt"];
  DeleteFile["dataExportAux.tsv"];
];



exportMONDtable := Block[
  {
    results, header, resultsData, resultsDataRAR, colR, colRho, colYD, colYB, 
    colChi2eff, colChi2, colN, dataExportAux0, listRsn, dataExportAux1, dataExport
  },
    
  results = Get @ FileNameJoin @ {pathAuxDirectory, "MONDRAR-Fxa0-GY-05-06-MAGMAtableResults.m"};
  header = First @ results;
  resultsDataRAR = Drop[results, 1]; (*The original table for MOND is already for the RAR sample*)
  
  colYD = First @ Flatten @ Position[header, "YD"];
  colYB = First @ Flatten @ Position[header, "YB"];
  colChi2eff = First @ Flatten @ Position[header, "Chi2"];
  colChi2 = First @ Flatten @ Position[header, "V-Chi2"];
  colN = First @ Flatten @ Position[header, "N.data points"];

  dataExport = resultsDataRAR[[All, {1, colYD, colYB, colChi2, colChi2eff, colN}]];
  PrependTo[dataExport, {"Galaxy", "YD", "YB", "Chi2", "Chi2eff", "N.Data"}];
  
  Export["dataExportAux.tsv", dataExport, Alignment-> Left, "TextDelimiters"-> None];
  Export["headerAux.txt", 
    {
      "# Normalized additional velocity distribution: ",
      "#   testing the radial profile of dark matter halos and MOND.",
      "# by D. C. Rodrigues, A. Hernandez-Arboleda, A. Wojnar",
      "# ",
      "# MOND individual fits for 153 SPARC galaxies with Gaussian priors on YD and YB.",
      "# 1st column: galaxy name.",
      "# 2nd column: stellar disk mass-to-light ratio (YD).",
      "# 3rd column: stellar bulge mass-to-light ratio (YB).",
      "# 4th column: minimum chi-squared value (Chi2) (- 2 log likelihood + constant).",
      "# 5th column: minimum effective chi-squared value (- 2 log posterior + constant).",
      "# 6th column: number of data points for the galaxy circular velocity (N.Data).", 
      "# ",
      "# "
    }
  ];
  Run["cat headerAux.txt dataExportAux.tsv > MONDindividualFits.tsv"];
  DeleteFile["headerAux.txt"];
  DeleteFile["dataExportAux.tsv"];
];



exportMONDgdTable := Block[
  {
    results, header, resultsData, resultsDataRAR, coldf2, colR, colRho, colYD, colYB, 
    colChi2eff, colChi2, colN, dataExportAux0, listRsn, dataExportAux1, dataExport
  },
    
  results = Get @ FileNameJoin @ {pathAuxDirectory, "MONDRAR-Fxa0-GY-05-06-GD-MAGMAtableResults.m"};
  header = First @ results;
  resultsDataRAR = Drop[results, 1]; (*The original table for MOND is already for the RAR sample*)
  
  coldf2 = First @ Flatten @ Position[header, "df2"];
  colYD = First @ Flatten @ Position[header, "YD"];
  colYB = First @ Flatten @ Position[header, "YB"];
  colChi2eff = First @ Flatten @ Position[header, "Chi2"];
  colChi2 = First @ Flatten @ Position[header, "V-Chi2"];
  colN = First @ Flatten @ Position[header, "N.data points"];

  dataExport = resultsDataRAR[[All, {1, colYD, colYB, coldf2, colChi2, colChi2eff, colN}]];
  PrependTo[dataExport, {"Galaxy", "YD", "YB", "deltaD", "Chi2", "Chi2eff", "N.Data"}];
  
  Export["dataExportAux.tsv", dataExport, Alignment-> Left, "TextDelimiters"-> None];
  Export["headerAux.txt", 
    {
      "# Normalized additional velocity distribution: ",
      "#   testing the radial profile of dark matter halos and MOND.",
      "# by D. C. Rodrigues, A. Hernandez-Arboleda, A. Wojnar",
      "# ",
      "# MOND_dist individual fits for 153 SPARC galaxies with Gaussian priors on YD, YB and on the distance.",
      "# 1st column: galaxy name.",
      "# 2nd column: stellar disk mass-to-light ratio (YD).",
      "# 3rd column: stellar bulge mass-to-light ratio (YB).",
      "# 4th column: multiplicative factor that changes the galaxy distance.",
      "# 4th column: minimum chi-squared value (Chi2) (- 2 log likelihood + constant).",
      "# 5th column: minimum effective chi-squared value (- 2 log posterior + constant).",
      "# 6th column: number of data points for the galaxy circular velocity (N.Data).", 
      "# ",
      "# "
    }
  ];
  Run["cat headerAux.txt dataExportAux.tsv > MONDgdIndividualFits.tsv"];
  DeleteFile["headerAux.txt"];
  DeleteFile["dataExportAux.tsv"];
];



(* ::Section:: *)
(*Executing*)

exportNFWtable;
exportBurkertTable;
exportArctanTable;
exportArctanHalfTable;
exportMONDtable;
exportMONDgdTable;
