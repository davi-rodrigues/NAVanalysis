# NAVanalysis
Normalized Additional Velocity analysis (Release 1.0.1)

[![DOI](https://zenodo.org/badge/469982786.svg)](https://zenodo.org/badge/latestdoi/469982786)

Code by Davi Rodrigues (UFES) and Alejandro Hernandez-Arboleda (UFES).

# The corresponding paper
This code shows in detail all the necessary technical procedures of the following paper:

> *Normalized Additional Velocity: a fast sample analysis for dark matter or modified gravity models*
> 
> *A.Hernandez-Arboleda, D.C. Rodrigues, A.Wojnar*
> 
> arXiv: [2204.03762](http://arxiv.org/abs/2204.03762)


If you use this code, or part of it, please cite the paper above.

All the methods are explained in the above paper.

# Files and folders explanation

* `NAVanalysis.nb` is a Mathematica Notebook and it is the main file.
* `NAVanalysis.m` is automatically generated from the `nb` file above. It contains all the code in plain text format, it can be run outside Mathematica.
* `NAVbaseCode.wl` contains several essential definitions for the execution of NAVanalysis.
* `NAVoptions.wl` is a set of options mainly devoted for the plots.
* The root of `AxiliaryData` folder contains large tables that include the best fit results generated by [MAGMA](https://github.com/davi-rodrigues/MAGMA) for 153 SPARC galaxies.
* The `AxiliaryData` folder also contains the folder `SPARC_Rotmod`, in which data from the SPARC galaxies was inserted. We are providing these data for easy use of NAVanalysis. 
The original data can be found [here](http://astroweb.cwru.edu/SPARC/), these data DO NOT CONSTITUTE a part of NAVanalysis. 
* The `Output` folder contains files that were generated and exported by NAVanalsis. 

# Other packages

* Some of the plots require the [CustomTicks package](https://github.com/mark-caprio/CustomTicks), it can be freely used.
* [MAGMA](https://github.com/davi-rodrigues/MAGMA) is helpful to generate new galaxy fit results, but it is not necessary to run NAVanalysis: the relevant MAGMA output is already in the `AuxiliaryData` folder. 

# Acknowledgements

This work was in part supported by the following Brazilian funding agencies: CAPES, CNPq and FAPES.
