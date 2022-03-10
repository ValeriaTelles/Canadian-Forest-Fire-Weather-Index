# Canadian Forest Fire Weather Index

This project implements a modern Fortran program (i.e., F95) of the Fortran IV program described in the article, "Equations and Fortran IV Program for the 1976 Metric Version of the Forest Fire Weather Index", by Van Wagner, C.E., and Pickett, T.L. 

## How to Use 

 1. Compile with ```gfortran -Wall FFWIndices.f95 ffwi.f95```
 2. Run with ```./a.out```
 3. Input a filename containing the appropriate weather data you want to use to analyze.
 4. Program will read the file and output the Forest Fire Weather Index.
