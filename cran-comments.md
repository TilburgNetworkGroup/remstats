# remstats 3.2.2
Date: 2024-05-14

## Added
- [Issue [[#56](https://github.com/TilburgNetworkGroup/remstats/issues/56)]] - The ability to generate exploratory `plot()` and `boxplot()` for `remstats` objects. These functions were designed to work with large remstats objects by taking a subset of timepoints, dyads or actors (depending on the type of plot). The user can determine the subset to be taken. Setting the subset to the whole sequence is not recommended for large remstats objects.
- Added warning when `consider_type` is specified but event types are not in the DV.
- Added tests for plotting functions.
- Increased test coverage.

## Fixed
- Resolved conversion errors occurring when using single-letter logical values (i.e., 'T' for TRUE or 'F' for FALSE).
- Resolved error with switch on boolean type (in tomstats.cpp)

## Test environments
- Local Windows 10, R version 4.3.1 
- rhub check: Fedora Linux, R-devel, clang, gfortran
- rhub check: Ubuntu Linux 20.04.1 LTS, R-release, GCC
- rhub check: Windows Server 2022, R-devel, 64 bit

## R CMD check results
There were no ERRORs, no WARNINGs, no NOTEs.