# remstats 3.2.1
Date: 2023-11-29

## Fixed
- Fixed [Issue [[#78](https://github.com/TilburgNetworkGroup/remstats/issues/78)]]: The attributes of the `remstats` object are now correctly set after executing `bind_remstats()`.
- Resolved a bug where `remstats()` failed to find the edgelist when `reh` is ordinal.
- Resolved a bug where `aomstats()` failed when the computation method was `pe`, the memory method was `decay` and the first event at the `start` index was not the first event at that time. 
- Resolved Rcpp literal string warning. 

## Test environments
- Local Windows 10, R version 4.3.1 
- rhub check: Fedora Linux, R-devel, clang, gfortran
- rhub check: Ubuntu Linux 20.04.1 LTS, R-release, GCC
- rhub check: Windows Server 2022, R-devel, 64 bit

## R CMD check results
There were no ERRORs, no WARNINGs, no NOTEs.