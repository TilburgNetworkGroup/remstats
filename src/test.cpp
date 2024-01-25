
#define ARMA_64BIT_WORD 1
#include "RcppArmadillo.h"
#include <stdexcept> // std::runtime_error
#include <progress.hpp>
#include <progress_bar.hpp>
#include <iostream>
#include <map>
#include <string>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppProgress)]]
