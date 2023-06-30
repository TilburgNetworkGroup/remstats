#define ARMA_64BIT_WORD 1
#include "RcppArmadillo.h"
#include <remify.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
#include <iostream>
#include <map>

using namespace Rcpp;

// [[Rcpp::export]]
void testStringComparison(std::string scaling) {

    if (scaling.compare("std") == 0) {
        std::cout << "String comparison: scaling is equal to \"std\"" << std::endl;
    } else {
        std::cout << "String comparison: scaling is not equal to \"std\"" << std::endl;
    }
}