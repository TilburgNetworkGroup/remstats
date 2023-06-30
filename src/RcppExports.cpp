// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/remstats.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// compute_stats_rate
arma::cube compute_stats_rate(const arma::vec& effects, const arma::mat& edgelist, const arma::vec& actors, const arma::vec& weights, const Rcpp::List& covariates, const Rcpp::List& interactions, std::string memory, const arma::vec memory_value, const arma::vec& scaling, int start, int stop, bool display_progress);
static SEXP _remstats_compute_stats_rate_try(SEXP effectsSEXP, SEXP edgelistSEXP, SEXP actorsSEXP, SEXP weightsSEXP, SEXP covariatesSEXP, SEXP interactionsSEXP, SEXP memorySEXP, SEXP memory_valueSEXP, SEXP scalingSEXP, SEXP startSEXP, SEXP stopSEXP, SEXP display_progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type effects(effectsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type edgelist(edgelistSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type actors(actorsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type covariates(covariatesSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type interactions(interactionsSEXP);
    Rcpp::traits::input_parameter< std::string >::type memory(memorySEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type memory_value(memory_valueSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type scaling(scalingSEXP);
    Rcpp::traits::input_parameter< int >::type start(startSEXP);
    Rcpp::traits::input_parameter< int >::type stop(stopSEXP);
    Rcpp::traits::input_parameter< bool >::type display_progress(display_progressSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_stats_rate(effects, edgelist, actors, weights, covariates, interactions, memory, memory_value, scaling, start, stop, display_progress));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _remstats_compute_stats_rate(SEXP effectsSEXP, SEXP edgelistSEXP, SEXP actorsSEXP, SEXP weightsSEXP, SEXP covariatesSEXP, SEXP interactionsSEXP, SEXP memorySEXP, SEXP memory_valueSEXP, SEXP scalingSEXP, SEXP startSEXP, SEXP stopSEXP, SEXP display_progressSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_remstats_compute_stats_rate_try(effectsSEXP, edgelistSEXP, actorsSEXP, weightsSEXP, covariatesSEXP, interactionsSEXP, memorySEXP, memory_valueSEXP, scalingSEXP, startSEXP, stopSEXP, display_progressSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// compute_stats_choice
arma::cube compute_stats_choice(const arma::vec& effects, const arma::mat& edgelist, const arma::vec& actors, const arma::vec& weights, const Rcpp::List& covariates, const Rcpp::List& interactions, std::string memory, const arma::vec memory_value, const arma::vec& scaling, int start, int stop, bool display_progress);
static SEXP _remstats_compute_stats_choice_try(SEXP effectsSEXP, SEXP edgelistSEXP, SEXP actorsSEXP, SEXP weightsSEXP, SEXP covariatesSEXP, SEXP interactionsSEXP, SEXP memorySEXP, SEXP memory_valueSEXP, SEXP scalingSEXP, SEXP startSEXP, SEXP stopSEXP, SEXP display_progressSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type effects(effectsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type edgelist(edgelistSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type actors(actorsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type covariates(covariatesSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type interactions(interactionsSEXP);
    Rcpp::traits::input_parameter< std::string >::type memory(memorySEXP);
    Rcpp::traits::input_parameter< const arma::vec >::type memory_value(memory_valueSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type scaling(scalingSEXP);
    Rcpp::traits::input_parameter< int >::type start(startSEXP);
    Rcpp::traits::input_parameter< int >::type stop(stopSEXP);
    Rcpp::traits::input_parameter< bool >::type display_progress(display_progressSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_stats_choice(effects, edgelist, actors, weights, covariates, interactions, memory, memory_value, scaling, start, stop, display_progress));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _remstats_compute_stats_choice(SEXP effectsSEXP, SEXP edgelistSEXP, SEXP actorsSEXP, SEXP weightsSEXP, SEXP covariatesSEXP, SEXP interactionsSEXP, SEXP memorySEXP, SEXP memory_valueSEXP, SEXP scalingSEXP, SEXP startSEXP, SEXP stopSEXP, SEXP display_progressSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_remstats_compute_stats_choice_try(effectsSEXP, edgelistSEXP, actorsSEXP, weightsSEXP, covariatesSEXP, interactionsSEXP, memorySEXP, memory_valueSEXP, scalingSEXP, startSEXP, stopSEXP, display_progressSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// testStringComparison
void testStringComparison(std::string scaling);
static SEXP _remstats_testStringComparison_try(SEXP scalingSEXP) {
BEGIN_RCPP
    Rcpp::traits::input_parameter< std::string >::type scaling(scalingSEXP);
    testStringComparison(scaling);
    return R_NilValue;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _remstats_testStringComparison(SEXP scalingSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_remstats_testStringComparison_try(scalingSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// getRisksetMatrix
arma::mat getRisksetMatrix(arma::uvec actorID, arma::uvec typeID, arma::uword N, arma::uword C, bool directed);
static SEXP _remstats_getRisksetMatrix_try(SEXP actorIDSEXP, SEXP typeIDSEXP, SEXP NSEXP, SEXP CSEXP, SEXP directedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< arma::uvec >::type actorID(actorIDSEXP);
    Rcpp::traits::input_parameter< arma::uvec >::type typeID(typeIDSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type N(NSEXP);
    Rcpp::traits::input_parameter< arma::uword >::type C(CSEXP);
    Rcpp::traits::input_parameter< bool >::type directed(directedSEXP);
    rcpp_result_gen = Rcpp::wrap(getRisksetMatrix(actorID, typeID, N, C, directed));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _remstats_getRisksetMatrix(SEXP actorIDSEXP, SEXP typeIDSEXP, SEXP NSEXP, SEXP CSEXP, SEXP directedSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_remstats_getRisksetMatrix_try(actorIDSEXP, typeIDSEXP, NSEXP, CSEXP, directedSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// compute_adjmat
arma::mat compute_adjmat(const arma::mat& edgelist, int D, bool directed, Rcpp::String memory, arma::vec memory_value, int start, int stop);
static SEXP _remstats_compute_adjmat_try(SEXP edgelistSEXP, SEXP DSEXP, SEXP directedSEXP, SEXP memorySEXP, SEXP memory_valueSEXP, SEXP startSEXP, SEXP stopSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type edgelist(edgelistSEXP);
    Rcpp::traits::input_parameter< int >::type D(DSEXP);
    Rcpp::traits::input_parameter< bool >::type directed(directedSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type memory(memorySEXP);
    Rcpp::traits::input_parameter< arma::vec >::type memory_value(memory_valueSEXP);
    Rcpp::traits::input_parameter< int >::type start(startSEXP);
    Rcpp::traits::input_parameter< int >::type stop(stopSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_adjmat(edgelist, D, directed, memory, memory_value, start, stop));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _remstats_compute_adjmat(SEXP edgelistSEXP, SEXP DSEXP, SEXP directedSEXP, SEXP memorySEXP, SEXP memory_valueSEXP, SEXP startSEXP, SEXP stopSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_remstats_compute_adjmat_try(edgelistSEXP, DSEXP, directedSEXP, memorySEXP, memory_valueSEXP, startSEXP, stopSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// compute_stats_tie
arma::cube compute_stats_tie(Rcpp::CharacterVector& effects, const arma::mat& edgelist, const arma::mat& adjmat, const arma::vec& actors, const arma::vec& types, const arma::mat& riskset, Rcpp::CharacterVector& scaling, Rcpp::LogicalVector& consider_type, const Rcpp::List& covariates, const Rcpp::List& interactions, int start, int stop, bool directed);
static SEXP _remstats_compute_stats_tie_try(SEXP effectsSEXP, SEXP edgelistSEXP, SEXP adjmatSEXP, SEXP actorsSEXP, SEXP typesSEXP, SEXP risksetSEXP, SEXP scalingSEXP, SEXP consider_typeSEXP, SEXP covariatesSEXP, SEXP interactionsSEXP, SEXP startSEXP, SEXP stopSEXP, SEXP directedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector& >::type effects(effectsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type edgelist(edgelistSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type adjmat(adjmatSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type actors(actorsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type types(typesSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type riskset(risksetSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector& >::type scaling(scalingSEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector& >::type consider_type(consider_typeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type covariates(covariatesSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type interactions(interactionsSEXP);
    Rcpp::traits::input_parameter< int >::type start(startSEXP);
    Rcpp::traits::input_parameter< int >::type stop(stopSEXP);
    Rcpp::traits::input_parameter< bool >::type directed(directedSEXP);
    rcpp_result_gen = Rcpp::wrap(compute_stats_tie(effects, edgelist, adjmat, actors, types, riskset, scaling, consider_type, covariates, interactions, start, stop, directed));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _remstats_compute_stats_tie(SEXP effectsSEXP, SEXP edgelistSEXP, SEXP adjmatSEXP, SEXP actorsSEXP, SEXP typesSEXP, SEXP risksetSEXP, SEXP scalingSEXP, SEXP consider_typeSEXP, SEXP covariatesSEXP, SEXP interactionsSEXP, SEXP startSEXP, SEXP stopSEXP, SEXP directedSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_remstats_compute_stats_tie_try(effectsSEXP, edgelistSEXP, adjmatSEXP, actorsSEXP, typesSEXP, risksetSEXP, scalingSEXP, consider_typeSEXP, covariatesSEXP, interactionsSEXP, startSEXP, stopSEXP, directedSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// combine_arrays
arma::cube combine_arrays(const Rcpp::List& array_list, int along);
RcppExport SEXP _remstats_combine_arrays(SEXP array_listSEXP, SEXP alongSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::List& >::type array_list(array_listSEXP);
    Rcpp::traits::input_parameter< int >::type along(alongSEXP);
    rcpp_result_gen = Rcpp::wrap(combine_arrays(array_list, along));
    return rcpp_result_gen;
END_RCPP
}

// validate (ensure exported C++ functions exist before calling them)
static int _remstats_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("arma::cube(*compute_stats_rate)(const arma::vec&,const arma::mat&,const arma::vec&,const arma::vec&,const Rcpp::List&,const Rcpp::List&,std::string,const arma::vec,const arma::vec&,int,int,bool)");
        signatures.insert("arma::cube(*compute_stats_choice)(const arma::vec&,const arma::mat&,const arma::vec&,const arma::vec&,const Rcpp::List&,const Rcpp::List&,std::string,const arma::vec,const arma::vec&,int,int,bool)");
        signatures.insert("void(*testStringComparison)(std::string)");
        signatures.insert("arma::mat(*getRisksetMatrix)(arma::uvec,arma::uvec,arma::uword,arma::uword,bool)");
        signatures.insert("arma::mat(*compute_adjmat)(const arma::mat&,int,bool,Rcpp::String,arma::vec,int,int)");
        signatures.insert("arma::cube(*compute_stats_tie)(Rcpp::CharacterVector&,const arma::mat&,const arma::mat&,const arma::vec&,const arma::vec&,const arma::mat&,Rcpp::CharacterVector&,Rcpp::LogicalVector&,const Rcpp::List&,const Rcpp::List&,int,int,bool)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _remstats_RcppExport_registerCCallable() { 
    R_RegisterCCallable("remstats", "_remstats_compute_stats_rate", (DL_FUNC)_remstats_compute_stats_rate_try);
    R_RegisterCCallable("remstats", "_remstats_compute_stats_choice", (DL_FUNC)_remstats_compute_stats_choice_try);
    R_RegisterCCallable("remstats", "_remstats_testStringComparison", (DL_FUNC)_remstats_testStringComparison_try);
    R_RegisterCCallable("remstats", "_remstats_getRisksetMatrix", (DL_FUNC)_remstats_getRisksetMatrix_try);
    R_RegisterCCallable("remstats", "_remstats_compute_adjmat", (DL_FUNC)_remstats_compute_adjmat_try);
    R_RegisterCCallable("remstats", "_remstats_compute_stats_tie", (DL_FUNC)_remstats_compute_stats_tie_try);
    R_RegisterCCallable("remstats", "_remstats_RcppExport_validate", (DL_FUNC)_remstats_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_remstats_compute_stats_rate", (DL_FUNC) &_remstats_compute_stats_rate, 12},
    {"_remstats_compute_stats_choice", (DL_FUNC) &_remstats_compute_stats_choice, 12},
    {"_remstats_testStringComparison", (DL_FUNC) &_remstats_testStringComparison, 1},
    {"_remstats_getRisksetMatrix", (DL_FUNC) &_remstats_getRisksetMatrix, 5},
    {"_remstats_compute_adjmat", (DL_FUNC) &_remstats_compute_adjmat, 7},
    {"_remstats_compute_stats_tie", (DL_FUNC) &_remstats_compute_stats_tie, 13},
    {"_remstats_combine_arrays", (DL_FUNC) &_remstats_combine_arrays, 2},
    {"_remstats_RcppExport_registerCCallable", (DL_FUNC) &_remstats_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_remstats(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
