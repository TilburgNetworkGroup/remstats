// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_remstats_RCPPEXPORTS_H_GEN_
#define RCPP_remstats_RCPPEXPORTS_H_GEN_

#include <RcppArmadillo.h>
#include <Rcpp.h>

namespace remstats {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("remstats", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("remstats", "_remstats_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in remstats");
            }
        }
    }

    inline arma::mat calculate_aom_tie_statistic(const arma::mat& covariates, const arma::mat& edgelist, const arma::vec& actors, int start, int stop, Rcpp::String scaling, bool display_progress) {
        typedef SEXP(*Ptr_calculate_aom_tie_statistic)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_calculate_aom_tie_statistic p_calculate_aom_tie_statistic = NULL;
        if (p_calculate_aom_tie_statistic == NULL) {
            validateSignature("arma::mat(*calculate_aom_tie_statistic)(const arma::mat&,const arma::mat&,const arma::vec&,int,int,Rcpp::String,bool)");
            p_calculate_aom_tie_statistic = (Ptr_calculate_aom_tie_statistic)R_GetCCallable("remstats", "_remstats_calculate_aom_tie_statistic");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_calculate_aom_tie_statistic(Shield<SEXP>(Rcpp::wrap(covariates)), Shield<SEXP>(Rcpp::wrap(edgelist)), Shield<SEXP>(Rcpp::wrap(actors)), Shield<SEXP>(Rcpp::wrap(start)), Shield<SEXP>(Rcpp::wrap(stop)), Shield<SEXP>(Rcpp::wrap(scaling)), Shield<SEXP>(Rcpp::wrap(display_progress)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::mat >(rcpp_result_gen);
    }

    inline arma::cube compute_stats_rate(Rcpp::CharacterVector& effects, const arma::mat& edgelist, const arma::vec& actors, const arma::vec& weights, const Rcpp::List& covariates, const Rcpp::List& interactions, std::string memory, const arma::vec memory_value, Rcpp::CharacterVector& scaling, int start, int stop, bool display_progress) {
        typedef SEXP(*Ptr_compute_stats_rate)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_compute_stats_rate p_compute_stats_rate = NULL;
        if (p_compute_stats_rate == NULL) {
            validateSignature("arma::cube(*compute_stats_rate)(Rcpp::CharacterVector&,const arma::mat&,const arma::vec&,const arma::vec&,const Rcpp::List&,const Rcpp::List&,std::string,const arma::vec,Rcpp::CharacterVector&,int,int,bool)");
            p_compute_stats_rate = (Ptr_compute_stats_rate)R_GetCCallable("remstats", "_remstats_compute_stats_rate");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_compute_stats_rate(Shield<SEXP>(Rcpp::wrap(effects)), Shield<SEXP>(Rcpp::wrap(edgelist)), Shield<SEXP>(Rcpp::wrap(actors)), Shield<SEXP>(Rcpp::wrap(weights)), Shield<SEXP>(Rcpp::wrap(covariates)), Shield<SEXP>(Rcpp::wrap(interactions)), Shield<SEXP>(Rcpp::wrap(memory)), Shield<SEXP>(Rcpp::wrap(memory_value)), Shield<SEXP>(Rcpp::wrap(scaling)), Shield<SEXP>(Rcpp::wrap(start)), Shield<SEXP>(Rcpp::wrap(stop)), Shield<SEXP>(Rcpp::wrap(display_progress)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::cube >(rcpp_result_gen);
    }

    inline arma::cube compute_stats_choice(Rcpp::CharacterVector& effects, const arma::mat& edgelist, const arma::vec& actors, const arma::vec& weights, const Rcpp::List& covariates, const Rcpp::List& interactions, std::string memory, const arma::vec memory_value, Rcpp::CharacterVector& scaling, int start, int stop, bool display_progress) {
        typedef SEXP(*Ptr_compute_stats_choice)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_compute_stats_choice p_compute_stats_choice = NULL;
        if (p_compute_stats_choice == NULL) {
            validateSignature("arma::cube(*compute_stats_choice)(Rcpp::CharacterVector&,const arma::mat&,const arma::vec&,const arma::vec&,const Rcpp::List&,const Rcpp::List&,std::string,const arma::vec,Rcpp::CharacterVector&,int,int,bool)");
            p_compute_stats_choice = (Ptr_compute_stats_choice)R_GetCCallable("remstats", "_remstats_compute_stats_choice");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_compute_stats_choice(Shield<SEXP>(Rcpp::wrap(effects)), Shield<SEXP>(Rcpp::wrap(edgelist)), Shield<SEXP>(Rcpp::wrap(actors)), Shield<SEXP>(Rcpp::wrap(weights)), Shield<SEXP>(Rcpp::wrap(covariates)), Shield<SEXP>(Rcpp::wrap(interactions)), Shield<SEXP>(Rcpp::wrap(memory)), Shield<SEXP>(Rcpp::wrap(memory_value)), Shield<SEXP>(Rcpp::wrap(scaling)), Shield<SEXP>(Rcpp::wrap(start)), Shield<SEXP>(Rcpp::wrap(stop)), Shield<SEXP>(Rcpp::wrap(display_progress)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::cube >(rcpp_result_gen);
    }

    inline arma::mat getRisksetMatrix(arma::uvec actorID, arma::uvec typeID, arma::uword N, arma::uword C, bool directed) {
        typedef SEXP(*Ptr_getRisksetMatrix)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_getRisksetMatrix p_getRisksetMatrix = NULL;
        if (p_getRisksetMatrix == NULL) {
            validateSignature("arma::mat(*getRisksetMatrix)(arma::uvec,arma::uvec,arma::uword,arma::uword,bool)");
            p_getRisksetMatrix = (Ptr_getRisksetMatrix)R_GetCCallable("remstats", "_remstats_getRisksetMatrix");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_getRisksetMatrix(Shield<SEXP>(Rcpp::wrap(actorID)), Shield<SEXP>(Rcpp::wrap(typeID)), Shield<SEXP>(Rcpp::wrap(N)), Shield<SEXP>(Rcpp::wrap(C)), Shield<SEXP>(Rcpp::wrap(directed)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::mat >(rcpp_result_gen);
    }

    inline arma::mat compute_adjmat(const arma::mat& edgelist, int D, bool directed, Rcpp::String memory, arma::vec memory_value, int start, int stop, bool display_progress) {
        typedef SEXP(*Ptr_compute_adjmat)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_compute_adjmat p_compute_adjmat = NULL;
        if (p_compute_adjmat == NULL) {
            validateSignature("arma::mat(*compute_adjmat)(const arma::mat&,int,bool,Rcpp::String,arma::vec,int,int,bool)");
            p_compute_adjmat = (Ptr_compute_adjmat)R_GetCCallable("remstats", "_remstats_compute_adjmat");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_compute_adjmat(Shield<SEXP>(Rcpp::wrap(edgelist)), Shield<SEXP>(Rcpp::wrap(D)), Shield<SEXP>(Rcpp::wrap(directed)), Shield<SEXP>(Rcpp::wrap(memory)), Shield<SEXP>(Rcpp::wrap(memory_value)), Shield<SEXP>(Rcpp::wrap(start)), Shield<SEXP>(Rcpp::wrap(stop)), Shield<SEXP>(Rcpp::wrap(display_progress)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::mat >(rcpp_result_gen);
    }

    inline arma::cube compute_stats_tie(Rcpp::CharacterVector& effects, const arma::mat& edgelist, const arma::mat& adjmat, const arma::vec& actors, const arma::vec& types, const arma::mat& riskset, Rcpp::CharacterVector& scaling, Rcpp::LogicalVector& consider_type, const Rcpp::List& covariates, const Rcpp::List& interactions, int start, int stop, bool directed, bool display_progress) {
        typedef SEXP(*Ptr_compute_stats_tie)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_compute_stats_tie p_compute_stats_tie = NULL;
        if (p_compute_stats_tie == NULL) {
            validateSignature("arma::cube(*compute_stats_tie)(Rcpp::CharacterVector&,const arma::mat&,const arma::mat&,const arma::vec&,const arma::vec&,const arma::mat&,Rcpp::CharacterVector&,Rcpp::LogicalVector&,const Rcpp::List&,const Rcpp::List&,int,int,bool,bool)");
            p_compute_stats_tie = (Ptr_compute_stats_tie)R_GetCCallable("remstats", "_remstats_compute_stats_tie");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_compute_stats_tie(Shield<SEXP>(Rcpp::wrap(effects)), Shield<SEXP>(Rcpp::wrap(edgelist)), Shield<SEXP>(Rcpp::wrap(adjmat)), Shield<SEXP>(Rcpp::wrap(actors)), Shield<SEXP>(Rcpp::wrap(types)), Shield<SEXP>(Rcpp::wrap(riskset)), Shield<SEXP>(Rcpp::wrap(scaling)), Shield<SEXP>(Rcpp::wrap(consider_type)), Shield<SEXP>(Rcpp::wrap(covariates)), Shield<SEXP>(Rcpp::wrap(interactions)), Shield<SEXP>(Rcpp::wrap(start)), Shield<SEXP>(Rcpp::wrap(stop)), Shield<SEXP>(Rcpp::wrap(directed)), Shield<SEXP>(Rcpp::wrap(display_progress)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::cube >(rcpp_result_gen);
    }

}

#endif // RCPP_remstats_RCPPEXPORTS_H_GEN_
