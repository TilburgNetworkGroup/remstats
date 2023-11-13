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

    inline arma::cube compute_stats_sender(Rcpp::CharacterVector& effects, const arma::mat& edgelist, const arma::vec& actors, const arma::vec& weights, const Rcpp::List& covariates, const Rcpp::List& interactions, std::string memory, const arma::vec memory_value, Rcpp::CharacterVector& scaling, int start, int stop, std::string method, bool display_progress) {
        typedef SEXP(*Ptr_compute_stats_sender)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_compute_stats_sender p_compute_stats_sender = NULL;
        if (p_compute_stats_sender == NULL) {
            validateSignature("arma::cube(*compute_stats_sender)(Rcpp::CharacterVector&,const arma::mat&,const arma::vec&,const arma::vec&,const Rcpp::List&,const Rcpp::List&,std::string,const arma::vec,Rcpp::CharacterVector&,int,int,std::string,bool)");
            p_compute_stats_sender = (Ptr_compute_stats_sender)R_GetCCallable("remstats", "_remstats_compute_stats_sender");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_compute_stats_sender(Shield<SEXP>(Rcpp::wrap(effects)), Shield<SEXP>(Rcpp::wrap(edgelist)), Shield<SEXP>(Rcpp::wrap(actors)), Shield<SEXP>(Rcpp::wrap(weights)), Shield<SEXP>(Rcpp::wrap(covariates)), Shield<SEXP>(Rcpp::wrap(interactions)), Shield<SEXP>(Rcpp::wrap(memory)), Shield<SEXP>(Rcpp::wrap(memory_value)), Shield<SEXP>(Rcpp::wrap(scaling)), Shield<SEXP>(Rcpp::wrap(start)), Shield<SEXP>(Rcpp::wrap(stop)), Shield<SEXP>(Rcpp::wrap(method)), Shield<SEXP>(Rcpp::wrap(display_progress)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::cube >(rcpp_result_gen);
    }

    inline arma::cube compute_stats_receiver(Rcpp::CharacterVector& effects, const arma::mat& edgelist, const arma::vec& actors, const arma::vec& weights, const Rcpp::List& covariates, const Rcpp::List& interactions, std::string memory, const arma::vec memory_value, Rcpp::CharacterVector& scaling, int start, int stop, std::string method, bool display_progress) {
        typedef SEXP(*Ptr_compute_stats_receiver)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_compute_stats_receiver p_compute_stats_receiver = NULL;
        if (p_compute_stats_receiver == NULL) {
            validateSignature("arma::cube(*compute_stats_receiver)(Rcpp::CharacterVector&,const arma::mat&,const arma::vec&,const arma::vec&,const Rcpp::List&,const Rcpp::List&,std::string,const arma::vec,Rcpp::CharacterVector&,int,int,std::string,bool)");
            p_compute_stats_receiver = (Ptr_compute_stats_receiver)R_GetCCallable("remstats", "_remstats_compute_stats_receiver");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_compute_stats_receiver(Shield<SEXP>(Rcpp::wrap(effects)), Shield<SEXP>(Rcpp::wrap(edgelist)), Shield<SEXP>(Rcpp::wrap(actors)), Shield<SEXP>(Rcpp::wrap(weights)), Shield<SEXP>(Rcpp::wrap(covariates)), Shield<SEXP>(Rcpp::wrap(interactions)), Shield<SEXP>(Rcpp::wrap(memory)), Shield<SEXP>(Rcpp::wrap(memory_value)), Shield<SEXP>(Rcpp::wrap(scaling)), Shield<SEXP>(Rcpp::wrap(start)), Shield<SEXP>(Rcpp::wrap(stop)), Shield<SEXP>(Rcpp::wrap(method)), Shield<SEXP>(Rcpp::wrap(display_progress)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::cube >(rcpp_result_gen);
    }

    inline arma::mat get_riskset(arma::uvec actorID, arma::uvec typeID, bool directed) {
        typedef SEXP(*Ptr_get_riskset)(SEXP,SEXP,SEXP);
        static Ptr_get_riskset p_get_riskset = NULL;
        if (p_get_riskset == NULL) {
            validateSignature("arma::mat(*get_riskset)(arma::uvec,arma::uvec,bool)");
            p_get_riskset = (Ptr_get_riskset)R_GetCCallable("remstats", "_remstats_get_riskset");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_get_riskset(Shield<SEXP>(Rcpp::wrap(actorID)), Shield<SEXP>(Rcpp::wrap(typeID)), Shield<SEXP>(Rcpp::wrap(directed)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::mat >(rcpp_result_gen);
    }

    inline arma::mat convert_to_risksetMatrix(arma::mat riskset, int N, int C) {
        typedef SEXP(*Ptr_convert_to_risksetMatrix)(SEXP,SEXP,SEXP);
        static Ptr_convert_to_risksetMatrix p_convert_to_risksetMatrix = NULL;
        if (p_convert_to_risksetMatrix == NULL) {
            validateSignature("arma::mat(*convert_to_risksetMatrix)(arma::mat,int,int)");
            p_convert_to_risksetMatrix = (Ptr_convert_to_risksetMatrix)R_GetCCallable("remstats", "_remstats_convert_to_risksetMatrix");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_convert_to_risksetMatrix(Shield<SEXP>(Rcpp::wrap(riskset)), Shield<SEXP>(Rcpp::wrap(N)), Shield<SEXP>(Rcpp::wrap(C)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::mat >(rcpp_result_gen);
    }

    inline arma::mat calculate_inertia(const arma::mat& edgelist, const arma::vec& weights, const arma::mat& risksetMatrix, Rcpp::String memory, const arma::vec& memory_value, int start, int stop, bool display_progress, Rcpp::String method) {
        typedef SEXP(*Ptr_calculate_inertia)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_calculate_inertia p_calculate_inertia = NULL;
        if (p_calculate_inertia == NULL) {
            validateSignature("arma::mat(*calculate_inertia)(const arma::mat&,const arma::vec&,const arma::mat&,Rcpp::String,const arma::vec&,int,int,bool,Rcpp::String)");
            p_calculate_inertia = (Ptr_calculate_inertia)R_GetCCallable("remstats", "_remstats_calculate_inertia");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_calculate_inertia(Shield<SEXP>(Rcpp::wrap(edgelist)), Shield<SEXP>(Rcpp::wrap(weights)), Shield<SEXP>(Rcpp::wrap(risksetMatrix)), Shield<SEXP>(Rcpp::wrap(memory)), Shield<SEXP>(Rcpp::wrap(memory_value)), Shield<SEXP>(Rcpp::wrap(start)), Shield<SEXP>(Rcpp::wrap(stop)), Shield<SEXP>(Rcpp::wrap(display_progress)), Shield<SEXP>(Rcpp::wrap(method)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<arma::mat >(rcpp_result_gen);
    }

    inline arma::cube compute_stats_tie(Rcpp::CharacterVector effects, const arma::mat& edgelist, const arma::mat& riskset, const arma::mat& risksetMatrix, const arma::mat& inertia, const Rcpp::List& covariates, const Rcpp::List& interactions, Rcpp::String memory, const arma::vec& memory_value, Rcpp::CharacterVector& scaling, Rcpp::LogicalVector& consider_type, int start, int stop, bool directed, bool display_progress, Rcpp::String method) {
        typedef SEXP(*Ptr_compute_stats_tie)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_compute_stats_tie p_compute_stats_tie = NULL;
        if (p_compute_stats_tie == NULL) {
            validateSignature("arma::cube(*compute_stats_tie)(Rcpp::CharacterVector,const arma::mat&,const arma::mat&,const arma::mat&,const arma::mat&,const Rcpp::List&,const Rcpp::List&,Rcpp::String,const arma::vec&,Rcpp::CharacterVector&,Rcpp::LogicalVector&,int,int,bool,bool,Rcpp::String)");
            p_compute_stats_tie = (Ptr_compute_stats_tie)R_GetCCallable("remstats", "_remstats_compute_stats_tie");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_compute_stats_tie(Shield<SEXP>(Rcpp::wrap(effects)), Shield<SEXP>(Rcpp::wrap(edgelist)), Shield<SEXP>(Rcpp::wrap(riskset)), Shield<SEXP>(Rcpp::wrap(risksetMatrix)), Shield<SEXP>(Rcpp::wrap(inertia)), Shield<SEXP>(Rcpp::wrap(covariates)), Shield<SEXP>(Rcpp::wrap(interactions)), Shield<SEXP>(Rcpp::wrap(memory)), Shield<SEXP>(Rcpp::wrap(memory_value)), Shield<SEXP>(Rcpp::wrap(scaling)), Shield<SEXP>(Rcpp::wrap(consider_type)), Shield<SEXP>(Rcpp::wrap(start)), Shield<SEXP>(Rcpp::wrap(stop)), Shield<SEXP>(Rcpp::wrap(directed)), Shield<SEXP>(Rcpp::wrap(display_progress)), Shield<SEXP>(Rcpp::wrap(method)));
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
