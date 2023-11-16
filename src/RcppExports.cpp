// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// sgd_lm_cpp
List sgd_lm_cpp(const arma::mat& x, const arma::colvec& y, const int& burn, const double& gamma_0, const double& alpha, const arma::colvec& bt_start, const arma::rowvec& x_mean, const arma::rowvec& x_sd, const bool& path, const arma::colvec& path_index);
RcppExport SEXP _SGDinference_sgd_lm_cpp(SEXP xSEXP, SEXP ySEXP, SEXP burnSEXP, SEXP gamma_0SEXP, SEXP alphaSEXP, SEXP bt_startSEXP, SEXP x_meanSEXP, SEXP x_sdSEXP, SEXP pathSEXP, SEXP path_indexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const int& >::type burn(burnSEXP);
    Rcpp::traits::input_parameter< const double& >::type gamma_0(gamma_0SEXP);
    Rcpp::traits::input_parameter< const double& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type bt_start(bt_startSEXP);
    Rcpp::traits::input_parameter< const arma::rowvec& >::type x_mean(x_meanSEXP);
    Rcpp::traits::input_parameter< const arma::rowvec& >::type x_sd(x_sdSEXP);
    Rcpp::traits::input_parameter< const bool& >::type path(pathSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type path_index(path_indexSEXP);
    rcpp_result_gen = Rcpp::wrap(sgd_lm_cpp(x, y, burn, gamma_0, alpha, bt_start, x_mean, x_sd, path, path_index));
    return rcpp_result_gen;
END_RCPP
}
// sgd_qr_cpp
List sgd_qr_cpp(const arma::mat& x, const arma::colvec& y, const int& burn, const double& gamma_0, const double& alpha, const arma::colvec& bt_start, const double& tau, const arma::rowvec& x_mean, const arma::rowvec& x_sd, const bool& path, const arma::colvec& path_index);
RcppExport SEXP _SGDinference_sgd_qr_cpp(SEXP xSEXP, SEXP ySEXP, SEXP burnSEXP, SEXP gamma_0SEXP, SEXP alphaSEXP, SEXP bt_startSEXP, SEXP tauSEXP, SEXP x_meanSEXP, SEXP x_sdSEXP, SEXP pathSEXP, SEXP path_indexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const int& >::type burn(burnSEXP);
    Rcpp::traits::input_parameter< const double& >::type gamma_0(gamma_0SEXP);
    Rcpp::traits::input_parameter< const double& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type bt_start(bt_startSEXP);
    Rcpp::traits::input_parameter< const double& >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< const arma::rowvec& >::type x_mean(x_meanSEXP);
    Rcpp::traits::input_parameter< const arma::rowvec& >::type x_sd(x_sdSEXP);
    Rcpp::traits::input_parameter< const bool& >::type path(pathSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type path_index(path_indexSEXP);
    rcpp_result_gen = Rcpp::wrap(sgd_qr_cpp(x, y, burn, gamma_0, alpha, bt_start, tau, x_mean, x_sd, path, path_index));
    return rcpp_result_gen;
END_RCPP
}
// sgdi_lm_cpp
List sgdi_lm_cpp(const arma::mat& x, const arma::colvec& y, const int& burn, const double& gamma_0, const double& alpha, const arma::colvec& bt_start, const std::string inference, const arma::uvec& rss_idx, const arma::rowvec& x_mean, const arma::rowvec& x_sd, const bool& path, const arma::colvec& path_index);
RcppExport SEXP _SGDinference_sgdi_lm_cpp(SEXP xSEXP, SEXP ySEXP, SEXP burnSEXP, SEXP gamma_0SEXP, SEXP alphaSEXP, SEXP bt_startSEXP, SEXP inferenceSEXP, SEXP rss_idxSEXP, SEXP x_meanSEXP, SEXP x_sdSEXP, SEXP pathSEXP, SEXP path_indexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const int& >::type burn(burnSEXP);
    Rcpp::traits::input_parameter< const double& >::type gamma_0(gamma_0SEXP);
    Rcpp::traits::input_parameter< const double& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type bt_start(bt_startSEXP);
    Rcpp::traits::input_parameter< const std::string >::type inference(inferenceSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type rss_idx(rss_idxSEXP);
    Rcpp::traits::input_parameter< const arma::rowvec& >::type x_mean(x_meanSEXP);
    Rcpp::traits::input_parameter< const arma::rowvec& >::type x_sd(x_sdSEXP);
    Rcpp::traits::input_parameter< const bool& >::type path(pathSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type path_index(path_indexSEXP);
    rcpp_result_gen = Rcpp::wrap(sgdi_lm_cpp(x, y, burn, gamma_0, alpha, bt_start, inference, rss_idx, x_mean, x_sd, path, path_index));
    return rcpp_result_gen;
END_RCPP
}
// sgdi_qr_cpp
List sgdi_qr_cpp(const arma::mat& x, const arma::colvec& y, const int& burn, const double& gamma_0, const double& alpha, const arma::colvec& bt_start, const std::string inference, const double& tau, const arma::uvec& rss_idx, const arma::rowvec& x_mean, const arma::rowvec& x_sd, const bool& path, const arma::colvec& path_index);
RcppExport SEXP _SGDinference_sgdi_qr_cpp(SEXP xSEXP, SEXP ySEXP, SEXP burnSEXP, SEXP gamma_0SEXP, SEXP alphaSEXP, SEXP bt_startSEXP, SEXP inferenceSEXP, SEXP tauSEXP, SEXP rss_idxSEXP, SEXP x_meanSEXP, SEXP x_sdSEXP, SEXP pathSEXP, SEXP path_indexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type y(ySEXP);
    Rcpp::traits::input_parameter< const int& >::type burn(burnSEXP);
    Rcpp::traits::input_parameter< const double& >::type gamma_0(gamma_0SEXP);
    Rcpp::traits::input_parameter< const double& >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type bt_start(bt_startSEXP);
    Rcpp::traits::input_parameter< const std::string >::type inference(inferenceSEXP);
    Rcpp::traits::input_parameter< const double& >::type tau(tauSEXP);
    Rcpp::traits::input_parameter< const arma::uvec& >::type rss_idx(rss_idxSEXP);
    Rcpp::traits::input_parameter< const arma::rowvec& >::type x_mean(x_meanSEXP);
    Rcpp::traits::input_parameter< const arma::rowvec& >::type x_sd(x_sdSEXP);
    Rcpp::traits::input_parameter< const bool& >::type path(pathSEXP);
    Rcpp::traits::input_parameter< const arma::colvec& >::type path_index(path_indexSEXP);
    rcpp_result_gen = Rcpp::wrap(sgdi_qr_cpp(x, y, burn, gamma_0, alpha, bt_start, inference, tau, rss_idx, x_mean, x_sd, path, path_index));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_SGDinference_sgd_lm_cpp", (DL_FUNC) &_SGDinference_sgd_lm_cpp, 10},
    {"_SGDinference_sgd_qr_cpp", (DL_FUNC) &_SGDinference_sgd_qr_cpp, 11},
    {"_SGDinference_sgdi_lm_cpp", (DL_FUNC) &_SGDinference_sgdi_lm_cpp, 12},
    {"_SGDinference_sgdi_qr_cpp", (DL_FUNC) &_SGDinference_sgdi_qr_cpp, 13},
    {NULL, NULL, 0}
};

RcppExport void R_init_SGDinference(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
