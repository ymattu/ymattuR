// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// mecab_df
DataFrame mecab_df(std::string str, std::string tagger_opt);
RcppExport SEXP _ymattuR_mecab_df(SEXP strSEXP, SEXP tagger_optSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type str(strSEXP);
    Rcpp::traits::input_parameter< std::string >::type tagger_opt(tagger_optSEXP);
    rcpp_result_gen = Rcpp::wrap(mecab_df(str, tagger_opt));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ymattuR_mecab_df", (DL_FUNC) &_ymattuR_mecab_df, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_ymattuR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}