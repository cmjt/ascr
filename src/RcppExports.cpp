// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// distances
NumericMatrix distances(const NumericMatrix& a, const NumericMatrix& b);
RcppExport SEXP admbsecr_distances(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const NumericMatrix& >::type a(aSEXP );
        Rcpp::traits::input_parameter< const NumericMatrix& >::type b(bSEXP );
        NumericMatrix __result = distances(a, b);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// bearings
NumericMatrix bearings(const NumericMatrix& a, const NumericMatrix& b);
RcppExport SEXP admbsecr_bearings(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const NumericMatrix& >::type a(aSEXP );
        Rcpp::traits::input_parameter< const NumericMatrix& >::type b(bSEXP );
        NumericMatrix __result = bearings(a, b);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// make_toa_ssq
NumericMatrix make_toa_ssq(const NumericMatrix& capt, const NumericMatrix& dists, const double& sound_speed);
RcppExport SEXP admbsecr_make_toa_ssq(SEXP captSEXP, SEXP distsSEXP, SEXP sound_speedSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const NumericMatrix& >::type capt(captSEXP );
        Rcpp::traits::input_parameter< const NumericMatrix& >::type dists(distsSEXP );
        Rcpp::traits::input_parameter< const double& >::type sound_speed(sound_speedSEXP );
        NumericMatrix __result = make_toa_ssq(capt, dists, sound_speed);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// find_local
List find_local(const IntegerMatrix& capt, const NumericMatrix& dists, const double& buffer);
RcppExport SEXP admbsecr_find_local(SEXP captSEXP, SEXP distsSEXP, SEXP bufferSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const IntegerMatrix& >::type capt(captSEXP );
        Rcpp::traits::input_parameter< const NumericMatrix& >::type dists(distsSEXP );
        Rcpp::traits::input_parameter< const double& >::type buffer(bufferSEXP );
        List __result = find_local(capt, dists, buffer);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// secr_nll
double secr_nll(const NumericVector& pars, const List& dat);
RcppExport SEXP admbsecr_secr_nll(SEXP parsSEXP, SEXP datSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const NumericVector& >::type pars(parsSEXP );
        Rcpp::traits::input_parameter< const List& >::type dat(datSEXP );
        double __result = secr_nll(pars, dat);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
