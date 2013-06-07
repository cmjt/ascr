#' SECR standard error correction
#'
#' Corrects standard errors from an \code{admbsecr} fit.
#'
#' @param fit an \code{admbsecr} model fit.
#' @param calls a vector containing call frequencies from monitored individuals.
#' @param size number of bootstrap samples.
se.correct <- function(fit, calls, size){
  coefs <- coef(fit)
  npars <- length(coefs)
  res <- matrix(0, nrow = size, ncol = npars + 1)
  names(res) <- c(names(coefs), "maxgrad")
  
  for (i in 1:size){
    capt <- sim.capt(fit, calls = calls)
    bootfit <- admbsecr(capt, traps = fit$traps, mask = fit$mask)
  }
  ## UNFINISHED
}