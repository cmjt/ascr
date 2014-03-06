#' Simulating SECR data
#'
#' Simulates SECR capture histories and associated additional
#' information in the correct format for use with
#' \code{\link[admbsecr]{admbsecr}}.
#'
#' If \code{fit} is provided then no other arguments are
#' required. Otherwise, at least \code{traps}, \code{mask}, and
#' \code{pars} are needed.
#'
#' @param fit A fitted \code{admbsecr} model object which provides the
#' additional information types, detection function, and parameter
#' values from which to generate capture histories.
#' @param traps A matrix with two columns. The rows provide Cartesian
#' coordiates for trap locations.
#' @param mask A matrix with two columns. The rows provide Cartesian
#' coordinates for the mask point locations.
#' @param infotypes A character vector indicating the type(s) of
#' additional information to be simulated. Elements can be a subset of
#' \code{"ang"}, \code{"dist"}, \code{"ss"}, \code{"toa"}, and
#' \code{"mrds"} (NOTE: \code{"mrds"} not yet implemented).
#' @param detfn A character string specifying the detection function
#' to be used. Options are "hn" (halfnormal), "hr" (hazard rate), "th"
#' (threshold), "lth" (log-link threshold), or "ss" (signal strength).
#' @param pars A named list. Component names are parameter names, and
#' each component is the value of the associated parameter. A value
#' for the parameter \code{D}, animal density (or call density, if it
#' an acoustic survey) must always be provided, along with values for
#' parameters associated with the chosen detection function and
#' additional information type(s).
#' @param ss.link A character string, either \code{"indentity"} or
#' \code{"log"}, which specifies the link function for the signal
#' strength detection function. Only required when \code{detfn} is
#' \code{"ss"}.
#' @param cutoff The signal strength threshold, above which sounds are
#' identified as detections. Only required when \code{detfn} is
#' \code{"ss"}.
#' @param call.freqs A vector of call frequencies collected
#' independently to an acoustic survey.
#' @param freq.dist A character string, either \code{"edf"} or
#' \code{"norm"}, which specifies how the distribution function of the
#' call frequencies should be estimated. If \code{"edf"}, then the
#' distribution of call frequencies is estimated using the empirical
#' distribution function. If \code{"norm"}, then a normal distribution
#' is fitted to the call frequencies using the sample mean and
#' variance.
#' @param sound.speed The speed of sound in metres per second,
#' defaults to 330 (the speed of sound in air). Only used when
#' \code{info} includes \code{"toa"}.
#' @param test.detfn Logical value, if \code{TRUE}, tests detection
#' function to aid debugging.
#' @export
sim.capt <- function(fit = NULL, traps = NULL, mask = NULL,
                     infotypes = character(0), detfn = "hn",
                     pars = NULL, ss.link = "identity",
                     cutoff = NULL, call.freqs = NULL, freq.dist = "edf",
                     sound.speed = 330, test.detfn = FALSE){
    ## Some error checking.
    if (any(infotypes == "ss")){
        stop("Signal strength information is simulated by setting argument 'detfn' to \"ss\".")
    }
    if (!missing(cutoff) & detfn != "ss"){
        warning("The argument 'cutoff' is being ignored, as 'detfn' is not \"ss\".")
    }
    if (!missing(ss.link) & detfn != "ss"){
        warning("The argument 'ss.link' is being ignored, as 'detfn' is not \"ss\".")
    }
    ## Grabbing values from fit if required.
    if (!is.null(fit)){
        traps <- get.traps(fit)
        mask <- get.mask(fit)
        infotypes <- fit$infotypes
        detfn <- fit$args$detfn
        pars <- get.par(fit, "all", as.list = TRUE)
        ss.link <- fit$args$ss.link
        cutoff <- fit$args$cutoff
        call.freqs <- fit$args$call.freqs
        sound.speed <- fit$args$sound.speed
    }
    ## Setting up logical indicators for additional information types.
    supp.types <- c("ang", "dist", "ss", "toa", "mrds")
    sim.types <- supp.types %in% infotypes
    names(sim.types) <- supp.types
    sim.angs <- sim.types["ang"]
    sim.dists <- sim.types["dist"]
    sim.toas <- sim.types["toa"]
    sim.mrds <- sim.types["mrds"]
    sim.ss <- ifelse(detfn == "ss", TRUE, FALSE)
    ## Working out required parameters.
    suppar.names <- c("kappa", "alpha", "sigma.toa")[sim.types[c("ang", "dist", "toa")]]
    if (sim.ss){
        if (ss.link == "identity"){
            detfn <- "ss"
        } else if (ss.link == "log"){
            detfn <- "log.ss"
        } else {
            stop("The argument 'ss.link' must be either \"identity\" or \"log\"")
        }
    }
    detpar.names <- switch(detfn,
                           hn = c("g0", "sigma"),
                           hr = c("g0", "sigma", "z"),
                           th = c("shape", "scale"),
                           lth = c("shape.1", "shape.2", "scale"),
                           ss = c("b0.ss", "b1.ss", "sigma.ss"),
                           log.ss = c("b0.ss", "b1.ss", "sigma.ss"))
    par.names <- c("D", detpar.names, suppar.names)
    if (!identical(sort(par.names), sort(names(pars)))){
        msg <- paste("The following must be named components of the list 'pars': ",
                     paste(par.names, collapse = ", "), ".", sep = "")
        stop(msg)
    }
    ## Grabbing detection function parameters.
    detpars <- pars[detpar.names]
    ## Specifies the area in which animal locations can be generated.
    core <- data.frame(x = range(mask[, 1]), y = range(mask[, 2]))
    ## Simulating population.
    if (is.null(call.freqs)){
        popn <- as.matrix(sim.popn(D = pars$D, core = core, buffer = 0))
    } else {
        D <- pars$D/mean(call.freqs)
        popn <- as.matrix(sim.popn(D = D, core = core, buffer = 0))
        n.a <- nrow(popn)
        if (freq.dist == "edf"){
            if (length(call.freqs) == 1){
                freqs <- rep(call.freqs, n.a)
            } else {
                freqs <- sample(call.freqs, size = n.a, replace = TRUE)
            }
        } else if (freq.dist == "norm"){
            if (diff(range(call.freqs)) == 0){
                freqs <- rep(unique(call.freqs), n.a)
            } else {
                freqs <- round(rnorm(n.a, mean(call.freqs), sd(call.freqs)))
            }
        } else {
            stop("The argument 'freq.dist' must be either \"edf\" or \"norm\"")
        }
        popn <- popn[rep(1:n.a, times = freqs), ]
    }
    n.popn <- nrow(popn)
    if (n.popn == 0) stop("No animals in population.")
    ## Calculating distances.
    dists <- distances(popn, traps)
    n.traps <- nrow(traps)
    ## Calculating detection probabilities and simulating captures.
    if (!sim.ss){
        det.probs <- calc.detfn(dists, detfn, detpars)
        full.bin.capt <- matrix(as.numeric(runif(n.popn*n.traps) < det.probs),
                                nrow = n.popn, ncol = n.traps)
        captures <- which(apply(full.bin.capt, 1, sum) > 0)
        bin.capt <- full.bin.capt[captures, ]
        out <- list(bincapt = bin.capt)
    } else {
        if (ss.link == "identity"){
            inv.ss.link <- identity
        } else if (ss.link == "log"){
            inv.ss.link <- exp
        } else {
            stop("Argument 'ss.link' must be \"identity\" or \"log\".")
        }
        pars$cutoff <- cutoff
        ss.mean <- inv.ss.link(pars$b0.ss - pars$b1.ss*dists)
        ss.error <- matrix(rnorm(n.popn*n.traps, mean = 0,
                                 sd = pars$sigma.ss),
                           nrow = n.popn, ncol = n.traps)
        full.ss.capt <- ss.mean + ss.error
        captures <- which(apply(full.ss.capt, 1,
                                function(x, cutoff) any(x > cutoff),
                                cutoff = cutoff))
        full.bin.capt <- ifelse(full.ss.capt > cutoff, 1, 0)
        ss.capt <- full.ss.capt[captures, ]
        bin.capt <- ifelse(ss.capt > cutoff, 1, 0)
        ss.capt[ss.capt < cutoff] <- 0
        out <- list(bincapt = bin.capt, ss = ss.capt)
    }
    ## Plot to test correct detection simulation.
    if (test.detfn){
        capt.dists <- dists[full.bin.capt == 1]
        evade.dists <- dists[full.bin.capt == 0]
        all.dists <- c(capt.dists, evade.dists)
        capt.dummy <- c(rep(1, length(capt.dists)),
                        rep(0, length(evade.dists)))
        breaks <- seq(0, max(all.dists), length.out = 100)
        mids <- breaks[-length(breaks)] + 0.5*diff(breaks)
        breaks[1] <- 0
        split.dummy <- split(capt.dummy,
                             f = cut(all.dists, breaks = breaks))
        props <- sapply(split.dummy, mean)
        plot(mids, props, type = "l", xlim = c(0, max(all.dists)),
             ylim = c(0, 1))
        xx <- seq(0, max(all.dists), length.out = 100)
        lines(xx, calc.detfn(xx, detfn, detpars), col = "blue")
    }
    ## Total number of detections.
    n.dets <- sum(bin.capt)
    ## Locations of captured individuals.
    capt.popn <- popn[captures, ]
    ## Capture distances.
    capt.dists <- dists[captures, ]
    ## Simulating additional information.
    if (sim.angs){
        bearings <- t(bearings(traps, capt.popn))
        ang.capt <- matrix(0, nrow = nrow(bin.capt),
                           ncol = ncol(bin.capt))
        ang.capt[bin.capt == 1] <- (bearings[bin.capt == 1] +
                     rvm(n.dets, mean = 0, k = pars$kappa)) %% (2*pi)
        out$ang <- ang.capt
    }
    if (sim.dists){
        dist.capt <- matrix(0, nrow = nrow(bin.capt),
                            ncol = ncol(bin.capt))
        betas <- pars$alpha/capt.dists[bin.capt == 1]
        dist.capt[bin.capt == 1] <- rgamma(n.dets, shape = pars$alpha,
                      rate = betas)
        out$dist <- dist.capt
    }
    if (sim.toas){
        ## Time taken for sound to travel from source to detector.
        toa.capt <- capt.dists/sound.speed*bin.capt
        ## Adding in TOA error.
        toa.capt[bin.capt == 1] <-
            toa.capt[bin.capt == 1] + rnorm(n.dets, sd = pars$sigma.toa)
        out$toa <- toa.capt
    }
    if (sim.mrds){
        out$mrds <- capt.dists
    }
    out
}

