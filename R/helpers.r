## Returns capture trap numbers.
trapvec <- function(capthist){
    x <- apply(capthist, 3, function(x) sum(x > 0))
    rep(1:length(x), times = x)
}

## Returns capture animal ID numbers.
animalIDvec <- function(capthist){
    x <- c(apply(capthist, 3, function(x) which(x > 0)), recursive = TRUE)
    names(x) <- NULL
    as.character(x)
}

#' Assigning ID numbers to sounds
#'
#' Identifies recaptures and assigns ID numbers to sounds recorded for
#' an SECR model.
#'
#' Detected sounds are assumed to come from the same animal if times
#' of arrival at different microphones are closer together than the
#' time it would take for sound to travel between these microphones.
#'
#' @param mics a matrix containing the coordinates of trap locations.
#' @param dets a data frame containing (at least): (i) \code{$toa},
#'     the precise time of arrival of the received sound, and (ii)
#'     \code{$trap} the trap at which the sound was recorded.
#' @param sound.speed the speed of sound in metres per second.
#' @return A data frame. Specifically, the \code{dets} dataframe, now
#'     with a new variable, \code{ID}.
#' @author David Borchers
#'
#' @export
make.acoustic.captures <- function(mics, dets, sound.speed){
    mics <- as.matrix(mics)
    dists <- distances(mics, mics)
    dt <- dists/sound.speed
    K <- dim(mics)[1]
    captures <- dets
    ct <- rep(-Inf, K)
    ID <- 1
    ct[dets$trap[1]] <- dets$toa[1]
    new <- FALSE
    ndets <- length(dets$toa)
    for (i in 2:ndets){
        if (ct[dets$trap[i]] > -Inf){
            nd <- length(which(ct > -Inf))
            captures$ID[(i - nd):(i - 1)] <- ID
            ct <- rep(-Inf, K)
            ct[dets$trap[i]] <- dets$toa[i]
            ID <- ID + 1
            if(i == ndets) captures$ID[i] <- ID
        }
        else {
            ct[dets$trap[i]] <- dets$toa[i]
            ctset <- which(ct > -Inf)
            dts <- dt[ctset, dets$trap[i]]
            cts <- -(ct[ctset] - dets$toa[i])
            if (any((cts - dts) > 0)) new <- TRUE
            if (new) {
                nd <- length(which(ct > -Inf)) - 1
                captures$ID[(i - nd):(i - 1)] <- ID
                ct <- rep(-Inf, K)
                ct[dets$trap[i]] <- dets$toa[i]
                ID <- ID + 1
                new <- FALSE
                if (i == ndets) captures$ID[i] <- ID
            } else if(i == ndets){
                nd <- length(which(ct > -Inf))
                captures$ID[(i - nd + 1):i] <- ID
            }
        }
    }
    captures
}

allocate.calls <- function(mics, dets, sound.speed){
    mics <- as.matrix(mics)
    trap.dists <- distances(mics, mics)
    n.dets <- nrow(dets)
    ## Allocating pairwise plausibility of common cue sources.
    dist.mat <- detection_dists(trap.dists, dets$trap)
    timediff.mat <- detection_timediffs(dets$toa, dets$trap)
    maxtime.mat <- dist.mat/sound.speed
    match.mat <- timediff.mat <= maxtime.mat
    ## Finding blocks of multiple cues with possible common sources.
    incomplete.blocks <- find_incomplete_blocks(match.mat)
    n.blocks <- max(incomplete.blocks)
    complete.block <- logical(n.blocks)
    final.mat <- matrix(FALSE, nrow = n.dets, ncol = n.dets)
    ## Allocating possible common cues to sources.
    reqss.mat <- dist.mat/timediff.mat
    for (i in 1:max(incomplete.blocks)){
        ## Grabbing a block.
        block <- match.mat[incomplete.blocks == i, incomplete.blocks == i]
        reqss <- reqss.mat[incomplete.blocks == i, incomplete.blocks == i]
        ## Working out if there is any possible ambiguity.
        is.complete <- all(block)
        ## If ambiguity, resolve it.
        if (!is.complete){
            block <- blockify(block, reqss)
        }
        final.mat[incomplete.blocks == i, incomplete.blocks == i] <- block
    }
    find_incomplete_blocks(final.mat)
}

## Adapted from R2admb.
read.ascr <- function(fn, verbose = FALSE, checkterm = TRUE){
    if (verbose)
        cat("reading output ...\n")
    parfn <- paste(fn, "par", sep = ".")
    if (!file.exists(parfn))
        stop("couldn't find parameter file ", parfn)
    L <- c(list(fn = fn), read_pars(fn))
    if (checkterm) {
        v <- with(L, vcov[seq(npar), seq(npar)])
        ev <- try(eigen(solve(v))$value, silent = TRUE)
        L$eratio <- if (inherits(ev, "try-error"))
            NA
        else min(ev)/max(ev)
    }
    class(L) <- "admb"
    L
}

#' Extract estimated or fixed parameter values from an \code{ascr} model fit
#'
#' Extracts estimated, derived, and fitted parameters from a model
#' fitted using \link{fit.ascr}.
#'
#' This is a similar function to \link{coef.ascr}, however
#' \code{get.par} also allows for extraction of parameters that have
#' been fixed using \code{fit.ascr}'s \code{fix} argument.
#'
#' @param pars A character vector containing names of parameter values
#'     to be extracted. Alternatively, the character string
#'     \code{"all"} will extract all parameters, fixed or otherwise,
#'     and the character string \code{"fitted"} extracts only fitted
#'     parameters (i.e., not the effective survey area). See the
#'     'Details' section for the \link{fit.ascr} function's
#'     documentation for information on the parameters that are
#'     fitted.
#' @param cutoff Logical, if \code{TRUE}, the cutoff value for an
#'     acoustic fit is included.
#' @param as.list Logical, if \code{TRUE}, each parameter value is
#'     returned as the component of a list, where component names give
#'     the parameter names. In this case, the returned object is
#'     suitable as the argument of a variety of functions (e.g.,
#'     \code{sv} or \code{fix} for the \link{fit.ascr} function, or
#'     \code{pars} for the \link{sim.capt} function). If \code{FALSE},
#'     parameter values are returned as part of a named vector.
#' @inheritParams locations
#'
#' @return See above information about the argument \code{as.list}. If
#'     \code{as.list} is \code{TRUE}, then a list is returned. If
#'     \code{as.list} is \code{FALSE}, then a named character vector
#'     is returned.
#'
#' @examples
#' get.par(fit = example$fits$simple.hn, pars = "all")
#' get.par(fit = example$fits$bearing.hn, pars = c("D", "kappa", "esa"), as.list = TRUE)
#'
#' @export
get.par <- function(fit, pars = "all", cutoff = FALSE, as.list = FALSE){
    allpar.names <- c("D", fit$detpars, fit$suppars, "esa")
    if (length(pars) == 1){
        if (pars == "all"){
            pars <- allpar.names
        } else if (pars == "fitted"){
            pars <- allpar.names[allpar.names != "esa"]
        }
    }
    ## Error checking.
    legal.names <- pars %in% allpar.names
    if (!all(legal.names)){
        illegal.pars <- pars[!legal.names]
        if (sum(!legal.names) == 1){
            msg <- paste(illegal.pars, "is not a parameter in the model provided.")
        } else if (sum(!legal.names) == 2){
            msg <- paste(paste(illegal.pars, collapse = " and "),
                         "are not parameters in the model provided.")
        } else if (sum(!legal.names) > 2){
            n.illegal <- length(illegal.pars)
            msg <- paste(paste(illegal.pars[-n.illegal], collapse = ", "),
                         ", and", illegal.pars[n.illegal],
                         "are not parameters in the model provided.")
        }
        stop(msg)
    }
    if (!fit$fit.types["ss"] & cutoff){
        warning("The cutoff is not being provided as 'fit' does not use signal strength information.")
        cutoff <- FALSE
    }
    out <- numeric(length(pars))
    names(out) <- pars
    det.index <- which(fit$detpars %in% pars)
    supp.index <- which(fit$suppars %in% pars)
    ## Logical vector indicating parameters that weren't estimated.
    phases <- fit$phases
    phases$esa <- 0
    fixed.pars <- phases[pars] == -1
    ## Putting in fixed parameter values.
    if (sum(fixed.pars) > 0){
        out[fixed.pars] <- c(fit$args$sv[pars[fixed.pars]], recursive = TRUE)
    }
    ## Working out parameter groups for parameters in 'pars'.
    det.index <- which(pars %in% fit$detpars)
    supp.index <- which(pars %in% fit$suppars)
    admb.pars <- pars
    ## Putting in estimated parameter values.
    out[!fixed.pars] <- fit$coefficients[admb.pars[!fixed.pars]]
    ## Adding the cutoff if necessary.
    if (cutoff){
        out <- c(out, fit$args$ss.opts$cutoff)
        names(out) <- c(pars, "cutoff")
    }
    if (as.list){
        out.vec <- out
        out <- vector("list", length = length(out.vec))
        names(out) <- names(out.vec)
        names(out.vec) <- NULL
        for (i in 1:length(out.vec)){
            out[[i]] <- out.vec[i]
        }
    }
    out
}

#' Extracting mask point locations
#'
#' Extracts the mask used in an ascr fit.
#'
#' @inheritParams locations
#'
#' @return A mask object.
#'
#' @export
get.mask <- function(fit){
    fit$args$mask
}

#' Extracting trap locations
#'
#' Extracts the trap locations used in an ascr fit.
#'
#' @inheritParams locations
#'
#' @return A traps object.
#'
#' @export
get.traps <- function(fit){
    fit$args$traps
}

#' Extracting capture history object
#'
#' Extracts the capture history object used in an ascr fit.
#'
#' @inheritParams locations
#'
#' @return A capture history object.
#'
#' @export
get.capt <- function(fit){
    fit$args$capt
}

## Error function.
erf <- function(x){
    2*pnorm(x*sqrt(2)) - 1
}

#' Calculating detection probabilities.
#'
#' Calculates the probability of detection by at least one detector
#' for specific locations in the survey area.
#'
#' @param fit A fitted model from \link{fit.ascr}.
#' @param esa Logical, if \code{TRUE} the effective sampling area is
#'     returned instead of capture probabilities.
#' @param points A matrix with two columns. Each row provides
#'     Cartesian coordinates for the location of a point at which a
#'     capture probability should be returned.
#' @param traps A matrix with two columns. Each row provides Cartesian
#'     coordinates for the location of a trap (or detector). Ignored
#'     if \code{fit} is not \code{NULL}.
#' @param detfn A character string specifying the detection function
#'     to be used. One of "hn" (halfnormal), "hr" (hazard rate), "th"
#'     (threshold), "lth" (log-link threshold), or "ss" (signal
#'     strength). Ignored if \code{fit} is not \code{NULL}.
#' @param ss.link A character string, either \code{"identity"},
#'     \code{"log"}, or \code{"spherical"}, which specifies the
#'     relationship between the expected received signal strength and
#'     distance from the microphone. See the documentation for
#'     \link{fit.ascr} for further details. Ignored if \code{fit} is
#'     not \code{NULL}.
#' @param pars A named list. Component names are parameter names, and
#'     each component is a value for the associated parameter. Ignored
#'     if \code{fit} is not \code{NULL}.
#' @param n.quadpoints An integer, giving the number of quadrature
#'     points used for numerical integration over the possible call
#'     directions.
#'
#' @return A vector containing detection probabilities for each
#'     location in \code{points}.
#'
#' @export
p.dot <- function(fit = NULL, esa = FALSE, points = get.mask(fit), traps = NULL,
                  detfn = NULL, ss.link = NULL, pars = NULL, n.quadpoints = 8){
    if (!is.null(fit)){
        traps <- get.traps(fit)
        detfn <- fit$args$detfn
        pars <- get.par(fit, fit$detpars, cutoff = fit$fit.types["ss"], as.list = TRUE)
        ss.link <- fit$args$ss.opts$ss.link
        re.detfn <- fit$re.detfn
    } else {
        re.detfn <- FALSE
        if (detfn == "ss"){
            if (pars$b2.ss != 0 | pars$sigma.b0.ss != 0){
                re.detfn <- TRUE
            }
        }
    }
    dists <- distances(traps, points)
    ## Calculating probabilities of detection when random effects are
    ## in detection function. Detections at traps no longer
    ## independent.
    if (re.detfn){
        if (!is.null(pars$sigma.b0.ss)){
            if (pars$sigma.b0.ss != 0){
                stop("Function p.dot() has not yet been implemented for models with heterogeneous source strengths.")
            }
        }
        n.traps <- nrow(traps)
        n.points <- nrow(points)
        dirs <- (0:(n.quadpoints - 1))*2*pi/n.quadpoints
        probs <- numeric(n.points)
        ## Integrating over all possible directions.
        ## TODO: Write all this in C++.
        for (i in 1:n.quadpoints){
            dir <- dirs[i]
            bearings <- bearings(traps, points)
            orientations <- abs(dir - bearings)
            for (j in 1:n.points){
                ## Probabilities of detection given orientation.
                o.prob <- numeric(n.traps)
                for (k in 1:n.traps){
                    o.prob[k] <- calc.detfn(dists[k, j], detfn, pars, ss.link,
                                            orientations[k, j])
                }
                probs[j] <- probs[j] + (1/n.quadpoints)*(1 - prod(1 - o.prob))
            }
        }
        out <- probs
    } else {
        probs <- calc.detfn(dists, detfn, pars, ss.link)
        out <- aaply(probs, 2, function(x) 1 - prod(1 - x))
    }
    if (esa){
        A <- attr(points, "area")
        if (is.null(A)){
            stop("The argument 'points' must be a mask object if ESA is to be calculated.")
        }
        out <- A*sum(out)
    }
    out
}

## Link functions for fit.ascr() function.
log.link <- function(x){
    x <- pmax(x, .Machine$double.eps)
    log(x)
}

logit.link <- function(x){
    x <- pmax(x, .Machine$double.eps)
    x <- pmin(x, 1 - .Machine$double.neg.eps)
    log(x/(1 - x))
}

scaled.logit.link <- function(x){
    (x/10)^3
}

inv.logit <- function(x){
    exp(x)/(exp(x) + 1)
}

inv.scaled.logit.link <- function(x){
    x <- inv.logit(x)
    2*1e8*x - 1e8
}

scaled.log.link <- function(x){
    x <- x + 1e8
    log(x)
}

#' Extracting Monte Carlo error
#'
#' Extracts calculated Monte Carlo errors from a bootstrap procedure
#' carried out by \link{boot.ascr}.
#'
#' @param estimate A character string, either \code{"bias"} or
#'     \code{"se"}, which determines whether Monte Carlo errors for
#'     bias estimates or standard errors are reported.
#' @inheritParams locations
#'
#' @seealso \link{boot.ascr} for the bootstrap procedure.
#' @seealso \link{stdEr.ascr.boot} for standard errors.
#' @seealso \link{get.bias} for estimated biases.
#'
#' @export
get.mce <- function(fit, estimate){
    if (estimate == "bias"){
        out <- fit$boot$bias.mce
    } else if (estimate == "se"){
        out <- fit$boot$se.mce
    } else {
        stop("The argument 'estimate' must be either \"bias\" or \"se\"")
    }
    out
}

#' Extracting estimated biases
#'
#' Extracts bias in parameter estimates, calculated using the
#' bootstrap procedure carried out by \link{boot.ascr}.
#'
#' @inheritParams locations
#' @inheritParams coef.ascr
#' @inheritParams stdEr.ascr.boot
#'
#' @seealso \link{boot.ascr} for the bootstrap procedure.
#' @seealso \link{get.mce} for Monte Carlo error the biases are
#'     subject to.
#'
#' @export
get.bias <- function(fit, pars = "fitted", mce = FALSE){
    if ("all" %in% pars){
        pars <- c("fitted", "derived", "linked")
    }
    par.names <- names(fit$coefficients)
    if (!all(pars %in% c("fitted", "derived", "linked", par.names))){
        stop("Argument 'pars' must either contain a vector of parameter names, or a subset of \"fitted\", \"derived\", \"linked\", and \"all\".")
    }
    mces <- get.mce(fit, estimate = "bias")
    if (any(c("fitted", "derived", "linked") %in% pars)){
        which.linked <- grep("_link", par.names)
        linked <- fit$boot$bias[which.linked]
        which.derived <- which(par.names == "esa" | par.names == "Da")
        derived <- fit$boot$bias[which.derived]
        fitted <- fit$boot$bias[-c(which.linked, which.derived)]
        out <- mget(pars)
        names(out) <- NULL
        out <- c(out, recursive = TRUE)
    } else {
        out <- fit$boot$bias[pars]
    }
    if (mce){
        out.vec <- out
        out <- cbind(out.vec, mces[names(out)])
        rownames(out) <- names(out.vec)
        colnames(out) <- c("Bias", "MCE")
    }
    out
}

#' Function to turn data frame into capture history matrix
#' @param data A data.frame with at least the following named columns:
#' \code{occasion}, numeric day or time index;
#' \code{post}, a factor vector of post (trap) names,
#' and \code{group}. It is the pairing of
#' \code{group} and \code{occasion} that forms the individual
#' "call id". Optional to include a column of bearings (in radians)
#' and/or distances.
#' @export 
get.capt.hist <- function(data){
    occasion <- data$occasion
    post <- data$post
    group <- data$group
    cantor <- 1/2 * (occasion + group)* (occasion + group + 1) + group
    tmp <- data.frame(array = rep(1,nrow(data)), ID = cantor,
                      occasion = occasion, trap = as.numeric(data$post))
    if("bearing" %in% names(data)) {tmp$bearing <- data$bearing}
    if("distance" %in% names(data)) {tmp$dist <- data$distance}
    tmp <- tmp[order(tmp$ID),]
    capt.hist <- create.capt(tmp,n.traps = length(table(tmp$trap)))
    capt.hist
}
#' Function to plot mask along with trap locations in a 'tidy' presentable manner
#' @param mask mask object from \code{create.mask}
#' @param traps a matrix of trap locations used to create the \link{mask}
#' 
#' @export

show.mask <- function(mask = NULL,traps = NULL){
    plot(mask,asp = 1,pch = 20,col = "grey",
         xlab = "Longitude",ylab = "Latitude")
    points(traps,cex = 2,pch = 4,lwd = 3)
}

#' Function to plot Von Mises distribution of bearing measurement error
#' from model.
#' @param fit ascr model
#'
#' @export
show.dvm <- function(fit = NULL){
    theta = sort(fit$args$capt$bearing - pi)
    val <- dvm(theta = theta,mu = 0, kappa = fit$coefficients["kappa"])
    plot(theta,val, type="l",xlim = c(-pi/2,pi/2),ylim = range(0,max(val)), main = "", axes = FALSE, xlab = "bearings (rad)", ylab = "")
    axis(1, at = c(-pi/2,0,pi/2), labels = c(expression(-pi/2),0,expression(pi/2)))
    axis(2)
}

#' Function to plotGamma distridution of distance measurtment error
#' from model.
#' @param fit ascr model
#' @param d distance of call/animal at which to plot
#' @export
show.distgam <- function(fit = NULL,d = NULL){
    x <- sort(fit$args$capt$dist)
    val <- dgamma(x = x, shape = fit$coefficients["alpha"],
                  scale = d/fit$coefficients["alpha"])
    plot(x,val, type="l",ylim = range(val), main = "", axes = FALSE, xlab = "distance (m)", ylab = "")
    axis(1)
    axis(2)    
}
    
#' Function to show capture history data, can also be used alongside model fit information
#' to show estimated locations
#' @param traps a matrix of trap locations
#' @param capt.hist a list of capture history information, an object returned by \link{get.capt.hist}
#' @param xlim x-axis limits of plot, by default this is based on trap locations
#' @param ylim y-axis limits of plot, by default this is based on trap locations
#' @param id call number to display, by default this is 1
#' @param show.axes logical, whhether to include plot axes
#' @examples
#' \dontrun{
#' detections <- shiny_example_detections
#' traps <- shiny_example_traps
#' detections <- shiny_example_detections
#' show.data(traps, capt.hist)}
#' @export
show.data <- function(traps, capt.hist, xlim = NULL,ylim = NULL, id = 1,show.axes = FALSE){
    if(is.null(xlim)){
        xrang = range(traps[,1])
        diff = diff(xrang)
        xlim =  c(xrang[1] - diff,xrang[2] + diff)
    }
    if(is.null(ylim)){
        yrang = range(traps[,2])
        diff = diff(yrang)
        ylim =  c(yrang[1] - diff,yrang[2] + diff)
    }
    traps <- cbind(traps$x,traps$y)
    capt <- capt.hist$bincapt[id,]
    plot.new()
    plot.window(xlim = xlim, ylim = ylim, asp = 1)
    box()
    if (show.axes){
            axis(1)
            axis(2)
        }
    points(traps, col = 1, pch = 4, lwd = 2)
    points(traps[capt == 1, , drop = FALSE], col = 1, cex = 2, lwd = 2)
    if("bearing"%in%names(capt.hist) & !("dist"%in%names(capt.hist))){
        bearing = capt.hist$bearing[id, ]
        arrow.length = 0.05*min(c(diff(range(xlim)), diff(range(ylim))))
        bearing.capt = bearing[capt == 1]
        trappos = traps[which(capt == 1), , drop = FALSE]
        sinb = sin(bearing.capt)*arrow.length
        cosb = cos(bearing.capt)*arrow.length
        arrows(trappos[, 1], trappos[, 2], trappos[, 1] + sinb, trappos[, 2] + cosb,
               length = 0.1, col = 1, lwd = 2)
    }
    if("dist"%in%names(capt.hist) & !("bearing"%in%names(capt.hist))){
        distance = capt.hist$dist[id, ]
        dist.capt = distance[ capt == 1]
        trappos = traps[which(capt == 1), , drop = FALSE]
        for (i in 1:nrow(trappos)){
            centre = trappos[i, ]
            radius = dist.capt[i]
            circles(as.numeric(centre), radius, col = 1, lwd = 2)
        }
    }
    if("dist"%in%names(capt.hist) & "bearing"%in%names(capt.hist)){
        bearing = capt.hist$bearing[id, ]
        distance = capt.hist$dist[id, ]
        arrow.length = distance[ capt == 1]
        bearing.capt = bearing[capt == 1]
        trappos = traps[which(capt == 1), , drop = FALSE]
        sinb = sin(bearing.capt)*arrow.length
        cosb = cos(bearing.capt)*arrow.length
        arrows(trappos[, 1], trappos[, 2], trappos[, 1] + sinb, trappos[, 2] + cosb,
               length = 0.1, col = 1, lwd = 2)
    }
}

