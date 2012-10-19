## Letting R know where everything is.
admbsecr.dir <- "~/admbsecr" # Point this to the admbsecr file.
if (.Platform$OS == "unix"){
  sep <- "/"
} else if (.Platform$OS == "windows") {
  sep <- "\\"
}
admb.dir <- paste(admbsecr.dir, "ADMB", sep = sep)
work.dir <- paste(admbsecr.dir, "Analysis", sep = sep)
func.dir <- paste(admbsecr.dir, "R", sep = sep)
dat.dir <- paste(admbsecr.dir, "Data", sep = sep)

## Choosing dataset to work with; pref is either "Somiedo" or
## "Silvermine", no is either 1 or 2.
pref <- c("Somiedo")
no <- 2
detsname <- paste(pref, paste("dets", no, sep = ""), "csv", sep = ".")
micsname <- paste(pref, paste("mics", no, sep = ""), "csv", sep = ".")

## Running setup code.
setwd(work.dir)
source("saffrogsetup.r")

## Carrying out simple SECR analysis.

## With secr.fit().
simplefit1 <- secr.fit(capt,model=list(D~1, g0~1, sigma~1), mask = mask, verify = FALSE)
## With admbsecr().
simplefit2 <- admbsecr(capt, traps = traps, mask = mask, sv = "auto", admbwd = admb.dir,
                       method = "simple")

## Carrying out signal strength analysis.

## With secr.fit().
ssfit1 <- secr.fit(sscapt,model=list(D~1, g0~1, sigma~1), detectfn = 10, mask = mask,
                  verify = FALSE, steptol = 1e-4)
## With admbsecr().
ssfit2 <- admbsecr(capt.ss, traps = traps, mask, sv = "auto", cutoff = 150,
                   admbwd = admb.dir, method = "ss")
