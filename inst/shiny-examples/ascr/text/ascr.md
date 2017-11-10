Animal abundance and density are often variables of particular interest
in ecology. Wildlife surveys that use either passive acoustic or visual
detection of animals using remote detectors are becoming increasingly
widespread, though obtaining estimates of population size from these can
be challenging. One particular class of methods known as spatially
explicit capture-recapture has shown promise, and there has been a
recent surge in related methodological development.

SECR methods account for heterogeneity in detection probabilities across
individuals due to spatial effects; the closer an individual is located
to a detector, the more likely it is to be detected. Information about
an animal’s location is usually provided by the locations of detectors
that did and did not detect it. In some cases the detectors collect
additional data, such as estimated bearings or distances to the
individual, or an acoustic signal’s received strength or time of
arrival. Incorporating these auxiliary data into SECR models can
substantially increase the precision of abundance and density
estimators. With the deployment of increasingly sophisticated detection
devices, collection of these data are likely to become more commonplace.

`ascr` is an `R` package designed to maximize the use of such
information. Its aim is to make analysis of SECR surveys with auxiliary
data accessible to practitioners – as existing software lacks the
capacity to use most kinds of auxiliary data. Parameter estimation is by
maximum likelihood, and optimization is carried out using an AD Model
Builder executable, providing efficient, stable and accurate results.
