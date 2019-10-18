
<!-- README.md is generated from README.Rmd. Please edit that file -->
DENVfoiMap
==========

<!-- badges: start -->
<!-- badges: end -->
The goal of DENVfoiMap is to provide a set of functions to run a basic analysis similar to the one described in

CITATION AND link

As the analysis described in the aformentioned publication includes bootstrapping and sensitivity analyses of model parameters, which required considerable computing time, the original code was tailored to run on the High Performance Computing Cluster at the Department of Infectious Disease Epidemiology (Imperial College London). DENVfoiMap does not aim to exactly replicates that analysis. It however provides a set of functions to make global predictions of dengue force of infection at 1/6 degree resolution and predict the effect of transmission reduction type of interventions and the Sanofi Pasteur dengue vaccine on global burden, using a single bootstrap sample of the original force of infection data.

Installation
------------

Please install `DENVfoiMap` from github with

``` r
devtools::install_github("lorecatta/DENVfoiMap")
```

Load and attach it with

``` r
library(DENVfoiMap)
```

which will load all the functions and the datasets used in the analysis. The datasets loaded with the package include:

-   foi\_data - DESCRIPTION TO COME
-   admin\_covariates -
-   predictor\_rank -
-   age\_structure -
-   endemic\_ID\_0\_ID\_1 -
-   all\_sqr\_covariates -
-   R0\_to\_prop\_cases\_averted\_lookup\_1 -
-   R0\_to\_prop\_cases\_averted\_lookup\_2 -
-   R0\_to\_prop\_hosp\_averted\_lookup\_1 -
-   R0\_to\_prop\_hosp\_averted\_lookup\_2 -
-   R0\_to\_prop\_infections\_averted\_lookup\_1 -
-   R0\_to\_prop\_infections\_averted\_lookup\_2 -

The code for running the analysis is in:

analysis.R

Plase do get in touch (<l.cattarino@imperial.ac.uk>) if you have any queries or encounter issues when using `DENVfoiMap`.
