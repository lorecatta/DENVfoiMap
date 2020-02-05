
<!-- README.md is generated from README.Rmd. Please edit that file -->
DENVfoiMap
==========

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/lorecatta/DENVfoiMap.svg?branch=master)](https://travis-ci.org/lorecatta/DENVfoiMap) <!-- badges: end -->

The goal of `DENVfoiMap` is to provide a set of functions to run a complete analysis similar to the one described in the following sudy:

[L. Cattarino, I. Rodriguez-Barraquer, N. Imai, D. A. T. Cummings, N. M. Ferguson, Mapping global variation in dengue transmission intensity. Sci. Transl. Med. 12, eaax4144 (2020)](https://stm.sciencemag.org/content/12/528/eaax4144)

A *complete* analysis includes:

-   making global predictions of dengue force of infection at 1/6 degree resolution (approximately 20 km),
-   estimating baseline global dengue burden (number of annual infections, mild febbrile cases and hospitalized cases), and
-   predicting the effect of transmission-reducing type of interventions and the Sanofi Pasteur dengue vaccine on burden.

As the analysis described in the aformentioned publication includes uncertainty estimation and sensitivity analyses of model parameters, which require considerable computing time, the original code was tailored to run on the High Performance Computing Cluster at the Department of Infectious Disease Epidemiology (Imperial College London). `DENVfoiMap` does not aim to perform uncertainty estimation and sensitivity analysis. It however provides a set of functions to run a complete analysis using a single bootstrap sample of the original force of infection dataset. To reduce computation time further I provide an example code which produces predictions only for the country of Brazil. However, for the bold and patient ones, a global dataset of covariates at 1/6 degree resolution is made availabe with `DENVfoiMap`. The global analysis does require some form of parallelization if you wish to run it fast.

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

This [vignette](https://lorecatta.github.io/DENVfoiMap/articles/how_to_run_analysis.html) explaines how to run a complete analysis.

Plase do get in touch (<l.cattarino@imperial.ac.uk>) if you have any queries or encounter issues when using `DENVfoiMap`.
