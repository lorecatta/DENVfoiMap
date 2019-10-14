
<!-- README.md is generated from README.Rmd. Please edit that file -->
DENVfoiMap
==========

<!-- badges: start -->
<!-- badges: end -->
The goal of DENVfoiMap is to provide a set of functions to run a basic analysis similar to the one deescribed in

CITATION

As the analysis described in the aformentioned publication includes bootstrapping and sensitivity analyses of model parameters, which required considerable computing time, the original code was tailored to run on the High Performance Computing Cluster at the Department of Infectious Disease Epidemiology (Imperial College London). DENVfoiMap does not aim to exactly replicates that analysis. It however provides a set of functions to make global predictions of dengue force of infection at 1/6 degree resolution and predict the effect of transmission reduction type of interventions and the Sanofi Pasteur dengue vaccine on global burden, using a single bootstrap sample of the original force of infection data.

Installation
------------

The package is not fully documented yet unfortunately. So the best way to use it is by cloning the github repository and do

``` r
devtools::load_all()
```

which will load all the functions and the datasets used in the analysys. The code for running the analys is in:

/scripts/analysis.R

Plase do get in touch (<l.cattarino@imperial.ac.uk>) if you have any queries or encounter issues when using `DENVfoiMap`.
