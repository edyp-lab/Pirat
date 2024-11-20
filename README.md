
[![R-CMD-check-bioc](https://github.com/edyp-lab/Pirat/actions/workflows/check_Bioc.yml/badge.svg)](https://github.com/edyp-lab/Pirat/actions/workflows/check_Bioc.yml)
[![R-CMD-check-bioc](https://github.com/edyp-lab/Pirat/actions/workflows/check-bioc.yml/badge.svg?branch=RELEASE_3_20)](https://github.com/edyp-lab/Pirat/actions/workflows/check-bioc.yml)

# Pirat

## Installation

To install this package from Github:

```
install.packages('remotes')
remotes::install_github('edyp-lab/Pirat')

```

## Test

```
library(Pirat)
data(subbouyssie)
my_pipeline_llkimpute(subbouyssie) 

data(subropers)
nsamples = nrow(subropers$peptides_ab)
my_pipeline_llkimpute(subropers, 
extension = "T",
rna.cond.mask = 1:nsamples, 
pep.cond.mask = 1:nsamples,
max.pg.size.pirat.t = 1)
```


Note: 
If you run the imputation function `my_pipeline_llkimpute()` for the first time after
the installation of the package, Pirat will automatically install the Python 
environment needed to process imputation computations. This function takes a long time to execute 
but need to be run only once.
