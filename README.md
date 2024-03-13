# Pirat

## Installation

To install this package from Github:

```
library(remotes)
remotes::install_github('prostarproteomics/Pirat')

```

Once the package has been downloaded, it is necessary to install a Python environment with specific versions of Python libraries. For this purpose, run: 

```
library(Pirat)
install_pirat()

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
