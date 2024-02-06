# Pirat

## Installation

To install this package from Github:

```
library(remotes)
remotes::install_github('prostarproteomics/Pirat')

```

Once the package has been installed, it is necessary to install a Python environment (called r-pirat in the package) with specific versions of Python libraries. For this purpose, run: 

```
libary(Pirat)
install_pirat()

```

This will install all necessary Python modules and restart the R session. Infos about versions can be obtained with:

```
pirat_config()
```


## Test

```
library(Pirat)
data(subbouyssie)
pipeline_llkimpute(subbouyssie) 

data(subropers)
nsamples = nrow(subropers$peptides_ab)
pipeline_llkimpute(subropers, 
                    extension = "T",
                    rna.cond.mask = 1:nsamples, 
                   pep.cond.mask = 1:nsamples,
                    max.pg.size.pirat.t = 1)
```
