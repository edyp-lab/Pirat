# Pirat

## Installation

To install this package from Github:

```
library(devtools)
install_github('prostarproteomics/Pirat')
```

Once the package has been installed, it is necessary to install a Python environment (called rPirat in the package). For this purpose, run: 

```
library(Pirat)
install_pirat()

```

This will install all necessary Python modules and restart the R session. Infos about versions can be obtained with:

```
pirat_config()
```

### RStudio-Anaconda users

If you are already using Anaconda plugged to RStudio, change your RStudio Python interpreter to the one of the r-pirat anaconda environment that has been installed. To do so in your RStudio window, go in "Global Options" -> "Python" -> "python interpreter".


