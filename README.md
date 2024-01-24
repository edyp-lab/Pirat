# Pirat


## Prerequest

The functions in Pirat use some Python libraries. Before installing Pirat, it is necessary to have Python 3.9.5 installed on the computer.
This version can be found here:
https://www.python.org/downloads/release/python-395/


## Installation

To install this package from Github:

```
library(remotes)
remotes::install_github('prostarproteomics/Pirat')
```

Once the package has been installed, it is necessary to install a Python environment (called r-pirat in the package) with specific versions of Python libraries. For this purpose, run: 

```
library(Pirat)
install_pirat()

```

This will install all necessary Python modules and restart the R session. Infos about versions can be obtained with:

```
pirat_config()
```

### Anaconda users

If you are already using Anaconda plugged to RStudio, change your RStudio Python interpreter to the one of the r-pirat anaconda environment that has been installed. To do so in your RStudio window, go in "Global Options" -> "Python" -> "python interpreter".
