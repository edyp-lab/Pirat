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
Pirat::install_pirat()

```
If Python is not found by R during environment installation, you can either try
```{r}
reticulate::use_python("path_to_python_binary")

```
Or, if you are using RStudio, to set your RStudio Python interpreter to the one installed on your machine (in your RStudio window, go in "Global Options" -> "Python" -> "python interpreter"). Then, retry to install the Python environment r-pirat.


This will install all necessary Python modules and restart the R session. Infos about versions can be obtained with:

```
pirat_config()
```
