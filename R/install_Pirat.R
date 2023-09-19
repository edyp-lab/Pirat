#' Install Pirat and its dependencies
#'
#' `install_Pirat()` installs just the python packages needed by Pirat.
#'
#' @details You may be prompted to download and install miniconda if reticulate
#'   did not find a non-system installation of python. Miniconda is the
#'   recommended installation method for most users, as it ensures that the R
#'   python installation is isolated from other python installations. All python
#'   packages will by default be installed into a self-contained conda or venv
#'   environment named "r-Pirat". Note that "conda" is the only 
#'   supported method on M1 Mac.
#'
#'   If you initially declined the miniconda installation prompt, you can later
#'   manually install miniconda by running [`reticulate::install_miniconda()`].
#'
#' @section Custom Installation: `install_Pirat()` isn't required to use Pirat.
#'  If you manually configure a python environment with the required 
#'  dependencies and Python environment, you can tell R to use it by pointing 
#'  reticulate at it, commonly by setting an environment variable:
#'
#'   ``` R
#'   Sys.setenv("RETICULATE_PYTHON" = "~/path/to/python-env/bin/python")
#'   ```
#'
#' @md
#' 
#' @param method xxx
#' @param conda xxx
#' @param envname xxx
#'
#' @param restart_session Whether to restart R session after installing. Note that 
#' it will be automatically if in RStudio.
#'
#' @param pip_ignore_installed Whether pip should ignore installed python
#'   packages and reinstall all already installed python packages. This defaults
#'   to `TRUE`, to ensure that Pirat dependencies like NumPy are compatible
#'   with the prebuilt Pirat binaries.
#'
#' @param new_env If `TRUE`, any existing Python virtual environment and/or
#'   conda environment specified by `envname` is deleted first.
#'
#' @export
#' 
#' @import reticulate
#' 
install_Pirat <- function(
    method = "conda",
    conda = "auto",
    envname = "r-Pirat",
    pip_ignore_installed = FALSE,
    new_env = identical(envname, "r-Pirat"),
    restart_session = TRUE
    ) {
  
  method <- match.arg(method)
  python_version <- "3.9.5"
  pytorch_version <- "1.10.0"
  extra_packages <- c("numpy", 'matplotlib')
  
  
 # if(is.null(reticulate::virtualenv_starter(version = python_version, all = FALSE)))
 #   reticulate::install_python(version = python_version)
  
  if (isTRUE(new_env)) {
    
    if (method == "conda") {
      if (!is.null(tryCatch(conda_python(envname, conda = conda),
                            error = function(e) NULL)))
        reticulate::conda_remove(envname, conda = conda)
    }
    
  }
  
  
  
   # install_pytorch(
   #   method = 'conda',
   #   # conda = "auto",
   #   version = '1.10.0',
   #   # channel = 'torch',
   #   envname = envname,
   #   extra_packages = extra_packages,
   #   conda_python_version = python_version)
  
  
  
  reticulate::py_install(
    packages = c("numpy=1.20.2", 'matplotlib', 'pytorch=1.10.0'),
    envname = envname,
    method = 'conda',
    conda = 'auto',
    python_version = '3.9.5'
  )
  

  
  cat("\nInstallation complete.\n\n")
  
  
  is.rstudio <- function(){
    .Platform$GUI == "RStudio"
  }
  
  
  
  if (restart_session){
    if (is.rstudio() &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()
    else
      cat('Please restart the R session.')
  }
  
  invisible(NULL)
}

