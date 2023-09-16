#' Install Pirat and its dependencies
#'
#' `install_Pirat()` installs just the python packages needed by Pirat.
#'
#' @details You may be prompted to download and install miniconda if reticulate
#'   did not find a non-system installation of python. Miniconda is the
#'   recommended installation method for most users, as it ensures that the R
#'   python installation is isolated from other python installations. All python
#'   packages will by default be installed into a self-contained conda or venv
#'   environment named "r-reticulate". Note that "conda" is the only supported
#'   method on M1 Mac.
#'
#'   If you initially declined the miniconda installation prompt, you can later
#'   manually install miniconda by running [`reticulate::install_miniconda()`].
#'
#' @section Custom Installation: `install_Pirat()` isn't required to use Pirat
#'  with the package. If you manually configure a python environment with the 
#'  required dependencies, you can tell R to use it by pointing reticulate at it,
#'   commonly by setting an environment variable:
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
#' @param restart_session Restart R session after installing (note this will
#'   only occur within RStudio).
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
install_Pirat <- function(
    method = c("virtualenv", "conda"),
    conda = "auto",
    envname = "reticulate-Pirat",
    pip_ignore_installed = FALSE,
    new_env = identical(envname, "reticulate-Pirat"),
    restart_session = TRUE
    ) {
  
  method <- match.arg(method)
  python_version <- "3.9.5"
  
  packages <- c("numpy==1.20.2", 'matplotlib', "torch==1.10.0")
  print(method)
  
  if (isTRUE(new_env)) {
    
    if (method  == "virtualenv" && 
        reticulate::virtualenv_exists(envname))
      reticulate::virtualenv_remove(envname = envname, confirm = FALSE)
    
    if (method == "conda") {
      if (!is.null(tryCatch(conda_python(envname, conda = conda),
                            error = function(e) NULL)))
        reticulate::conda_remove(envname, conda = conda)
    }
    
  }
  
  
  
  reticulate::py_install(
    packages = packages,
    envname = envname,
    method = method,
    conda = conda,
    python_version = python_version,
    pip = TRUE,
    pip_ignore_installed = pip_ignore_installed
  )
  
  cat("\nInstallation complete.\n\n")
  
  if (restart_session &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()
  
  invisible(NULL)
}

