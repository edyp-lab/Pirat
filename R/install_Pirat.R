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
#' @param envname xxx
#'
#' @param restart_session Whether to restart R session after installing. Note that 
#' it will be automatically if in RStudio.
#'
#' @param new_env If `TRUE`, any existing Python virtual environment and/or
#'   conda environment specified by `envname` is deleted first.
#'
#' @export
#' 
#' @import reticulate
#' 
#' @examples
#' \dontrun{
#' install_Pirat()
#' }
#' 
#' 
install_Pirat <- function(
    envname = "r-Pirat",
    new_env = identical(envname, "r-Pirat"),
    restart_session = TRUE
    ) {
  
  method <- switch(get_os(),
         windows = 'virtualenv',
         linux = 'conda',
         default = {
           warning("OS not detected or managed")
           NULL
         }
  )
  
  
  if (method == 'virtualenv')
    pkgs <- c("numpy==1.20.2", 'matplotlib', 'torch==1.10.0')
  if (method == 'conda')
    pkgs <- c("numpy=1.20.2", 'matplotlib', 'pytorch=1.10.0')
  
  python_version <- "3.9.5"
  
  
  #if(is.null(reticulate::virtualenv_starter(version = python_version, all = FALSE)))
  # reticulate::install_python(version = python_version)
  
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
    packages = pkgs,
    envname = envname,
    method = method,
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
    rstudioapi::restartSession(command='library(Pirat)')
    else
      cat("Please restart the R session and reload the 'Pirat' package.")
  }
  
  invisible(NULL)
}

