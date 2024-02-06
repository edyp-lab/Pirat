#' @title Install the package Pirat
#'
#' @description This script installs Python and PyTorch in their requested
#' versions. TSo as to be compliant with different OS, it only uses a conda
#' environment. This script is largely inspired by the scripts in the packages 
#' rTorch (https://github.com/f0nzie/rTorch) and tensorflow 
#' (https://github.com/rstudio/tensorflow).
#'
#' @param force A boolean (default value is FALSE) indicating whether to erase 
#' a current installation of the package Pirat.
#' 
#' @author Samuel Wieczorek
#' 
#' @examples
#' \dontrun{
#' install_pirat()
#' }
#'
#' @export
#'
install_pirat <- function(force = FALSE) {
    
    method = "conda"
    conda = "auto"
    restart_session = TRUE
    pip_ignore_installed = FALSE
    envname = "r-pirat"
    channel = c("pytorch", "stable", "torch")
    
    packages <- c('numpy==1.20.2', 
                  'matplotlib', 
                  'pytorch==1.10.0', 
                  'cpuonly')
    
    python_version <- '3.9.5'
    
    
    # some special handling for windows
    if (is_windows()) {

      # avoid DLL in use errors
      if (reticulate::py_available()) {
        stop("You should call install_pirat() only in a fresh ",
             "R session that has not yet initialized Pirat (this is ",
             "to avoid DLL in use errors during installation)")
      }

     }
    
    if (!is.null(tryCatch(conda_python(envname, conda = conda),
                              error = function(e) NULL))){
      if (force){
      cat('Pirat is already installed. To force a new installation, 
      set the argument force = TRUE. This will erase the current installation.')
      return()
      }
      reticulate::conda_remove(envname, conda = conda)
      }

    #reticulate::install_python(version = '3.9.5', force = TRUE)
    reticulate::install_miniconda(force = TRUE)
    
    reticulate::conda_install(packages       = packages,
                              envname        = envname,
                              method         = 'conda',
                              channel        = channel,
                              conda          = 'auto',
                              python_version = python_version,
                              pip            = FALSE)

    cat("\nInstallation complete.\n\n")
    
    if (restart_session &&
        requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::hasFun("restartSession"))
      rstudioapi::restartSession()
    
    invisible(NULL)
  }
