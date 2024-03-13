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
#' @param silent A boolean (default is TRUE) xxx
#' @param verbose A boolean (default is TRUE) to indicate whether to 
#' show details or not.
#' 
#' @author Samuel Wieczorek
#' 
#' @return No value
#' 
#' @examples
#' \donttest{
#' install_pirat()
#' }
#'
#' @export
#'
install_pirat <- function(force = FALSE, 
                          silent = TRUE,
                          verbose = TRUE) {
    
  
  #require(reticulate)
  Sys.unsetenv("RETICULATE_PYTHON")
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
    
  if(verbose)
    cat('Checks for Pirat...\n')
  if (!is.null(tryCatch(reticulate::condaenv_exists(envname),
                              error = function(e) NULL)) &&
      reticulate::condaenv_exists(envname) == TRUE){
      if (!force){
        stop('Pirat is already installed. To force a new installation and 
        erase the current one, set the argument force = TRUE.')
        #return(NULL)
      } else {
        cat('Removing previous installation of Pirat...\n')
        reticulate::conda_remove(envname, conda = conda)
      }
    }
    
   if(verbose)
     cat('Installing miniconda...\n')
    tryCatch({
      reticulate::install_miniconda(path = reticulate::miniconda_path(), 
                                    force = TRUE)
      },
      error = function(e) {
        if(!silent){
          user_input <- readline("The R session must be restarted. 
                                 Do you want to proceed ? (Y/n)  ")
          if(user_input == 'n') stop('Exiting...')
          restart_session(cmd = 'Pirat::install_pirat()',
                    alternate.msg = "Please restart manually the R session.")
        }
      })
    
    reticulate::conda_install(packages = packages,
                              envname = envname,
                              method = 'conda',
                              channel = channel,
                              conda = 'auto',
                              python_version = python_version,
                              pip = FALSE)

    cat("\nInstallation complete.\n\n")
    
   restart_session(cmd = 'library(Pirat)',
                   alternate.msg = "Please restart the R session and 
                   reload the 'Pirat' package.")
    
    invisible(NULL)
}



restart_session <- function(cmd = NULL,
                            alternate.msg = NULL){
  is.rstudio <- function(){
    .Platform$GUI == "RStudio"
  }
  
  #if (restart_session){
    if (is.rstudio() &&
        requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::hasFun("restartSession"))
      rstudioapi::restartSession(command = cmd)
    else
      cat(paste0('\n', alternate.msg, '\n\n'))
  #}
}