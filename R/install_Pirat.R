#' @title Install Python environment and libraries
#' @description This function installs the Python environment and the
#' Python libraries needed to run the imputation functions of the package 
#' `Pirat`. This operation takes several minutes.
#' It has to be execute only once, before the first use of the package. 
#' 
#' @rdname install_pirat
#' 
#' @examples
#' \donttest{
#' install_pirat()
#' }
#' 
#' @export
#' 
#' @importFrom stats cov
#' 
#' @return NA
#' 
install_pirat <- function() { 
    proc <- basilisk::basiliskStart(envPirat)
    on.exit(basilisk::basiliskStop(proc))
  
    some_useful_thing <- basilisk::basiliskRun(proc, 
        fun = function() {
        py <- reticulate::import("torch", delay_load = FALSE)
      
        message('Installation completed !')
        #output <- NULL
        # The return value MUST be a pure R object, i.e., no reticulate
        # Python objects, no pointers to shared memory. 
        #output 
    })
  
    invisible(some_useful_thing)
}