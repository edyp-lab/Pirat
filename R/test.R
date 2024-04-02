#' @title Test function
#'
#' @description Does nothing but test that we can load modules from different virtual environments.
#'
#' @return A list of names of objects exposed in each module.
#' @author Aaron Lun
#' 
#' @examples
#' test()
#' @export
#' @importFrom reticulate import
#' @importFrom basilisk basiliskStart basiliskRun basiliskStop
test_Pirat <- function() {
  cl <- basiliskStart(envPirat)
  matplotlib.names <- basiliskRun(cl, function() { 
    X <- reticulate::import("matplotlib")
    names(X) 
  })
  basiliskStop(cl)

  list(matplotlib=matplotlib.names)
}
