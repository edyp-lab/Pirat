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
install_Pirat <- function() {
  cl <- basiliskStart(envPirat)
  pirat.install <- basiliskRun(cl, function() { 
    X <- reticulate::import("torch")
    names(X) 
  })
  basiliskStop(cl)

  pirat.install
}
