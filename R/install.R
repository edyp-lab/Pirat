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
    #X <- reticulate::import("torch")
     pkgs <- reticulate::py_list_packages()
  .ver <- reticulate::py_config()$version_string
  
  list(
    location = reticulate::py_config()$pythonhome,
    python = unlist(strsplit(.ver, split = ' '))[1],
    torch = pkgs[which(pkgs$package=='pytorch'),]$version,
    numpy = pkgs[which(pkgs$package=='numpy'),]$version,
    matplotlib = pkgs[which(pkgs$package=='matplotlib'),]$version    
    )
  })
  basiliskStop(cl)

  cat('Installed packages:\n')
  cat(paste0("\t", names(pirat.install), " (", pirat.install, ")\n"))
}
