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

     pkgs <- reticulate::py_list_packages()
  .ver <- reticulate::py_config()$version_string
  
  list(#active_env = Get_active_env(),
    torch_version = pkgs[which(pkgs$package=='pytorch'),]$version,
    numpy_version = pkgs[which(pkgs$package=='numpy'),]$version,
    matplotlib_version = pkgs[which(pkgs$package=='matplotlib'),]$version,
    location = reticulate::py_config()$pythonhome,
    python_version <- unlist(strsplit(.ver, split = ' '))[1]
    )

  })
  basiliskStop(cl)

  pirat.install
}
