



.onAttach <- function(libname, pkgname) {
  msg <- paste0("\nThis is Pirat version ", utils::packageVersion("Pirat"))
  packageStartupMessage(msg)
  Load_Python_Scripts()
}


#' @title Load Python scripts into R session
#' @description xxxx
#' 
#' @import reticulate
#' 
#' @examples
#' Load_Python_Scripts()
#' 
#' @export
#' 
Load_Python_Scripts <- function(){
  if(!('r-Pirat' %in% reticulate::conda_list()['name',])){
    warning("No conda 'r-Pirat' env exists. 
    You should install one first by running: install_Pirat()")
    return()
  } 
  dir.backup <- getwd()
  setwd(system.file(".", package="Pirat"))
  reticulate::use_condaenv("r-Pirat")
  #reticulate::use_virtualenv("reticulate-Pirat")
  reticulate::source_python(system.file("python", "LBFGS.py", package = "Pirat"))
  reticulate::source_python(system.file("python", "llk_maximize.py", package = "Pirat"))
  setwd(dir.backup)
  
  py <- reticulate::import("torch")
  return(TRUE)
}