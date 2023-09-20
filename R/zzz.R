



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
  
  
  cat("\nLoading Python env: ...")
  switch(get_os(),
         windows = {
           if (!reticulate::virtualenv_exists('r-Pirat')){
              warning("No 'r-Pirat' virtualenv exists. 
                    You should install one first by running: install_Pirat()")
                return()
             } else {
               reticulate::use_virtualenv("r-Pirat")
             }
           },
           linux = {
             if(!('r-Pirat' %in% reticulate::conda_list()$name)){
                warning("No conda 'r-Pirat' env exists. 
                You should install one first by running: install_Pirat()")
                return()
            } else {
              reticulate::use_condaenv("r-Pirat")
            }
             },
         default = {
           warning("OS not detected or managed")
           return()
         }
  )
  cat("done\n\n")

  
  # Now, install custom Python scripts
  cat("\nInstallation of custom Python scripts: ...")
  dir.backup <- getwd()
  setwd(system.file(".", package="Pirat"))
  reticulate::source_python(system.file("python", "LBFGS.py", package = "Pirat"))
  reticulate::source_python(system.file("python", "llk_maximize.py", package = "Pirat"))
  setwd(dir.backup)
  cat("done\n\n")
  
  
  py <- reticulate::import("torch")

}