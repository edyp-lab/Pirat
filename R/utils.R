#' @title Source user defined Python code
#' @description Use the function `reticulate::source_python()` to source
#' custom python files stored int the directory 'inst/python' of the 
#' package `Pirat`.
#' 
#' @export
#' 
#' @return NA
#' 
source_own_pyScripts <- function(){
    
    tryCatch({
      dir.backup <- getwd()
      setwd(system.file(".", package = "Pirat"))
      custom_scripts <- c("LBFGS.py", "llk_maximize.py")
      for (i in custom_scripts)
        reticulate::source_python(system.file("python", i, package = "Pirat"))
      setwd(dir.backup)
      },
      warning = function(w) w,
      error = function(e) e
      )
}





