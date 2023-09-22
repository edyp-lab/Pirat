



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
           warning("OS not detected or unmanaged")
           return()
         }
  )
  cat("done")

  
  # Now, install custom Python scripts
  cat("\nInstalling custom Python scripts: ...")
  dir.backup <- getwd()
  setwd(system.file(".", package="Pirat"))
  reticulate::source_python(system.file("python", "LBFGS.py", package = "Pirat"))
  reticulate::source_python(system.file("python", "llk_maximize.py", package = "Pirat"))
  setwd(dir.backup)
  cat("done")
  
  cat("\nFinalizing loading: ...")
  py <- reticulate::import("torch")
  cat("done")
}




#' Pirat configuration information
#'
#' @return List with information on the current configuration of TensorFlow.
#'   You can determine whether TensorFlow was found using the `available`
#'   member (other members vary depending on whether `available` is `TRUE`
#'   or `FALSE`)
#'
#' @keywords internal
#' @export
torch_config <- function() {
  
  # first check if we found tensorflow
  have_Pirat<- py_module_available("torch")
  
  # get py config
  config <- py_config()
  
  # found it!
  if (have_tensorflow) {
    
    # get version
    if (reticulate::py_has_attr(tf, "version"))
      version_raw <- tf$version$VERSION
    else
      version_raw <- tf$VERSION
    
    tfv <- strsplit(version_raw, ".", fixed = TRUE)[[1]]
    version <- package_version(paste(tfv[[1]], tfv[[2]], sep = "."))
    
    structure(class = "tensorflow_config", list(
      available = TRUE,
      version = version,
      version_str = version_raw,
      location = config$required_module_path,
      python = config$python,
      python_version = config$version
    ))
    
    # didn't find it
  } else {
    structure(class = "tensorflow_config", list(
      available = FALSE,
      python_versions = config$python_versions,
      error_message = tf_config_error_message()
    ))
  }
}


#' @rdname tf_config
#' @keywords internal
#' @export
torch_version <- function() {
  config <- torch_config()
  if (config$available)
    config$version
  else
    NULL
}
