



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
  pirat_conda_exists <- 'r-pirat' %in% reticulate::conda_list()$name
  pirat_venv_exists <-reticulate::virtualenv_exists('r-pirat')
    
  
  if (!pirat_conda_exists && !rpirat_venv_exists){
  warning("No 'r-pirat' env exists. 
          You should install one first by running: install_Pirat()")
    return()
    } 
  
  if (pirat_conda_exists)
    reticulate::use_condaenv("r-pirat")
  else if (pirat_venv_exists)
    reticulate::use_virtualenv("r-pirat")

  cat("done")

  
  # Now, install custom Python scripts
  cat("\nSourcing custom Python scripts: ...")
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



#' @export
Get_active_env <- function(){
  
  py_home <- reticulate::py_config()$pythonhome
  tmp <- strsplit(py_home, split="/")[[1]]
  active_env <- tmp[length(tmp)]
  
  active_env
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
pirat_config <- function() {
  
  # first check if we found pytorch
  have_torch <- reticulate::py_module_available("torch")
  
  # get py config
  config <- reticulate::py_config()
  
  # found it!
  if (have_torch) {
    
    # get version
    ind <- which(reticulate::py_list_packages()$package=='pytorch')
    torch_version <- reticulate::py_list_packages()[ind,]$version
    
    ind <- which(reticulate::py_list_packages()$package=='numpy')
    numpy_version <- reticulate::py_list_packages()[ind,]$version
    
    ind <- which(reticulate::py_list_packages()$package=='matplotlib')
    matplotlib_version <- reticulate::py_list_packages()[ind,]$version
    
    
    structure(class = "pirat_config", list(
      available = TRUE,
      acive_env = Get_active_env(),
      torch_version = torch_version,
      numpy_version = numpy_version,
      matplotlib_version = matplotlib_version,
      python = config$python,
      python_version = reticulate::py_version()
    ))
    
    # didn't find it
  } else {
    structure(class = "pirat_config", list(
      available = FALSE,
      python_versions = config$python_versions,
      #error_message = pirat_config_error_message()
      error_message = NULL
    ))
  }
}


#' @rdname pirat_config
#' @keywords internal
#' @export
torch_version <- function() {
  config <- pirat_config()
  if (config$available)
    config$torch_version
  else
    NULL
}


# Build error message for TensorFlow configuration errors
pirat_config_error_message <- function() {
  message <- "Valid installation of Pirat not found."
  config <- pirat_config()
  if (!is.null(config)) {
    if (length(config$python_versions) > 0) {
      message <- paste0(message,
                        "\n\nPython environments searched for 'tensorflow' package:\n")
      python_versions <- paste0(" ", normalizePath(config$python_versions, mustWork = FALSE),
                                collapse = "\n")
      message <- paste0(message, python_versions, sep = "\n")
    }
  }
  
  python_error <- tryCatch({
    import("tensorflow")
    list(message = NULL)
  },
  error = function(e) {
    on.exit(py_clear_last_error())
    py_last_error()
  })
  
  message <- paste0(message,
                    "\nPython exception encountered:\n ",
                    python_error$message, "\n")
  
  message <- paste0(message,
                    "\nYou can install TensorFlow using the install_tensorflow() function.\n")
  message
}