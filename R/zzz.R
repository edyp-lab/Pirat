#' @title xxx
#' @description xxxx
#' # https://community.rstudio.com/t/when-to-use-onload-vs-onattach/21953
#' Usually you want .onLoad, which—as the name suggests—runs when the package is 
#' loaded. If something has to happen before anything is run, that's the way to 
#' go. onAttach only runs when the library is attached, e.g. when somebody calls
#' library(your_package). onLoad will also run when somebody loads but doesn't 
#' attach your package by calling your_package::your_function.
#'
#' @docType package
#' @aliases Piratpackage
#' @name Pirat
NULL



msg <- paste0("This is Pirat v", utils::packageVersion("Pirat"))
packageStartupMessage(msg)

.onAttach <- function(libname, pkgname) {
  
  pirat_envname <- 'r-pirat'
  config <- reticulate::py_discover_config()
  
  if (py_version(config$version_string) != '3.9.5'){
    cat("Python 3.9.5 is not installed. Please install it before using Pirat")
    return()
  }
  
  reticulate::use_python(config$python)
  
   tryCatch({
    packageStartupMessage({"Loading Python env..."})
      pirat_conda_exists <- pirat_envname %in% reticulate::conda_list()$name
      pirat_venv_exists <- reticulate::virtualenv_exists(pirat_envname)
  
  
      if (!pirat_conda_exists && !pirat_venv_exists){
        packageStartupMessage({paste0("Any ", pirat_envname, " environment exists. ")})
        packageStartupMessage({"You should install one first by running: install_pirat()"})
        Pirat::install_pirat()
      } 
  
      if (pirat_conda_exists)
        reticulate::use_condaenv(pirat_envname)
      else if (pirat_venv_exists)
        reticulate::use_virtualenv(pirat_envname)
  
      # Now, source custom Python scripts
      packageStartupMessage({"Sourcing custom Python scripts..."})
      dir.backup <- getwd()
      setwd(system.file(".", package="Pirat"))
      reticulate::source_python(system.file("python", "LBFGS.py", package = "Pirat"))
      reticulate::source_python(system.file("python", "llk_maximize.py", package = "Pirat"))
      setwd(dir.backup)
      ##packageStartupMessage({"done"})
  
      packageStartupMessage({"Finalizing loading..."})
      py <- reticulate::import("torch")
      #packageStartupMessage({"done"})
    },
    warning = function(w){packageStartupMessage({w})},
    error = function(e){packageStartupMessage({e})}
    )
}



#' @title xxx
#' @description xxx
#' 
#' @param config xxx
#' 
#' @export
#' 
config_isValid <- function(config){
  is.valid <- (config$torch_version == requested_versions$torch &&
      config$numpy_version == requested_versions$numpy &&
      config$python_version == requested_versions$python)

    return(is.valid)
}


#' @title xxx
#' @description xxx
#' 
#' 
#' @export
#' 
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
      location = config$pythonhome,
      python_version = py_version(config$version_string)
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


py_version <- function(x)
  strsplit(x, split = ' | ')[[1]][1]



#' @export
print.pirat_config <- function(x, ...) {
  if (x$available) {
    aliased <- function(path) sub(Sys.getenv("HOME"), "~", path)
    cat("Python v", x$python_version, " (location: ", aliased(x$location), "\n", sep = "")
    cat("Active env: ", x$acive_env, "\n", sep = "")
    cat("PyTorch v", x$torch_version, "\n", sep = "")
    cat("NumPy v", x$numpy_version, "\n", sep = "")
    cat("matplotlib v", x$matplotlib_version, "\n", sep = "")
    
  } else {
    cat(x$error_message, "\n")
  }
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