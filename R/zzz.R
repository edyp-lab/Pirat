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
#' @aliases Pirat-package
#' @name Pirat
"_PACKAGE"



msg <- paste0("This is Pirat v", utils::packageVersion("Pirat"))
packageStartupMessage(msg)

.onAttach <- function(libname, pkgname) {
  
  pirat_envname <- 'r-pirat'
  Sys.unsetenv("RETICULATE_PYTHON")
  
  
   
  # packageStartupMessage({'Checking if Python 3.9.5 is installed...'})
   # if (!is.null(tryCatch(reticulate::use_miniconda(reticulate::miniconda_path(), required = T),
   #                       error = function(e) 'error'))){
   #   packageStartupMessage("Conda not found")
   #   return(NULL)
   # }
  

  packageStartupMessage('Checking if Pirat is installed...')
    if (is.null(tryCatch(reticulate::conda_python(pirat_envname),
                                          error = function(e) NULL))){
      cat("Pirat not found. You should use by running: install_pirat()")
      return(NULL)
    }
  
  
  # Force reticulate to use pyhton in the r-pirat env
  conda <- reticulate::conda_list()
  my_python <- reticulate::conda_python(pirat_envname)
  Sys.setenv(RETICULATE_PYTHON = my_python)
  
      packageStartupMessage("Loading conda env...")
    if (!is.null(tryCatch(reticulate::use_miniconda(pirat_envname, required = TRUE),
                         error = function(e) e,
                         warning = function(w) w))){
      cat("Env cannot be launched")
      
      return(NULL)
    }
      
    
    packageStartupMessage("Sourcing custom Python scripts...")
    tryCatch({
      dir.backup <- getwd()
      setwd(system.file(".", package="Pirat"))
      custom_scripts <- c("LBFGS.py", "llk_maximize.py")
      for (i in custom_scripts)
        reticulate::source_python(system.file("python", i, package = "Pirat"))
      setwd(dir.backup)
      },
      warning = function(w) w,
      error = function(e) e
      )
    
    
    packageStartupMessage("Loading torch package...")
    tryCatch({
      py <- reticulate::import("torch", delay_load = FALSE)
      },
      warning = function(w) w,
      error = function(e) e
      )

}


#' @title Check the validity of the r-pirat environment
#' @description xxx
#' 
#' @return A boolean indicating if the r-pirat if well configured
#' 
#' @export
#' 
config_isValid <- function(){
  
  config <- pirat_config()
  is.valid <- config$torch_version == requested_versions$torch &&
      config$numpy_version == requested_versions$numpy &&
      config$python_version == requested_versions$python

    return(is.valid)
}



#' Pirat configuration information
#'
#' @return List with information on the current configuration of Pirat
#'
#' @keywords internal
#' @export
pirat_config <- function() {
  
  if (reticulate::condaenv_exists(envname = 'r-pirat'))
    use_condaenv('r-pirat')
  else {
    cat("r-pirat not found.")
    return()
  }
  
  
  
  Get_active_env <- function(){
    py_home <- reticulate::py_config()$pythonhome
    tmp <- strsplit(py_home, split="/")[[1]]
    active_env <- tmp[length(tmp)]
    active_env
  }
  
  
  pkgs <- reticulate::py_list_packages()
  # get version
  structure(class = "pirat_config", list(
    available = TRUE,
    acive_env = Get_active_env(),
    torch_version = pkgs[which(pkgs$package=='pytorch'),]$version,
    numpy_version = pkgs[which(pkgs$package=='numpy'),]$version,
    matplotlib_version = pkgs[which(pkgs$package=='matplotlib'),]$version,
    location = reticulate::py_config()$pythonhome,
    python_version = unlist(strsplit(reticulate::py_config()$version_string, split=' '))[1]
    ))
}

#' @export
print.pirat_config <- function(x, ...) {
  if (x$available) {
    aliased <- function(path) sub(Sys.getenv("HOME"), "~", path)
    cat("Python v", x$python_version, " (location: ", aliased(x$location), "\n", sep = "")
    cat("Active env: ", x$acive_env, "\n", sep = "")
    cat("pytorch v", x$torch_version, "\n", sep = "")
    cat("NumPy v", x$numpy_version, "\n", sep = "")
    cat("matplotlib v", x$matplotlib_version, "\n", sep = "")
    
  } else {
    cat(x$error_message, "\n")
  }
}



# Build error message for Pirat configuration errors
pirat_config_error_message <- function() {
  message <- "Valid installation of Pirat not found."
  config <- pirat_config()
  if (!is.null(config)) {
    if (length(config$python_versions) > 0) {
      message <- paste0(message,
                        "\n\nPython environments searched for 'Pirat' package:\n")
      python_versions <- paste0(" ", normalizePath(config$python_versions, mustWork = FALSE),
                                collapse = "\n")
      message <- paste0(message, python_versions, sep = "\n")
    }
  }
  
  python_error <- tryCatch({
    import("Pirat")
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
                    "\nYou can install Pirat using the install_pirat() function.\n")
  message
}