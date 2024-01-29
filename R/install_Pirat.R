#' @title Install Pirat package
#' 
#' @description This script installs Python and PyTorch in the requested
#' versions. It is largely inspired by wthe scripts in rTorch package 
#' (https://github.com/f0nzie/rTorch)
#'
#' @param method Installation method. By default, "auto" automatically finds a
#'   method that will work in the local environment. Change the default to force
#'   a specific installation method.  Note that since this command runs without 
#'   privilege the "system" method is available only on _Windows_.
#'
#' @param conda www
#' @param extra_packages Additional Python packages to install along with
#'   PyTorch. Default are `c("numpy=1.20.2", "matplotlib")`.
#'
#' @param restart_session Restart R session after installing (note this will
#'   only occur within RStudio).
#'
#' @param pip logical
#'
#' @param channel conda channel. The default channel is `stable`.
#'   The alternative channel is `nightly`.
#'
#' @param cuda_version string for the cuda toolkit version to install. For example,
#'   to install a specific CUDA version use `cuda_version="10.2"`.
#'
#' @param dry_run logical, set to TRUE for unit tests, otherwise will execute
#'   the command.
#'
#' @param ... other arguments passed to [reticulate::conda_install()] or
#'   [reticulate::virtualenv_install()].
#'
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#'
#' # install PyTorch 1.10.0 on Python 3.3.9.5 including pandas
#' install_pytorch(version = "1.10.0", conda_python_version = "3.9.5",
#' extra_packages = "pandas")
#'
#' # Install PyTorch 1.10.0, Python 3.9.5, pandas, matplotlib install from the console
#' install_pytorch(version = "1.10.0", conda_python_version = "3.9.5",
#' extra_packages = c("pandas", "matplotlib"))
#'
#' # Install PyTorch 1.10.0 on Python 3.9.5 including pandas, matplotlib
#' install_pytorch(version = "1.10.0", conda_python_version = "3.9.5",
#' extra_packages = c("pandas", "matplotlib"), dry_run = FALSE)
#' }
#'
#' @export
#' 
install_pirat <- function(method = "conda",
                          conda = "auto",
                          restart_session = TRUE,
                          pip = FALSE,
                          channel = c("pytorch", "stable"),
                          force = TRUE) {
  
  requested_versions <- list(
    torch = '1.10.0',
    numpy = '1.20.2',
    python = '3.9.5'
  )
  
  default_packages <- c("pytorch==1.10.0", "numpy=1.20.2", 'matplotlib')
  
  envname <- 'r-pirat'
  
  
  r.pirat.exists <- reticulate::condaenv_exists(envname)
  if (r.pirat.exists && force){
    remove_Pirat(envname)
    cat('Pirat will be reinstalled...')
    reticulate::miniconda_uninstall(reticulate::miniconda_path())
  }
  
  reticulate::install_miniconda(force = force)
  
  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop("Unable to install PyTorch on this platform.",
         "Binary installation is only available for 64-bit platforms.")
  }
  
  method <- match.arg(method)
  
  
  # Main OS verification.
  if (is_osx() || is_linux()) {
    
    if (method == "conda") {
      install_conda(
        package = default_packages,
        envname = envname,
        conda = conda,
        conda_python_version = conda_python_version,
        channel = channel,
        pip = pip)
    } 

  } else if (is_windows()) {
    message("Creating ", envname, " conda environment... \n")
      reticulate::conda_create(
        envname = envname, 
        packages = default_packages,
        pip = TRUE,       # always use pip since it's the recommend way.
        channel = channel,
        python_version = conda_python_version
      )
     }

  reticulate::use_condaenv('r-pirat')
  
  # Check if necessary packages are available in the current env
  packageStartupMessage({"Checking configuration..."})
  config <- pirat_config()
  if(!config_isValid(config, requested_versions)){
    packageStartupMessage({'Error in config: Please run install_pirat()'})
    return()
  } else
    message("\nInstallation complete.\n\n")
  
  if (restart_session)
    restart_R_session('library(Pirat)')
  
  invisible(NULL)
}


restart_R_session <- function(cmd = ''){
  
  is.rstudio <- function(){
    .Platform$GUI == "RStudio"
  }
  
  if (is.rstudio() &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession(command = cmd)
  else
    cat("Please restart the R session and reload the 'Pirat' package.")
}


#' @title xxx
#' @description xxx
#' 
#' @param envname xxx
#' @param conda xxx
#' 
#' @return NULL
#' 
#' @export
#' 
remove_Pirat <- function(envname = pirat_envname) {
  
  # find if environment exists
  conda_env_exists <- reticulate::condaenv_exists(envname)
  
  if (!reticulate::condaenv_exists(envname)){
    message('No pirat environment was found')
  } else {
    message("Removing ", envname, " conda environment... \n")
    reticulate::conda_remove(envname = envname)
  }
}