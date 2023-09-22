#' Install Pirat and its dependencies
#'
#' `install_Pirat()` installs just the python packages needed by Pirat.
#'
#' @details You may be prompted to download and install miniconda if reticulate
#'   did not find a non-system installation of python. Miniconda is the
#'   recommended installation method for most users, as it ensures that the R
#'   python installation is isolated from other python installations. All python
#'   packages will by default be installed into a self-contained conda or venv
#'   environment named "r-Pirat". Note that "conda" is the only 
#'   supported method on M1 Mac.
#'
#'   If you initially declined the miniconda installation prompt, you can later
#'   manually install miniconda by running [`reticulate::install_miniconda()`].
#'
#' @section Custom Installation: `install_Pirat()` isn't required to use Pirat.
#'  If you manually configure a python environment with the required 
#'  dependencies and Python environment, you can tell R to use it by pointing 
#'  reticulate at it, commonly by setting an environment variable:
#'
#'   ``` R
#'   Sys.setenv("RETICULATE_PYTHON" = "~/path/to/python-env/bin/python")
#'   ```
#'
#' @md
#' 
#' @param envname xxx
#'
#' @param restart_session Whether to restart R session after installing. Note that 
#' it will be automatically if in RStudio.
#'
#' @param new_env If `TRUE`, any existing Python virtual environment and/or
#'   conda environment specified by `envname` is deleted first.
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' install_pirat()
#' }
#' 
#' 
install_pirat <- function(method = c("conda", "virtualenv", "auto"),
                          conda = "auto",
                          version = "1.10.0",
                          envname = "r-pirat",
                          extra_packages = NULL,
                          restart_session = TRUE,
                          conda_python_version = "3.9.5",
                          pip = FALSE,
                          channel = "stable",
                          cuda_version = NULL,
                          dry_run = FALSE,
                          ...) {
  
  
  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop("Unable to install PyTorch on this platform.",
         "Binary installation is only available for 64-bit platforms.")
  }
  
  method <- match.arg(method)
  
  # unroll version
  ver <- parse_torch_version(version, cuda_version, channel)
  
  version <- ver$version
  gpu <- ver$gpu
  package <- ver$package
  cpu_gpu_packages <- ver$cpu_gpu_packages
  channel <- ver$channel
  
  # Packages in this list should always be installed.
  
  default_packages <- c("numpy=1.20.2", 'matplotlib')
  
  # # Resolve torch probability version.
  # if (!is.na(version) && substr(version, 1, 4) %in% c("1.1.0", "1.1", "1.1.0")) {
  #   default_packages <- c(default_packages, "pandas")
  #   # install pytorch-nightly
  # } else if (is.na(version) ||(substr(version, 1, 4) %in% c("2.0.") || version == "nightly")) {
  #   default_packages <- c(default_packages, "numpy")
  # }
  
  extra_packages <- unique(c(cpu_gpu_packages, default_packages, extra_packages))
  
  if (dry_run) {
    os <- ifelse(is_osx(), "osx",
                 ifelse(is_linux(), "linux",
                        ifelse(is_windows(), "windows", "None")))
    out <- list(package = package, extra_packages = extra_packages,
                envname = envname, conda = conda,
                conda_python_version = conda_python_version,
                channel = channel, pip = pip, os = os)
    return(out)
  }
  
  # Main OS verification.
  if (is_osx() || is_linux()) {
    
    if (method == "conda") {
      install_conda(
        package = package,
        extra_packages = extra_packages,
        envname = envname,
        conda = conda,
        conda_python_version = conda_python_version,
        channel = channel,
        pip = pip,
        ...
      )
    } else if (method == "virtualenv" || method == "auto") {
      install_virtualenv(
        package = package,
        extra_packages = extra_packages,
        envname = envname,
        ...
      )
    }
    
  } else if (is_windows()) {
    
    if (method == "virtualenv") {
      stop("Installing PyTorch into a virtualenv is not supported on Windows",
           call. = FALSE)
    } else if (method == "conda" || method == "auto") {
      
      install_conda(
        package = package,
        extra_packages = extra_packages,
        envname = envname,
        conda = conda,
        conda_python_version = conda_python_version,
        channel = channel,
        pip = pip,
        ...
      )
      
    }
    
  } else {
    stop("Unable to install PyTorch on this platform. ",
         "Binary installation is available for Windows, OS X, and Linux")
  }
  
  message("\nInstallation complete.\n\n")
  
  if (restart_session && rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession()
  
  invisible(NULL)
}



install_conda <- function(package, 
                          extra_packages, 
                          envname, 
                          conda,
                          conda_python_version, 
                          channel, 
                          pip, 
                          ...) {
  
  # Example:
  # rTorch:::install_conda(package="pytorch=1.4",
  # extra_packages=c("torchvision", "cpuonly", "matplotlib", "pandas")
  # envname="r-torch", conda="auto", conda_python_version = "3.6",
  # channel="pytorch", pip=FALSE
  # )
  
  # find if environment exists
  envname_exists <- envname %in% reticulate::conda_list(conda = conda)$name
  
  # remove environment
  if (envname_exists) {
    message("Removing ", envname, " conda environment... \n")
    reticulate::conda_remove(envname = envname, conda = conda)
  }
  
  
  message("Creating ", envname, " conda environment... \n")
  reticulate::conda_create(
    envname = envname, conda = conda,
    packages = paste0("python=", conda_python_version)
  )
  
  message("Installing python modules...\n")
  # rTorch::conda_install(envname="r-torch-37", packages="pytorch-cpu",
  #         channel = "pytorch", conda="auto", python_version = "3.7")
  conda_install(
    envname = envname,
    packages = c(package, extra_packages),
    conda = conda,
    pip = pip,       # always use pip since it's the recommend way.
    channel = channel,
    ...
  )
  
}

install_virtualenv <- function(package, extra_packages, envname, ...) {
  
  # find if environment exists
  envname_exists <- envname %in% reticulate::virtualenv_list()
  
  # remove environment
  if (envname_exists) {
    message("Removing ", envname, " virtualenv environment... \n")
    reticulate::virtualenv_remove(envname = envname, confirm = FALSE)
  }
  
  message("Creating ", envname, " virtualenv environment... \n")
  reticulate::virtualenv_create(envname = envname)
  
  message("Installing python modules...\n")
  reticulate::virtualenv_install(
    envname = envname,
    packages = c(package, extra_packages),
    ...
  )
  
}
  



parse_torch_version <- function(version, cuda_version = NULL, channel = "stable") {
  default_version <- "1.10.0"
  # channel <- "pytorch"    # this is the channel
  
  ver <- list(
    version = default_version,
    gpu = FALSE,
    package = NULL,
    cuda_version = cuda_version,
    cpu_gpu_packages = NULL,
    channel = channel
  )
  
  if (version == "default") {
    ver$package <- paste0("pytorch==", ver$version)
  } else {
    ver$version <- version
    ver$package <- paste0("pytorch==", ver$version)
  }
  
  
  if (is.null(ver$cuda_version)) {
    ver$cpu_gpu_packages <- "cpuonly"
  } else {
    ver$cuda_version <- cuda_version
    ver$cpu_gpu_packages <- paste0("cudatoolkit==", ver$cuda_version)
  }
  
  if (channel == "stable") {
    ver$channel <- "pytorch"
  } else if (channel == "nightly") {
    ver$channel <- "pytorch-nightly"
  } else {
    stop("not a valid channel")
  }
  
  ver
}



#' Install additional Python packages alongside PyTorch
#'
#' This function is deprecated. Use the `extra_packages` argument in function
#' `install_pytorch()` to install additional packages.
#'
#' @param packages Python packages to install
#' @param conda Path to conda executable (or "auto" to find conda using the PATH
#'   and other conventional install locations). Only used when PyTorch is
#'   installed within a conda environment.
#'
#' @keywords internal
#'
install_torch_extras <- function(packages, conda = "auto") {
  message("Extra packages not installed (this function is deprecated). \n",
          "Use the extra_packages argument to install_pytorch() to ",
          "install additional packages.")
}
