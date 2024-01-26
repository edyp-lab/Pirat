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
install_pirat <-
  function(method = c("auto", "virtualenv", "conda"),
           conda = "auto",
           version = "default",
           envname = "r-pirat",
           extra_packages = NULL,
           restart_session = TRUE,
           conda_python_version = NULL,
           ...,
           cuda = NULL,
           metal = is_mac_arm64(),
           pip_ignore_installed = FALSE,
           new_env = identical(envname, "r-pirat"),
           python_version = '3.9.5') {
  
  # requested_versions <- list(
  #   torch = '1.10.0',
  #   numpy = '1.20.2',
  #   python = '3.9.5'
  # )
  # 
  # envname <- 'r-reticulate'
  # # PyTorch version to install. The "default" version is __1.10.0__. 
  # # You can specify a specific __PyTorch__ version with 
  # # `version="1.2"`, or `version="1.6"`.
  # version <- requested_versions$torch
  # reticulate::install_python("3.9.5")
  # reticulate::install_miniconda(force=TRUE)
  # reticulate::conda_create(version = "3.9.5")
  # reticulate::use_condaenv('r-reticulate', required = TRUE)
  # reticulate::conda_python_version <- '3.9.5'
  

      
      method <- match.arg(method)
      
      # verify 64-bit
      if (.Machine$sizeof.pointer != 8) {
        stop("Unable to install TensorFlow on this platform.",
             "Binary installation is only available for 64-bit platforms.")
      }
      
      # some special handling for windows
      if (is_windows()) {
        
        # avoid DLL in use errors
        if (py_available()) {
          stop("You should call install_tensorflow()/install_keras() only in a fresh ",
               "R session that has not yet initialized Keras and TensorFlow (this is ",
               "to avoid DLL in use errors during installation)")
        }
        
        if(grepl("gpu", as.character(version), ignore.case = TRUE))
          warning("Caution: TensorFlow 2.10 was the last TensorFlow release that supported GPU on native-Windows. Starting with TensorFlow 2.11, you will need to install TensorFlow in WSL2, or install a CPU-only version of TensorFlow.",
                  if(identical(.Platform$GUI, "RStudio")) " For a guide on how to use RStudio with WSL2, see https://support.posit.co/hc/en-us/articles/360049776974-Using-RStudio-Server-in-Windows-WSL2")
        
      }
      
      
      can_use_gpu <- FALSE
      if (is.null(cuda)) {
        
        can_use_gpu <-
          is_linux() &&
          (version %in% c("default", "release") ||
             isTRUE(extract_numeric_version(version) >= "2.14")) &&
          tryCatch(as.logical(length(system("lspci | grep -i nvidia", intern = TRUE))),
                   warning = function(w) FALSE) # warning emitted by system for non-0 exit stat
        
        cuda <- can_use_gpu
        
      }
      
      tf_package_spec <- parse_tensorflow_version(version)
      
      if(isTRUE(cuda) && !grepl("^.*\\.whl$", tf_package_spec)) {
        tf_package_spec <- sub("([^=<>!]*)(.*)", "\\1[and-cuda]\\2",
                               tf_package_spec)
      }
      
      default_packages <- c("numpy==1.20.2", 'matplotlib')
      packages <- unique(c(
        #tf_package_spec,
        #as.character(extra_packages),
        default_packages
      ))
      
      
      # if (isTRUE(metal)) repeat {
      #   tf_ver <- extract_numeric_version(tf_package_spec)
      #   if(is.na(tf_ver))
      #     break
      #
      #   if(tf_ver >= "2.14")
      #     metal <- "tensorflow-metal>1.0.1"
      #   else if (tf_ver >= "2.13")
      #     metal <- "tensorflow-metal>=1.0.1"
      #   else if (tf_ver >= "2.12")
      #     metal <- "tensorflow-metal==0.8.*"
      #   else
      #     # https://pypi.org/project/tensorflow-metal/
      #     metal <- "tensorflow-metal"
      #
      #   break
      # }
      
      python_version <- python_version %||% conda_python_version
      if(method %in% c("auto", "virtualenv") &&
         is.null(python_version)) {
        
        # virtualenv_starter() picks the most recent version available, but older
        # versions of tensorflow typically don't work with the latest Python
        # release. In general, we're better off picking the oldest Python version available
        # that works with the current release of tensorflow.
        # TF 2.13 is compatible with Python <=3.11,>=3.8
        
        available <- reticulate::virtualenv_starter(version = ">=3.9", all = TRUE)
        # pick the smallest minor version, ignoring patchlevel
        if(nrow(available))
          python_version <- min(available$version[, 1:2])
      }
      
      if (isTRUE(new_env)) {
        
        if (method %in% c("auto", "virtualenv") &&
            reticulate::virtualenv_exists(envname))
          reticulate::virtualenv_remove(envname = envname, confirm = FALSE)
        
        if (method %in% c("auto", "conda")) {
          if (!is.null(tryCatch(conda_python(envname, conda = conda),
                                error = function(e) NULL)))
            reticulate::conda_remove(envname, conda = conda)
        }
        
      }
      
      py_install_args <- list(
        packages       = packages,
        envname        = envname,
        method         = method,
        conda          = conda,
        python_version = python_version,
        pip            = TRUE,
        pip_ignore_installed = pip_ignore_installed,
        ...
      )
      
      # now ignored, superseded by `cuda`
      py_install_args$configure_cudnn <- NULL
      
      do.call(reticulate::py_install, py_install_args)
      
      if(is_string(metal)) {
        py_install_args$packages <- metal
        tryCatch(do.call(reticulate::py_install, py_install_args),
                 error = function(e) {
                   message(e)
                   message("No suitable version of the 'tensorflow-metal' found. You can ",
                           "use TensorFlow with CPU only, or install a previous release ",
                           "of tensorflow that has GPU support on ARM macs with ",
                           "`tensorflow::install_tensorflow(version = '2.13')`")
                 })
      }
      
      cat("\nInstallation complete.\n\n")
      
      if (restart_session &&
          requireNamespace("rstudioapi", quietly = TRUE) &&
          rstudioapi::hasFun("restartSession"))
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
  

  remove_Pirat(envname, conda)
  
  
  message("Creating ", envname, " conda environment... \n")
  reticulate::conda_create(
    envname = envname, conda = conda,
    packages = paste0("python=", conda_python_version)
  )
  
  message("Installing python modules...\n")
  # rTorch::conda_install(envname="r-torch-37", packages="pytorch-cpu",
  #         channel = "pytorch", conda="auto", python_version = "3.7")
  reticulate::conda_install(envname = envname,
                packages = c(package, extra_packages),
                conda = conda,
                pip = pip,       # always use pip since it's the recommend way.
                channel = channel,
                python_version = conda_python_version)
  
}

install_virtualenv <- function(package, extra_packages, envname, ...) {
  
  remove_Pirat(envname)
  
  
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










default_version <- numeric_version("2.14")

parse_tensorflow_version <- function(version) {
  # returns unquoted string directly passable to pip, e.g 'tensorflow==2.5.*'
  
  if(is.null(version) || is.na(version) || version %in% c("", "release"))
    return("tensorflow")
  
  version <- as.character(version) # if numeric_version()
  
  if(version == "release-cpu")
    return("tensorflow-cpu")
  
  # full path to whl.
  if (grepl("^.*\\.whl$", version))
    return(normalizePath(version))
  
  if (grepl("nightly", version)) {
    if(!startsWith(version, "tf-"))
      version <- paste0("tf-", version)
    return(version)
  }
  
  package <- "tensorflow"
  if(grepl(".*(cpu|gpu)$", version)) {
    # append {-cpu,-gpu} suffix to package
    package <- sprintf("%s-%s", package, sub(".*-(cpu|gpu)$", "\\1", version))
    
    # strip -?{cpu,gpu} suffix from version
    version <- sub("(.*?)-?([cg]pu)$", "\\1", version)
  }
  
  if(version %in% c("default", ""))
    version <- default_version
  
  if(!grepl("[><=]", version))
    version <- sprintf("==%s.*", version)
  
  paste0(package, version)
}


extract_numeric_version <- function(x, strict = FALSE) {
  x <- gsub("[^0-9.]+", "", as.character(x), perl = TRUE)
  x <- sub("^\\.+", "", x)
  x <- sub("\\.+$", "", x)
  numeric_version(x, strict = strict)
}