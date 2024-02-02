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
  function(method = "conda",
           conda = "auto",
           envname = "r-pirat",
           restart_session = TRUE,
           channel = c("pytorch", "stable", "torch"),
           pip_ignore_installed = FALSE,
           new_env = identical(envname, "r-pirat")
           ) {
    
    method <- match.arg(method)
    
    requested_versions <- list(
      torch = '1.10.0',
      numpy = '1.20.2',
      python = '3.9.5'
    )
    
    packages <- c('numpy==1.20.2', 'matplotlib')
    if (is_windows()) {
      packages <- c(packages, 'torch==1.10.0')
    } else if (is_linux()) {
      packages <- c(packages, 'pytorch==1.10.0')
    } else if (is_osx()){
      packages <- c(packages, 'pytorch==1.10.0')
    }
    python_version <- '3.9.5'
    
    # verify 64-bit
    if (.Machine$sizeof.pointer != 8) {
      stop("Unable to install TensorFlow on this platform.",
           "Binary installation is only available for 64-bit platforms.")
    }
    
    # some special handling for windows
    if (is_windows()) {
      
      # avoid DLL in use errors
      if (reticulate::py_available()) {
        stop("You should call install_pirat() only in a fresh ",
             "R session that has not yet initialized Pirat (this is ",
             "to avoid DLL in use errors during installation)")
      }
      
     }
    
 
    python_version <- python_version %||% conda_python_version
    # if(method %in% c("auto", "virtualenv") && is.null(python_version)) {
    #  # 
    # #  # virtualenv_starter() picks the most recent version available, but older
    # #  # versions of tensorflow typically don't work with the latest Python
    # # # # release. In general, we're better off picking the oldest Python version available
    # #  # that works with the current release of tensorflow.
    # #  # TF 2.13 is compatible with Python <=3.11,>=3.8
    #   
    #   available <- reticulate::virtualenv_starter(version = ">=3.9", all = TRUE)
    # #  # pick the smallest minor version, ignoring patchlevel
    #   if(nrow(available))
    #     python_version <- min(available$version[, 1:2])
    # }
    
    if (isTRUE(new_env)) {
      
      # if (method %in% c("auto", "virtualenv") &&
      #     reticulate::virtualenv_exists(envname))
      #   reticulate::virtualenv_remove(envname = envname, confirm = FALSE)
      
      if (method %in% c("auto", "conda")) {
        if (!is.null(tryCatch(conda_python(envname, conda = conda),
                              error = function(e) NULL)))
          reticulate::conda_remove(envname, conda = conda)
      }
      
    }
    
    reticulate::install_python(version = '3.9.5', force = TRUE)
    
    #browser()
    py_install_args <- list(
      packages       = packages,
      envname        = envname,
      method         = method,
      channel        = channel,
      conda          = conda,
      python_version = python_version,
      pip            = FALSE)
    
    # now ignored, superseded by `cuda`
    #py_install_args$configure_cudnn <- NULL
    
    do.call(reticulate::py_install, py_install_args)
    
    #if(is_string(metal)) {
    #  py_install_args$packages <- metal
    #  tryCatch(do.call(reticulate::py_install, py_install_args),
    #           error = function(e) {
     #            message(e)
    #             message("No suitable version of the 'tensorflow-metal' found. You can ",
    #                     "use TensorFlow with CPU only, or install a previous release ",
    #                     "of tensorflow that has GPU support on ARM macs with ",
    #                     "`tensorflow::install_tensorflow(version = '2.13')`")
    #           })
    #}
    
    cat("\nInstallation complete.\n\n")
    
    if (restart_session &&
        requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::hasFun("restartSession"))
      rstudioapi::restartSession()
    
    invisible(NULL)
  }
