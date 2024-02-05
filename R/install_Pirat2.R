#' @title Install Pirat package
#'
#' @description This script installs Python and PyTorch in the requested
#' versions. It is largely inspired by wthe scripts in rTorch package
#' (https://github.com/f0nzie/rTorch)
#'
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
install_pirat <- function() {
    
    method = "conda"
    conda = "auto"
    restart_session = TRUE
    pip_ignore_installed = FALSE
    envname = "r-pirat"
    channel = c("pytorch", "stable", "torch")
    new_env = identical(envname, "r-pirat")
    
    
    # requested_versions <- list(
    #   torch = '1.10.0',
    #   numpy = '1.20.2',
    #   python = '3.9.5'
    # )
    
    packages <- c('numpy==1.20.2', 
                  'matplotlib', 
                  'pytorch==1.10.0', 
                  'cpuonly')
    
    # if (is_windows()) {
    #   packages <- c(packages, 'pytorch==1.10.0')
    # } else if (is_linux()) {
    #   packages <- c(packages, 'pytorch==1.10.0')
    # } else if (is_osx()){
    #   packages <- c(packages, 'pytorch==1.10.0')
    # }
    python_version <- '3.9.5'
    
    
    # some special handling for windows
    # if (is_windows()) {
    #   
    #   # avoid DLL in use errors
    #   if (reticulate::py_available()) {
    #     stop("You should call install_pirat() only in a fresh ",
    #          "R session that has not yet initialized Pirat (this is ",
    #          "to avoid DLL in use errors during installation)")
    #   }
    #   
    #  }
    
 
    #python_version <- python_version %||% conda_python_version
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
    
    #reticulate::install_python(version = '3.9.5', force = TRUE)
    reticulate::install_miniconda(force = TRUE)
    #browser()
    py_install_args <- list(
      packages       = packages,
      envname        = envname,
      method         = 'conda',
      channel        = channel,
      conda          = 'auto',
      python_version = python_version,
      pip            = FALSE)
    
    # now ignored, superseded by `cuda`
    #py_install_args$configure_cudnn <- NULL
    
    #do.call(reticulate::py_install, py_install_args)
    do.call(reticulate::conda_install, py_install_args)
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
