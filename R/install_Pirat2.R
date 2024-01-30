#' Install TensorFlow and its dependencies
#'
#' `install_tensorflow()` installs just the tensorflow python package and it's
#' direct dependencies. For a more complete installation that includes
#' additional optional dependencies, use [`keras::install_keras()`].
#'
#' @details You may be prompted to download and install miniconda if reticulate
#'   did not find a non-system installation of python. Miniconda is the
#'   recommended installation method for most users, as it ensures that the R
#'   python installation is isolated from other python installations. All python
#'   packages will by default be installed into a self-contained conda or venv
#'   environment named "r-reticulate". Note that "conda" is the only supported
#'   method on M1 Mac.
#'
#'   If you initially declined the miniconda installation prompt, you can later
#'   manually install miniconda by running [`reticulate::install_miniconda()`].
#'
#' @section Custom Installation: `install_tensorflow()` or
#'   `keras::install_keras()` isn't required to use tensorflow with the package.
#'   If you manually configure a python environment with the required
#'   dependencies, you can tell R to use it by pointing reticulate at it,
#'   commonly by setting an environment variable:
#'
#'   ``` R
#'   Sys.setenv("RETICULATE_PYTHON" = "~/path/to/python-env/bin/python")
#'   ```
#'
#' @section Apple Silicon: Beginning with Tensorflow version 2.13, the default
#'   tensorflow package now works on Apple Silicon. See
#'   \url{https://developer.apple.com/metal/tensorflow-plugin/} for instructions
#'   on how to install older versions of Tensorflow on macOS. Please note that
#'   not all operations are supported on Arm Mac GPUs. You can work around the
#'   missing operations by pinning operations to CPU. For example:
#'
#'   ```` R
#'   x <- array(runif(64*64), c(1, 64, 64))
#'   keras::layer_random_rotation(x, .5)  # Error:
#'   # No registered 'RngReadAndSkip' OpKernel for 'GPU' devices
#'   # Pin the operation to the CPU to avoid the error
#'   with(tf$device("CPU"), keras::layer_random_rotation(x, .5) ) # No Error
#'   ````
#'
#' @section Additional Packages:
#'
#'   If you wish to add additional PyPI packages to your Keras / TensorFlow
#'   environment you can either specify the packages in the `extra_packages`
#'   argument of `install_tensorflow()` or `install_keras()`, or alternatively
#'   install them into an existing environment using the
#'   [reticulate::py_install()] function. Note that `install_keras()` includes a
#'   set of additional python packages by default, see `?keras::install_keras`
#'   for details.
#'
#' @md
#'
#' @inheritParams reticulate::py_install
#'
#' @param version TensorFlow version to install. Valid values include:
#'
#'   +  `"default"` installs  `r default_version`
#'
#'   + `"release"` installs the latest release version of tensorflow (which may
#'   be incompatible with the current version of the R package)
#'
#'   + A version specification like `"2.4"` or `"2.4.0"`. Note that if the patch
#'   version is not supplied, the latest patch release is installed (e.g.,
#'   `"2.4"` today installs version "2.4.2")
#'
#'   + `nightly` for the latest available nightly build.
#'
#'   + To any specification, you can append "-cpu" to install the cpu version
#'   only of the package (e.g., `"2.4-cpu"`)
#'
#'   + The full URL or path to a installer binary or python *.whl file.
#'
#' @param extra_packages Additional Python packages to install along with
#'   TensorFlow.
#'
#' @param restart_session Restart R session after installing (note this will
#'   only occur within RStudio).
#'
#' @param python_version,conda_python_version Pass a string like "3.8" to
#'   request that conda install a specific Python version. This is ignored when
#'   attempting to install in a Python virtual environment. Note that the Python
#'   version must be compatible with the requested Tensorflow version,
#'   documented here:
#'   <https://www.tensorflow.org/install/pip#system-requirements>
#'
#' @param cuda logical `TRUE` or `FALSE`. If `install_tensorflow()` detects the platform is
#'   Linux, an Nvidia GPU is available, and the TensorFlow version is 2.14 (the
#'   default), it will install also install the required CUDA libraries through pip.
#'
#' @param metal Whether to install `tensorflow-metal` pip package on Arm Macs.
#'   This enables tensorflow to use the GPU. Pass a string to install a specific
#'   version like `"tensorflow-metal==0.7.*`.
#'
#' @param pip_ignore_installed Whether pip should ignore installed python
#'   packages and reinstall all already installed python packages.
#'
#' @param new_env If `TRUE`, any existing Python virtual environment and/or
#'   conda environment specified by `envname` is deleted first.
#'
#' @param ... other arguments passed to [`reticulate::conda_install()`] or
#'   [`reticulate::virtualenv_install()`], depending on the `method` used.
#'
#' @seealso
#' -  [`keras::install_keras()`]
#' -  <https://tensorflow.rstudio.com/reference/tensorflow/install_tensorflow>
#'
#' @export
install_pirat <-
  function(method = c("auto", "virtualenv", "conda"),
           conda = "auto",
           envname = "r-pirat",
           restart_session = TRUE,
           channel = c("pytorch", "stable"),
           pip_ignore_installed = FALSE,
           new_env = identical(envname, "r-pirat")
           ) {
    
    method <- match.arg(method)
    
    requested_versions <- list(
      torch = '1.10.0',
      numpy = '1.20.2',
      python = '3.9.5'
    )
    
    packages <- c('torch==1.10.0', 'numpy==1.20.2', 'matplotlib')
    #packages <- c('pytorch', 'numpy', 'matplotlib')
    python_version <- '3.9.5'
    
    # verify 64-bit
    if (.Machine$sizeof.pointer != 8) {
      stop("Unable to install TensorFlow on this platform.",
           "Binary installation is only available for 64-bit platforms.")
    }
    
    # some special handling for windows
    if (is_windows()) {
      
      # avoid DLL in use errors
      if (py_available()) {
        stop("You should call install_pirat() only in a fresh ",
             "R session that has not yet initialized Pirat (this is ",
             "to avoid DLL in use errors during installation)")
      }
      
      #if(grepl("gpu", as.character(version), ignore.case = TRUE))
      #  warning("Caution: TensorFlow 2.10 was the last TensorFlow release that supported GPU on native-Windows. Starting with TensorFlow 2.11, you will need to install TensorFlow in WSL2, or install a CPU-only version of TensorFlow.",
      #          if(identical(.Platform$GUI, "RStudio")) " For a guide on how to use RStudio with WSL2, see https://support.posit.co/hc/en-us/articles/360049776974-Using-RStudio-Server-in-Windows-WSL2")
      
    }
    
    
    #can_use_gpu <- FALSE
    #if (is.null(cuda)) {
    #  
    #  can_use_gpu <-
    #    is_linux() &&
    #    (version %in% c("default", "release") ||
    #       isTRUE(extract_numeric_version(version) >= "2.14")) &&
     #   tryCatch(as.logical(length(system("lspci | grep -i nvidia", intern = TRUE))),
    #             warning = function(w) FALSE) # warning emitted by system for non-0 exit stat
    #  
    #  cuda <- can_use_gpu
    #  
    #}
    
    #tf_package_spec <- parse_tensorflow_version(version)
    
    #if(isTRUE(cuda) && !grepl("^.*\\.whl$", tf_package_spec)) {
    #  tf_package_spec <- sub("([^=<>!]*)(.*)", "\\1[and-cuda]\\2",
    #                         tf_package_spec)
    #}
    
    #packages <- unique(c(
    #  tf_package_spec,
    #  as.character(extra_packages)
    #))
    
    
    #if (isTRUE(metal)) repeat {
    #  tf_ver <- extract_numeric_version(tf_package_spec)
    #  if(is.na(tf_ver))
    #    break
    #  
    #  if(tf_ver >= "2.14")
    #    metal <- "tensorflow-metal>1.0.1"
    #  else if (tf_ver >= "2.13")
     #   metal <- "tensorflow-metal>=1.0.1"
    #  else if (tf_ver >= "2.12")
    #    metal <- "tensorflow-metal==0.8.*"
    #  else
    #    # https://pypi.org/project/tensorflow-metal/
    #    metal <- "tensorflow-metal"
    #  
    #  break
    #}
    python_version <- python_version %||% conda_python_version
    if(method %in% c("auto", "virtualenv") &&
       is.null(python_version)) {
     # 
    #  # virtualenv_starter() picks the most recent version available, but older
    #  # versions of tensorflow typically don't work with the latest Python
    # # # release. In general, we're better off picking the oldest Python version available
    #  # that works with the current release of tensorflow.
    #  # TF 2.13 is compatible with Python <=3.11,>=3.8
      
      available <- reticulate::virtualenv_starter(version = ">=3.9", all = TRUE)
    #  # pick the smallest minor version, ignoring patchlevel
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
    
    reticulate::install_python(version = '3.9.5')
    
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
