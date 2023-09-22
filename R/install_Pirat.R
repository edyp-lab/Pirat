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
#' @import rTorch
#' 
#' @examples
#' \dontrun{
#' install_Pirat()
#' }
#' 
#' 
install_Pirat <- function(
    envname = "r-pirat",
    new_env = identical(envname, "r-pirat"),
    restart_session = TRUE
    ) {
  
  
  torch_version <- "1.10.0"
  python_version <- "3.9.5"
  pkgs <- c("cpuonly", 
            "matplotlib", 
            'numpy=1.20.2')
  
  # Check if Python is already installed. If not, install conda
  # Miniconda appears to be not 100% reliable, specially in macOS. 
  # It is strongly recommend using full conda for your PyTorch installation.
  
  
  rTorch::install_pytorch(method = c("conda", "virtualenv", "auto"),
                  conda = "auto",
                  version = torch_version,
                  envname = envname,
                  extra_packages = pkgs,
                  restart_session = TRUE,
                  conda_python_version = python_version,
                  pip = FALSE,
                  channel = "stable",
                  cuda_version = NULL,
                  dry_run = FALSE)
  
  cat("\nInstallation complete.\n\n")
  
  
  is.rstudio <- function(){
    .Platform$GUI == "RStudio"
  }
  
  
  
  if (restart_session){
    if (is.rstudio() &&
      requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::hasFun("restartSession"))
    rstudioapi::restartSession(command='library(Pirat)')
    else
      cat("Please restart the R session and reload the 'Pirat' package.")
  }
  
  invisible(NULL)
}

