#' @title Install python environment for Pirat
#'
#' @description This function may be run only once and use the package
#' `basilisk` to install the Python environment needed to run the 
#' imputation functions in the package `Pirat`
#'
#' @return A list of names of objects exposed in each module.
#' @author Samuel Wieczorek
#' 
#' @examples
#' install_Pirat_env()
#' 
#' @export
#' @import basilisk
#' 
install_Pirat_env <- function() {
    cl <- basiliskStart(envPirat)
    pirat.install <- basiliskRun(cl, function() { 
        pkgs <- basilisk::listPackages(env = envPirat)
        path <- normalizePath(basilisk::obtainEnvironmentPath(envPirat))
        list(
            location = path,
            python = basilisk::listPythonVersion(env = envPirat),
            torch = pkgs[which(pkgs$package=='torch'),]$full,
            numpy = pkgs[which(pkgs$package=='numpy'),]$full
        )
    })
    basiliskStop(cl)
    
    cat('Installed packages:\n')
    cat(paste0("\t", names(pirat.install), " (", pirat.install, ")\n"))
}
