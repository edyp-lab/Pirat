#' @title Creates a BasiliskEnvironment class
#' @description Please refer to the package `basilisk`.
#' @importFrom basilisk BasiliskEnvironment
#' @export
#' @return An instance of the class `BasiliskEnvironment`
#' 
envPirat <- basilisk::BasiliskEnvironment("envPirat",
    pkgname = "Pirat",
    packages = c("python==3.10", "numpy==1.21", "pytorch==1.12.0", "cpuonly==2.0"),
    #channels = c("conda-forge", "pytorch", "torch", "nodefaults"),
    channels = c("conda-forge", "pytorch"),
    path = "myModules"
)
