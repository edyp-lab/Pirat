#' @title Creates a BasiliskEnvironment class
#' @description Please refer to the package `basilisk`.
#' @importFrom basilisk BasiliskEnvironment
#' @export
#' @return An instance of the class `BasiliskEnvironment`
#' 
envPirat <- basilisk::BasiliskEnvironment("envPirat",
    pkgname = "Pirat",
    packages = c("python=3.10", "pytorch=2.0.0", "cpuonly=2.0", "numpy=1.24"), #, "numpy", "pytorch", "cpuonly"),
    #channels = c("conda-forge", "pytorch", "torch", "nodefaults"),
    channels = c("pytorch", "conda-forge", "defaults"),
    path = "myModules"
)
