#' @title Creates a BasiliskEnvironment class
#' @description Please refer to the package `basilisk`.
#' @importFrom basilisk BasiliskEnvironment
#' @export
#' @return An instance of the class `BasiliskEnvironment`
#' 
envPirat <- basilisk::BasiliskEnvironment("envPirat",
  pkgname = "Pirat",
  packages = c("python=3.9.5", "numpy==1.20.2", "pytorch==1.10.0"),
  channels = c("conda-forge", "pytorch", "stable", "torch")
  ,path = "myModules"
  )
