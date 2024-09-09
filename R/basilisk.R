#' @title Creates a BasiliskEnvironment class
#' @description Please refer to the package `basilisk`.
#' @importFrom basilisk BasiliskEnvironment
#' @export
#' @return An instance of the class `BasiliskEnvironment`
#' 
envPirat <- basilisk::BasiliskEnvironment("envPirat",
  pkgname = "Pirat",
  packages = c("numpy==1.20.2", "pytorch==1.10.0"),
  channels = c("pytorch", "torch", "stable")
  ,path = "myModules"
  )
