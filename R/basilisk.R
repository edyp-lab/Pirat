#' @title Creates a BasiliskEnvironment class
#' @description Please refer to the package `basilisk`.
#' @importFrom basilisk BasiliskEnvironment
#' @export
#' @return An instance of the class `BasiliskEnvironment`
#'
envPirat <- basilisk::BasiliskEnvironment(
  "envPirat",
  pkgname = "Pirat",
  packages = c("python=3.10", "torch=2.5.1", "numpy=1.24"), 
  path = "myModules"
)