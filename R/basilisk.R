#' @title Creates a BasiliskEnvironment class
#' @description Please refer to the package `basilisk`.
#' @importFrom basilisk BasiliskEnvironment
#' @export
#' @return An instance of the class `BasiliskEnvironment`
#' 
# If machine is Linux or Windows, add to torch version
packages_versions <- if (Sys.info()["sysname"] == "Darwin") {
  c("python=3.10", "torch=1.13.1", "numpy=1.21")
} else {
  c("python=3.10", "torch=2.5.1", "numpy=1.24")
}
envPirat <- basilisk::BasiliskEnvironment(
  "envPirat",
  pkgname = "Pirat",
  packages = packages_versions, 
  path = "myModules"
)