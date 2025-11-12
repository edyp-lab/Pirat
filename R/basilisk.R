#' @title Creates a BasiliskEnvironment class
#' @description Please refer to the package `basilisk`.
#' @importFrom basilisk BasiliskEnvironment
#' @export
#' @return An instance of the class `BasiliskEnvironment`
#' 
# If machine is Linux or Windows, add +cpu to torch version
torch_version <- if (Sys.info()["sysname"] == "Darwin") {
  "torch==1.13.1"
} else {
  "torch==1.13.1+cpu"
}
envPirat <- basilisk::BasiliskEnvironment(
  "envPirat",
  pkgname = "Pirat",
  packages = c("python=3.10", torch_version, "numpy=1.21"), 
  path = "myModules"
)