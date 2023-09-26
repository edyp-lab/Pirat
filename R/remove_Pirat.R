#' @title xxx
#' @description xxx
#' 
#' @param envname xxx
#' @param conda xxx
#' 
#' @return NULL
#' 
#' @export
#' 
remove_Pirat <- function(envname = pirat_envname,
                         conda = 'auto') {
  
   # find if environment exists
  venv_exists <- envname %in% reticulate::virtualenv_list()
  conda_env_exists <- envname %in% reticulate::conda_list(conda=conda)$name
  
  if (!venv_exists && !conda_env_exists){
    message('No pirat environment was found')
    } else {
      # remove environment
      if (venv_exists) {
        message("Removing ", envname, " virtualenv environment... \n")
        reticulate::virtualenv_remove(envname = envname, confirm = FALSE)
        }
      
      if (conda_env_exists) {
        message("Removing ", envname, " conda environment... \n")
        reticulate::conda_remove(envname = envname, conda = conda)
      }
    }
}


