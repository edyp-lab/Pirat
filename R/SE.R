#' @title COnvert Pirat dataset to SummarizedExperiment
#' @description This function converts the original dataset structure into 
#' a SummarizedExperiment .
#' 
#' @param peptides_ab xxx
#' @param adj xxx
#' @param mask_prot_diff xxx
#' @param mask_pep_diff xxx
#' 
#' @import SummarizedExperiment
#' @import S4Vectors
#' 
#' @export
#' 
#' @examples
#' data(bouyssie)
#' pirat2SE(bouyssie$peptides_ab, bouyssie$adj, bouyssie$mask_prot_diff, bouyssie$mask_pep_diff )
#' 
pirat2SE <- function(peptides_ab, 
                      adj, 
                      mask_prot_diff, 
                      mask_pep_diff){
  
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Please install SummarizedExperiment: BiocManager::install('SummarizedExperiment')")
  }
  if (!requireNamespace("S4Vectors", quietly = TRUE)) {
    stop("Please install S4Vectors: BiocManager::install('S4Vectors')")
  }
  
 obj <- SummarizedExperiment(assays = as.matrix(t(peptides_ab), row.names = colnames(t(peptides_ab))), 
                       colData = S4Vectors::DataFrame(Condition = colnames(t(peptides_ab)),
                                         Sample = colnames(t(peptides_ab)),
                                         row.names = colnames(t(peptides_ab)))
  )
 
 S4Vectors::metadata(obj)$adj <- adj
 S4Vectors::metadata(obj)$mask_prot_diff <- mask_prot_diff
 S4Vectors::metadata(obj)$mask_pep_diff <- mask_pep_diff
  
  obj
}




#' @title xxx
#' @description xxx
#' 
#' @param se xxx
#' @param ... Additional arguments to pass to `pipeline_llkimpute()`
#' 
#' @import SummarizedExperiment
#' 
#' @export
#' 
#' @examples
#' data(bouyssie)
#' obj <- pirat2SE(bouyssie$peptides_ab, bouyssie$adj, bouyssie$mask_prot_diff,  bouyssie$mask_pep_diff )
#' wrapper_pipeline_llkimpute(obj)
#' wrapper_pipeline_llkimpute(obj, extension = '2', mcar = TRUE)
#' 

wrapper_pipeline_llkimpute <- function(se, ...){
  
  stopifnot(inherits(obj, 'SummarizedExperiment'))
  
  obj <- list(
    peptides_ab = t(assay(se)),
    adj = metadata(se)$adj,
    mask_prot_diff = metadata(se)$mask_prot_diff,
    mask_pep_diff = metadata(se)$mask_pep_diff
  )

  pipeline_llkimpute(obj, ...)
  }