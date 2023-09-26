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
#' 
#' @export
#' 
#' @examples
#' data(bouyssie)
#' create_SE(bouyssie$peptides_ab, bouyssie$adj, bouyssie$mask_prot_diff, bouyssie$mask_pep_diff )
#' 
pirat2SE <- function(peptides_ab, 
                      adj, 
                      mask_prot_diff, 
                      mask_pep_diff){
  
 obj <- SummarizedExperiment(assays = as.matrix(t(peptides_ab), row.names = colnames(t(peptides_ab))), 
                       colData = DataFrame(Condition = colnames(t(peptides_ab)),
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
#' @param ... xxx
#' 
#' @import SummarizedExperiment
#' 
#' @export
#' 
#' @examples
#' data(bouyssie)
#' 

wrapper_pipeline_llkimpute <- function(se, ...){
  
  obj <- list(
    peptides_ab = t(assay(se)),
    adj = metadata(se)$adj,
    mask_prot_diff = metadata(se)$mask_prot_diff,
    mask_pep_diff = metadata(se)$mask_pep_diff
  )

  res.mle.transp <- pipeline_llkimpute(obj, ...)
  res.mle.transp
}