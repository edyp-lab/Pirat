#' @title COnvert Pirat dataset to SummarizedExperiment
#' @description This function converts the original dataset structure into 
#' a SummarizedExperiment .
#' 
#' @param peptides_ab the peptide or precursor abundance matrix to impute, 
#' with samples in row and peptides or precursors in column;
#' @param adj a n_peptide x n_protein adjacency matrix between peptides and 
#' proteins containing 0 and 1, or TRUE and FALSE.
#' Can contain: 
#' **rnas_ab**, the mRNA normalized count matrix, with samples in 
#' row and mRNAs in column;
#' **adj_rna_pg**, a n_mrna x n_protein adjacency matrix n_mrna and proteins 
#' containing 0 and 1, or TRUE and FALSE; 
#' @param mask_prot_diff (Optional) boolean vector of size equal to the number of proteins, indicating
#' whether proteins are ground truth differentially abundant (typically in spike-in benchmark datasets).
#' @param mask_pep_diff (Optional) boolean vector of size equal to the number of peptides, indicating
#' whether peptides are ground truth differentially abundant (typically in spike-in benchmark datasets).
#' 
#' @return An instance of the class `SummarizedExperiment`
#' 
#' @import SummarizedExperiment
#' 
#' @export
#' 
#' @examples
#' data(subbouyssie)
#' peptides_ab <- subbouyssie$peptides_ab
#' adj <- subbouyssie$adj
#' mask_prot_diff <- subbouyssie$mask_prot_diff
#' mask_pep_diff <- subbouyssie$mask_pep_diff
#' obj <- pirat2SE(peptides_ab, adj, mask_prot_diff, mask_pep_diff )
#' obj
#' 
#' @import SummarizedExperiment 
#' @importFrom S4Vectors metadata
#' 
pirat2SE <- function(peptides_ab, 
                     adj, 
                     mask_prot_diff = NULL, 
                     mask_pep_diff = NULL){

 obj <- SummarizedExperiment::SummarizedExperiment(
   assays = as.matrix(t(peptides_ab), row.names = colnames(t(peptides_ab))), 
   colData = data.frame(Condition = colnames(t(peptides_ab)),
                        Sample = colnames(t(peptides_ab)),
                        row.names = colnames(t(peptides_ab)))
   )
 
 metadata(obj)$adj <- adj
 metadata(obj)$mask_prot_diff <- mask_prot_diff
 metadata(obj)$mask_pep_diff <- mask_pep_diff
  
  obj
}




#' @title Imputation method using SummarizedExperiment dataset
#' @description This function imputes data from an instance of the
#' SummarizedExperiment structure data. After a conversion step, it calls the 
#' function `my_pipeline_llkimpute`.
#' 
#' @param se An instance of the class SummarizedExperiment
#' @param ... Additional arguments to pass to `my_pipeline_llkimpute()`
#' 
#' 
#' @export
#' 
#' @examples
#' data(subbouyssie)
#' obj <- pirat2SE(subbouyssie$peptides_ab, subbouyssie$adj, 
#' subbouyssie$mask_prot_diff, subbouyssie$mask_pep_diff )
#' res <- wrapper_pipeline_llkimpute(obj)
#' 
#' @return See my_pipeline_llkimpute() function
#' @importFrom S4Vectors metadata
#' 
wrapper_pipeline_llkimpute <- function(se, ...){
  
  stopifnot(inherits(obj, 'SummarizedExperiment'))
  
  obj <- list(
    peptides_ab = t(assay(se)),
    adj = metadata(se)$adj,
    mask_prot_diff = metadata(se)$mask_prot_diff,
    mask_pep_diff = metadata(se)$mask_pep_diff
  )

  my_pipeline_llkimpute(obj, ...)
}


