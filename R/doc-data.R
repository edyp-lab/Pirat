#' @title Sub-Bouyssie dataset
#' @name subbouyssie
#' @rdname subbouyssie
#' @docType data
#' @keywords data
#' @description This dataset is extracted from the original `Bouyssie2020`
#' dataset mentionned in Pirat article, where only 5 PGs were randomly selected.
#'
#' @references Bouyssié, D., Hesse, A. M., Mouton-Barbosa, E., Rompais, 
#' M., MacRon, C., Carapito, C., Gonzalez De Peredo, A., Couté, Y., Dupierris, 
#' V., Burel, A., Menetrey, J. P., Kalaitzakis, A., Poisat, J., Romdhani, A., 
#' Burlet-Schiltz, O., Cianférani, S., Garin, J., & Bruley, C. (2020). 
#' Proline: an efficient and user-friendly software suite for large-scale 
#' proteomics. Bioinformatics, 36(10), 3148–3155. 
#' https://doi.org/10.1093/BIOINFORMATICS/BTAA118
#'
#' @return A dataset
#' 
#' @format 
#' A list of two items:
#' * peptides_ab: a data.frame() containing the abundance of 31 peptides 
#' (columns) over a series of 40 samples (lines) (10 conditions, 4 replicates 
#' for each condition)
#' * adj: A matrix which is the adjacency matrix for the slot peptides_ab.
#'
#' @keywords datasets
NULL




#' @title Ropers dataset
#' @name ropers
#' @rdname ropers
#' @docType data
#' @keywords data
#' @description This dataset corresponds to `Ropers2021` dataset, described in 
#' Pirat article.
#'
#' @return A dataset
#' @references Ropers, D., Couté, Y., Faure, L., Ferré, S., Labourdette, D., 
#' Shabani, A., Trouilh, L., Vasseur, P., Corre, G., Ferro, M., Teste, M. A., 
#' Geiselmann, J., & de Jong, H. (2021). Multiomics Study of Bacterial Growth 
#' Arrest in a Synthetic Biology Application. ACS Synthetic Biology, 10(11), 
#' 2910–2926. 
#' https://doi.org/10.1021/ACSSYNBIO.1C00115/SUPPL_FILE/SB1C00115_SI_010.ZIP
#'
#' @keywords datasets
#' 
#' @format 
#' A list of six items:
#' * peptides_ab: the peptide or precursor abundance matrix to impute, 
#' with samples in row and peptides or precursors in column
#' * rnas_ab: a n_peptide x n_protein adjacency matrix between peptides and 
#' proteins containing 0 and 1, or TRUE and FALSE.
#' * adj: A matrix which is the adjacency matrix for the slot peptides_ab.
#' * adj_rna_pg: a n_mrna x n_protein adjacency matrix n_mrna and proteins 
#' containing 0 and 1, or TRUE and FALSE
#' * charges: xxx
#' * modifs: xxx
#' 
#' 
NULL



#' @title Sub-Ropers dataset
#' @name subropers
#' @rdname subropers
#' @docType data
#' @keywords data
#' @description This dataset is extracted from the original `Ropers2021`
#' dataset described in Pirat article, where only 10 PGs were randomly selected.
#'
#' @return A dataset
#' @references Ropers, D., Couté, Y., Faure, L., Ferré, S., Labourdette, D., 
#' Shabani, A., Trouilh, L., Vasseur, P., Corre, G., Ferro, M., Teste, M. A., 
#' Geiselmann, J., & de Jong, H. (2021). Multiomics Study of Bacterial Growth 
#' Arrest in a Synthetic Biology Application. ACS Synthetic Biology, 10(11), 
#' 2910–2926. 
#' https://doi.org/10.1021/ACSSYNBIO.1C00115/SUPPL_FILE/SB1C00115_SI_010.ZIP
#'
#' @keywords datasets
#' 
#' @format 
#' A list of six items:
#' * peptides_ab: the peptide or precursor abundance matrix to impute, 
#' with samples in row and peptides or precursors in column
#' * rnas_ab: a n_peptide x n_protein adjacency matrix between peptides and 
#' proteins containing 0 and 1, or TRUE and FALSE.
#' * adj: A matrix which is the adjacency matrix for the slot peptides_ab.
#' * adj_rna_pg: a n_mrna x n_protein adjacency matrix n_mrna and proteins 
#' containing 0 and 1, or TRUE and FALSE
#' * charges: xxx
#' * modifs: xxx
#' 
NULL