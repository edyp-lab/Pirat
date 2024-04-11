#' @title Sub-Bouyssie dataset
#' @name subbouyssie
#' @rdname subbouyssie
#' @docType data
#' @keywords data
#' @description This dataset is extracted from the original `Bouyssie2020`
#' dataset mentionned in Pirat article, where only 5 PGs were randomly selected.
#' @format A list containing:
#' - peptides_ab: numeric matrix of peptide (or precrusors) log2 abundances.
#' - adj: adjacency matrix between peptides and PGs.
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
#' @keywords datasets
NULL




#' @title Ropers dataset
#' @name ropers
#' @rdname ropers
#' @docType data
#' @keywords data
#' @description This dataset corresponds to `Ropers2021` dataset, described in 
#' Pirat article.
#' @format A list containing:
#' - peptides_ab: numeric matrix of precrusors log2 abundances.
#' - adj: adjacency matrix between peptides and PGs
#' - rnas_ab: numeric matrix of gene expression log2 counts from mRNA analysis.
#' - adj_rna_pg: adjacency matrix between genes and PGs
#' - charges: charge of each precursor.
#' - modifs: post-translational modification of each precursor.
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
NULL



#' @title Sub-Ropers dataset
#' @name subropers
#' @rdname subropers
#' @docType data
#' @keywords data
#' @description This dataset is extracted from the original `Ropers2021`
#' dataset described in Pirat article, where only 10 PGs were randomly selected.
#' @format A list containing:
#' - peptides_ab: numeric matrix of peptide (or precrusors) log2 abundances.
#' - adj: adjacency matrix between peptides and PGs
#' - rnas_ab: numeric matrix of gene expression log2 counts from mRNA analysis.
#' - adj_rna_pg: adjacency matrix between genes and PGs
#' @return A dataset
#' @references Ropers, D., Couté, Y., Faure, L., Ferré, S., Labourdette, D., 
#' Shabani, A., Trouilh, L., Vasseur, P., Corre, G., Ferro, M., Teste, M. A., 
#' Geiselmann, J., & de Jong, H. (2021). Multiomics Study of Bacterial Growth 
#' Arrest in a Synthetic Biology Application. ACS Synthetic Biology, 10(11), 
#' 2910–2926. 
#' https://doi.org/10.1021/ACSSYNBIO.1C00115/SUPPL_FILE/SB1C00115_SI_010.ZIP
#'
#' @keywords datasets
NULL
