

# Remove peptides with number of MVs higher or equal to t
remove_NA_pep_reset <- function(l_pep_rna, 
                                t, 
                                percentage=FALSE) {
  nsample = nrow(l_pep_rna$rnas_ab)
  if (percentage) {
    t = ceiling(nsample*t)
  }
  mv_count_pep = colSums(is.na(l_pep_rna$peptides_ab))
  i_pep_rm = which(mv_count_pep >= t)
  l_pep_rna = remove_pep_from_idx(l_pep_rna, i_pep_rm)
  return(l_pep_rna)
}

remove_Os_rna_reset <- function(l_pep_rna, cond_idx, n_cond_min=2,
                                offset_log = 1) {
  rnas_ab = 2^l_pep_rna$rnas_ab - offset_log
  count_present = matrix(FALSE, length(unique(cond_idx)), ncol(rnas_ab))
  for (cond in unique(cond_idx)) {
    .tmp <- rnas_ab[cond_idx == cond, , drop = FALSE]
    count_present[cond, ] <- colSums(.tmp > 0) >= 1
  }
  idx_rna_2_rm = which(colSums(count_present) < n_cond_min)
  if (length(idx_rna_2_rm) != 0) {
    l_pep_rna$adj_rna_pg <- l_pep_rna$adj_rna_pg[-idx_rna_2_rm, ]
    l_pep_rna$rnas_ab <- l_pep_rna$rnas_ab[, -idx_rna_2_rm]
    return(l_pep_rna)
  }
  else {
    return(l_pep_rna)
  }

}



# Remove shared peptides
remove_shared_pep <- function(l_pep_rna) {
  nsample = nrow(l_pep_rna$rnas_ab)
  i_pep_rm = which(rowSums(l_pep_rna$adj) >= 2)
  if (length(i_pep_rm) != 0) {
    new_adj = l_pep_rna$adj[-i_pep_rm,]
    i_rna_rm = which(colSums(new_adj) == 0)
    new_pep = l_pep_rna$peptides_ab[,-i_pep_rm]
    new_z = l_pep_rna$charges[-i_pep_rm]
    new_modifs = l_pep_rna$modifs[-i_pep_rm]
    if (length(i_rna_rm) != 0) {
      new_rna = l_pep_rna$rnas_ab[,-i_rna_rm]
      new_adj = new_adj[,-i_rna_rm]
      return(
        list("peptides_ab" = new_pep, 
                  "rnas_ab" = new_rna, 
                  "adj" = new_adj,
                  "charges" = new_z, 
                  "modifs" = new_modifs)
        )
    }
    else {
      return(
        list("peptides_ab" = new_pep, 
                  "rnas_ab" = l_pep_rna$rnas_ab,
                  "adj" = new_adj, 
                  "charges" = new_z, 
                  "modifs" = new_modifs)
        )
    }

  }
  else {
    return(l_pep_rna)
  }
}
