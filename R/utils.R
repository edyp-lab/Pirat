#' @title Get Operating System
#' @description Get the operating system 
#'
#' @export
#'
#' @examples
#' get_os()
#'
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

#' @title Indexes of PGs embedded in each others
#' @description Returns indexes of PGs that are embedded in others
#'
#' @param adj An adjacency matrix between precursors/peptides and PGs
#'
#' @return A vector of indices
#' @export
#'
#' @examples
#' NULL
#'
get_indexes_embedded_prots <- function(adj) {
  similarity.mat = t(adj) %*% adj
  mat.size = ncol(similarity.mat)
  idx.prot = 1
  idx.prot.rm = c()
  for (idx.prot in 1:mat.size) {
    if (any(similarity.mat[idx.prot, idx.prot] ==
            similarity.mat[idx.prot, -c(idx.prot, idx.prot.rm)])) {
      idx.prot.rm = c(idx.prot.rm, idx.prot)
    }
  }
  return(idx.prot.rm)
}


#' @title Remove PGs by index and merge
#' @description Remove PG by index and merge transcripts (if transcriptomic information is available)
#' of PG included in one another (under condition that they have peptide).
#' Then it removes transcripts without PG. Do not remove peptides that are left without PG.
#'
#' @param l_pep_rna A list representing dataset, formatted as in pipeline_llkimpute function
#' @param pg_idx Vector of indices
#'
#' @return A list representing dataset.
#' @export
#'
#' @examples
#' NULL
#'
rm_pg_from_idx_merge_pg <- function(l_pep_rna, pg_idx) {
  if (!(length(pg_idx) == 0) & !is.null(pg_idx)) {
    adj2keep = l_pep_rna$adj[, -pg_idx, drop = F]
    adj2rm = l_pep_rna$adj[, pg_idx, drop = F]
    l_pep_rna$adj = adj2keep
    if (!is.null(l_pep_rna$adj_rna_pg)) {
      n_pep_per_pg2rm = colSums(adj2rm)
      pg_sim_mat = ((t(adj2rm) %*% adj2keep) == n_pep_per_pg2rm) &
        (n_pep_per_pg2rm != 0)
      idx.pg2merge = which(pg_sim_mat, arr.ind = T)
      adj2keep_rna = l_pep_rna$adj_rna_pg[, -pg_idx, drop = F]
      adj2rm_rna = l_pep_rna$adj_rna_pg[, pg_idx, drop = F] 
      if (length(idx.pg2merge) != 0) {
        for (i in 1:nrow(idx.pg2merge)) {
          adj2keep_rna[, idx.pg2merge[i, 2]] = adj2keep_rna[, idx.pg2merge[i, 2]] | 
            adj2rm_rna[, idx.pg2merge[i, 1]]
        }
      }
      l_pep_rna$adj_rna_pg = adj2keep_rna
      i_rna_rm = which(rowSums(adj2keep_rna) == 0)
      if (length(i_rna_rm) != 0) {
        l_pep_rna$rnas_ab = l_pep_rna$rnas_ab[,-i_rna_rm]
        l_pep_rna$adj_rna_pg = l_pep_rna$adj_rna_pg[-i_rna_rm, ]
      }
    }
    else {
      l_pep_rna$adj = adj2keep
      if (!is.null(l_pep_rna$mask_prot_diff)) {
        l_pep_rna$mask_prot_diff = l_pep_rna$mask_prot_diff[-pg_idx]
      }
    }
    
  }
  return(l_pep_rna)
}


#' @title Remove peptides by index
#' 
#' @description Remove peptide by index, then deletes empty PGs
#'
#' @param l_pep_rna List representing the dataset
#' @param pepidxs Vector of indices
#'
#' @export
#' @return List representing the dataset
#'
remove_pep_from_idx <- function(l_pep_rna,
                                pepidxs) {
  i_pep_rm = pepidxs
  if (length(i_pep_rm) != 0) {
    l_pep_rna$adj = l_pep_rna$adj[-i_pep_rm,]
    l_pep_rna$peptides_ab = l_pep_rna$peptides_ab[,-i_pep_rm]
    if (!is.null(l_pep_rna$mask_pep_diff)) {
      l_pep_rna$mask_pep_diff = l_pep_rna$mask_pep_diff[-i_pep_rm]
    }
    if (!is.null(l_pep_rna$charges)) {
      l_pep_rna$charges = l_pep_rna$charges[-i_pep_rm]
    }
    if (!is.null(l_pep_rna$modifs)) {
      l_pep_rna$modifs = l_pep_rna$modifs[-i_pep_rm]
    }
    i_pg_rm = which(colSums(l_pep_rna$adj) == 0)
    if (length(i_pg_rm) != 0) {
      l_pep_rna = rm_pg_from_idx_merge_pg(l_pep_rna, i_pg_rm)
    }
  }
  return(l_pep_rna)
}


#' @title Split too large PGs
#' 
#' @description Split PGs with too many peptides/precursors. It creates different PGs with
#' size equal to size max. Hence, some peptides can be duplicated in the new PGs.
#'
#' @param adj Adjacency matrix between peptides and PGs.
#' @param size_max Maximum PG size desired.
#'
#' @return New adjacency matrix between peptides and PGs.
#' @export
#'
#' @examples
#' NULL
#'
split_large_pg = function(adj, size_max) {
  set.seed(1234)
  idx_pg_too_large = which(colSums(adj) >= size_max)
  if (length(idx_pg_too_large) > 0) {
    new_adj = adj
    npeps = nrow(adj)
    l_adjs2bind = list()
    idx_adj2bind = 1
    for (i in 1:length(idx_pg_too_large)) {
      cur_pg_idx = idx_pg_too_large[i]
      cur_pg = adj[, cur_pg_idx]
      idx_pg_pep = which(cur_pg == 1)
      n_groups = ceiling(sum(cur_pg) / size_max)
      groups_idx_pg_pep = suppressWarnings(
        split(sample(idx_pg_pep, replace = F), 1:n_groups)
      )
      for (j in 1:length(groups_idx_pg_pep)) {
        new_pg = rep(F, npeps)
        idx2add = setdiff(idx_pg_pep, groups_idx_pg_pep[[j]])
        n2samples = size_max - length(groups_idx_pg_pep[[j]])
        idx2add = sample(idx2add, n2samples)
        new_pg[c(groups_idx_pg_pep[[j]], idx2add)] = T
        l_adjs2bind[[idx_adj2bind]] = new_pg
        idx_adj2bind = idx_adj2bind + 1
      }
    }
    return(cbind(new_adj, do.call(cbind, l_adjs2bind))[, -idx_pg_too_large])
  }
  else {
    return(adj)
  }
}

# TODO: Rename this function in code. 
# TODO: Impfunc shoud not be passed as parameter, by we should directly call "estimate params & impute.
# TODO: Some parameters are not used anymore in pipeline_ll_imp file, need to remove tham
#' @title Impute each PG.
#' @description Imputes each PG separately and return the results for each PG. 
#'
#' @param data.pep.rna.crop A list representing dataset
#' @param impfunc Imputation function
#' @param psi Inverse scale parameter for IW prior of peptides abundances
#' @param pep_ab_or In case we impute a dataset with pseudo-MVS, we can provide the ground truth abundance table, 
#' such that imputation will by done only for pseudo-MVs. This will accelerate imputation algorithm.
#' @param prot.idxs Vector of indices of PGs to impute
#' @param df Estimate degree of freedom of the IG distribution fitted on observed variance.
#' @param nu_factor Multiplication factor on degree of freedom. 2 by default.
#' @param max_pg_size Maximum PGs size authorized for imputation. PG size is plitted if its size is above this threshold.
#' @param max.pg.size2imp Maximum PG size to impute after splitting. PGs for which size is greater are not imputed. Should be lower than max_pg_size to have effect. 
#' @param ... xxx
#'
#' @return A list containing imputation results for each PG, the execution time, and adjacency matrix between peptides and PGs corresponding to the imputed PGs.
#' @export
#'
#' @examples
#' NULL
#'
impute_block_llk_reset = function(data.pep.rna.crop,
                                  impfunc,
                                  psi,
                                  pep_ab_or = NULL,
                                  prot.idxs = NULL,
                                  df = 1,
                                  nu_factor = 2,
                                  max_pg_size = NULL,
                                  max.pg.size2imp = NULL,
                                  ...) {
  
  adj = data.pep.rna.crop$adj
  if (!is.null(max_pg_size)) {
    adj = split_large_pg(adj, max_pg_size)
  }
  if (is.null(prot.idxs)) {
    prot.idxs = 1:ncol(adj)
  }
  nsamples = nrow(data.pep.rna.crop$peptides_ab)
  logs = list()
  npseudoNA = 0
  begtime = Sys.time()
  if (!is.null(max.pg.size2imp)) {
    pg.idxs = which(colSums(adj) <= max.pg.size2imp)
    adj = adj[, pg.idxs]
    adj_rna_pg = adj_rna_pg[, pg.idxs]
    prot.idxs = 1:ncol(adj)
  }
  n_params = sum(colSums(adj)^2)
  pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                         total = n_params,
                         complete = "=",   # Completion bar character
                         incomplete = "-", # Incomplete bar character
                         current = ">",    # Current bar character
                         clear = FALSE,    # If TRUE, clears the bar when finish
                         width = 100)      # Width of the progress bar
  
  for (i in prot.idxs) {
    # cat("\n##### PROT ", i, "/ ", n_pg, "#####\n")
    idx_cur_pep = which(adj[,i] == 1)
    pb$tick(length(idx_cur_pep)^2)
    cur_ab = matrix(data.pep.rna.crop$peptides_ab[,idx_cur_pep], nrow = nsamples)
    colnames(cur_ab) = colnames(data.pep.rna.crop$peptides_ab)[idx_cur_pep]
    if (is.null(pep_ab_or)) {
      X_gt = NULL
      subpp_ab = as.matrix(cur_ab, nrow = nsamples)
    }
    else {
      cur_ab_gt = matrix(pep_ab_or[,idx_cur_pep], nrow = nsamples)
      subpp_ab = as.matrix(cur_ab, nrow = nsamples)
      X_gt = as.matrix(cur_ab_gt, nrow = nsamples)
    }
    if (sum(is.na(subpp_ab)) == 0 |
        (all(is.na(X_gt) == is.na(subpp_ab)) & !is.null(X_gt))) {
      logs[[i]] = list()
    }
    else {
      if (all(is.na(X_gt) == is.na(subpp_ab))) {
        X_gt = NULL
      }
      n_pep_cur = ncol(subpp_ab)
      K = (nu_factor*df + n_pep_cur - 1) + n_pep_cur + 1
      psimat = psi*diag(n_pep_cur)
      res_imp = impfunc(subpp_ab, true_X = X_gt, K = K, psi = psimat, ...) # + max(colSums(is.na(subpp_ab)))
      ermsg = res_imp$error_msg
      stopifnot(ermsg == "success")
      if ((!is.null(X_gt)) & (ermsg == "success")) {
        npseudos = sum((!is.na(X_gt)) & is.na(subpp_ab))
        npseudoNA = npseudoNA + npseudos
      }
      logs[[i]] = res_imp
    }
  }
  endtime = Sys.time()
  logs[["time"]] = endtime - begtime
  if (group_pep_solo | !is.null(max_pg_size)) {
    logs[["new_adj"]] = adj
  }
  return(logs)
}

# TODO: Rename the function
#' @title Split too large PGs in multi-omic context
#' 
#' @description Split PGs with too many peptides/precursors, and adapts adjacency matrix between mRNA and PGs accordingly. It creates different PGs with
#' size equal to size max (including peptides and mRNAs). Hence, some peptides and mRNA can be duplicated in the new PGs.
#'
#' @param adj Adjacency matrix between peptides and PGs.
#' @param size_max Maximum PG size desired.
#' @param adj_rna_pg Adjacency matrix between mRNA and PGs.
#'
#' @return List containing new adjacency matrix between peptides and PGs, 
#' and new adjacency matrix between mRNA and PGs. 
#' @export
#'
#' @examples
#' NULL
#'
split_large_pg_PG = function(adj, size_max, adj_rna_pg) {
  set.seed(1234)
  idx_pg_too_large = which((colSums(adj) + colSums(adj_rna_pg)) >=
                             size_max)
  if (length(idx_pg_too_large) > 0) {
    new_adj = adj
    new_adj_rna_pg = adj_rna_pg
    nrnas = nrow(adj_rna_pg)
    npeps = nrow(adj)
    adj2bind = matrix(NA, nrow = npeps, ncol = 0)
    adj_rna_pg_2bind = matrix(NA, nrow = nrnas, ncol = 0)
    for (i in 1:length(idx_pg_too_large)) {
      cur_pg_idx = idx_pg_too_large[i]
      cur_pg = adj[, cur_pg_idx]
      idx_pg_pep = which(cur_pg == 1)
      cur_rna_pg = matrix(adj_rna_pg[, cur_pg_idx], ncol = 1)
      colnames(cur_rna_pg) = colnames(adj_rna_pg)[cur_pg_idx]
      n_rna_cur_pg = sum(cur_rna_pg)
      stopifnot("Size max of pg too low when including RNA expression" =
                  n_rna_cur_pg < size_max)
      n_groups = ceiling(sum(cur_pg) / (size_max - n_rna_cur_pg) )
      groups_idx_pg_pep = suppressWarnings(
        split(sample(idx_pg_pep, replace = F), 1:n_groups)
      )
      for (j in 1:length(groups_idx_pg_pep)) {
        new_pg = rep(0, npeps)
        idx2add = setdiff(idx_pg_pep, groups_idx_pg_pep[[j]])
        n2samples = size_max - n_rna_cur_pg - length(groups_idx_pg_pep[[j]])
        idx2add = sample(idx2add, n2samples)
        new_pg[c(groups_idx_pg_pep[[j]], idx2add)] = 1
        adj2bind = cbind(adj2bind, new_pg)
        adj_rna_pg_2bind = cbind(adj_rna_pg_2bind, cur_rna_pg)
      }
    }
    return(list(adj = cbind(new_adj, adj2bind)[, -idx_pg_too_large],
                adj_rna_pg = cbind(new_adj_rna_pg,
                                   adj_rna_pg_2bind)[, -idx_pg_too_large]))
  }
  else {
    return(list(adj = adj, adj_rna_pg = adj_rna_pg))
  }
}

# TODO: Rename this function in code. 
# TODO: Impfunc shoud not be passed as parameter, by we should directly call "estimate params & impute.
# TODO: Some parameters are not used anymore in pipeline_ll_imp file, need to remove tham
#' @title Impute each PG.
#' @description Imputes each PG separately accounting for transcriptomic dataset and returns the results for each PG. 
#'
#' @param data.pep.rna.crop A list representing dataset, with mRNA normalized counts and mRNA/PGs adjacecy table.
#' @param impfunc Imputation function
#' @param psi Inverse scale parameter for IW prior of peptides abundances
#' @param psi_rna Inverse scale parameter for IW prior of mRNA abundances
#' @param rna.cond.mask Vector of size equal to the number of samples in mRNA abundance table, 
#' containing indices of conditions of each sample.
#' @param pep.cond.mask Vector of size equal to the number of samples in peptide abundance table, 
#' containing indices of conditions of each sample.
#' @param pep_ab_or In case we impute a dataset with pseudo-MVS, we can provide the ground truth abundance table, 
#' such that imputation will by done only for pseudo-MVs. This will accelerate imputation algorithm.
#' @param prot.idxs Vector of indices of PGs to impute
#' @param df Estimate degree of freedom of the IG distribution fitted on observed variance.
#' @param nu_factor Multiplication factor on degree of freedom. 2 by default.
#' @param max_pg_size Maximum PGs size authorized for imputation. PG size is plitted if its size is above this threshold.
#' @param max.pg.size2imp Maximum PG size to impute after splitting. PGs for which size is greater are not imputed. Should be lower than max_pg_size to have effect. 
#' @param ... 
#'
#' @return A list containing imputation results for each PG, the execution time, and adjacency matrix between peptides and PGs corresponding to the imputed PGs.
#' @export
#' @import reticulate
#'
impute_block_llk_reset_PG = function(data.pep.rna.crop,
                                     impfunc,
                                     psi,
                                     psi_rna,
                                     rna.cond.mask,
                                     pep.cond.mask,
                                     pep_ab_or = NULL,
                                     prot.idxs = NULL,
                                     df = 2,
                                     nu_factor = 1,
                                     max_pg_size = NULL,
                                     max.pg.size2imp = NULL
                                     ...) {

  if (!is.null(max_pg_size)) {
    adjs = split_large_pg_PG(data.pep.rna.crop$adj, max_pg_size,
                             data.pep.rna.crop$adj_rna_pg)
    adj = adjs$adj
    adj_rna_pg = adjs$adj_rna_pg
  }
  if (is.null(prot.idxs)) {
    prot.idxs = 1:ncol(adj)
  }
  niter = length(prot.idxs)
  nsamples = nrow(data.pep.rna.crop$peptides_ab)
  logs = list()
  begtime = Sys.time()
  n_pg = length(prot.idxs)
  n_cond = length(unique(rna.cond.mask))
  rnas_ab = matrix(NA, length(pep.cond.mask), nrow(adj_rna_pg))
  colnames(rnas_ab) = colnames(data.pep.rna.crop$rnas_ab)
  
  for (i in unique(rna.cond.mask)) {
    nrep_rna = sum(rna.cond.mask == i)
    nrep_pep = sum(pep.cond.mask == i)
    rnas_means = colMeans(matrix(
      data.pep.rna.crop$rnas_ab[rna.cond.mask == i, ,drop = F], nrep_rna))
    # rnas_sds = apply(matrix(
    #   data.pep.rna.crop$rnas_ab[rna.cond.mask == i, ], nrep_rna), 2, sd, na.rm = T)
    # rnas_sds[is.na(rnas_sds)] = 0
    # rnas_ab[pep.cond.mask == i, ] = matrix(rnorm(
    #   nrow(adj_rna_pg) * nrep_pep, rnas_means, rnas_sds), nrep_pep, byrow = T) # This line enables to sample from statistics of each condition instead of setting the mean.
    rnas_ab[pep.cond.mask == i, ] = matrix(rep(rnas_means, nrep_pep), nrep_pep, byrow = T)
    
  }
  if (!is.null(max.pg.size2imp)) {
    pg.idxs = which(colSums(adj) <= max.pg.size2imp)
    adj = adj[, pg.idxs]
    adj_rna_pg = adj_rna_pg[, pg.idxs]
    prot.idxs = 1:ncol(adj)
  }
  n_params = sum((colSums(adj[, prot.idxs]) + 1)^2)
  pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                         total = n_params,
                         complete = "=",   # Completion bar character
                         incomplete = "-", # Incomplete bar character
                         current = ">",    # Current bar character
                         clear = FALSE,    # If TRUE, clears the bar when finish
                         width = 100)      # Width of the progress bar
  
  for (i in prot.idxs) {
    # cat("\n##### PROT ", i, "/ ", n_pg, "#####\n")
    idx_cur_pep = which(adj[,i] == 1)
    pb$tick((length(idx_cur_pep) + 1)^2)
    idx_cur_rna = which(adj_rna_pg[,i] == 1)
    cur_ab = matrix(data.pep.rna.crop$peptides_ab[,idx_cur_pep], nrow = nsamples)
    colnames(cur_ab) = colnames(data.pep.rna.crop$peptides_ab)[idx_cur_pep]
    cur_ab_rna = matrix(rnas_ab[,idx_cur_rna], nrow = nsamples)
    colnames(cur_ab_rna) = colnames(rnas_ab)[idx_cur_rna]
    subpp_ab = cbind(cur_ab, cur_ab_rna)
    if (is.null(pep_ab_or)) {
      X_gt = NULL
    }
    else {
      cur_ab_gt = matrix(pep_ab_or[,idx_cur_pep], nrow = nsamples)
      X_gt = cbind(cur_ab_gt, cur_ab_rna)
    }
    if (sum(is.na(subpp_ab)) == 0 |
        (all(is.na(X_gt) == is.na(subpp_ab)) & !is.null(X_gt))) {
      logs[[i]] = list()
    }
    else {
      if (all(is.na(X_gt) == is.na(subpp_ab))) {
        X_gt = NULL
      }
      n_pep_cur = ncol(subpp_ab)
      K = (nu_factor*df + n_pep_cur - 1) + n_pep_cur + 1
      psimat = c(rep(psi, ncol(cur_ab)), rep(psi_rna, ncol(cur_ab_rna))) * 
        diag(n_pep_cur)
      res_imp = impfunc(subpp_ab, true_X = NULL, K = K, psi = psimat, ...) # + max(colSums(is.na(subpp_ab)))
      res_imp$Xhat = res_imp$Xhat[, 1:ncol(cur_ab)]
      # res_imp = list(Xhat=matrix(10, nsamples, ncol(cur_ab)), error_msg="success")
      ermsg = res_imp$error_msg
      #print(ermsg)
      stopifnot(ermsg == "success")
      logs[[i]] = res_imp
    }
  }
  endtime = Sys.time()
  logs[["time"]] = endtime - begtime
  if (!is.null(max_pg_size)) {
    logs[["new_adj"]] = adj
  }
  return(logs)
}

# TODO: Change function name
#' @title Impute abundance table from PGs results
#' @description From imputation results in each PG and the associate adjacency peptide/PG matrix,
#' imputes the original abundance table.  .
#'
#' @param logs.blocks List of PGs imputation results, that also contains related peptide/PGs adjacency matrix.
#' @param data.pep.rna List representing the dataset not yet imputed
#' @param idx_blocks Indices of PGs for which imputation results should be integrated
#'
#' @return The original peptide abundance table with imputed values.
#' @export
#'
#' @examples
#' 
#'
impute_from_blocks = function(logs.blocks,
                              data.pep.rna,
                              idx_blocks) {
  if (!is.null(logs.blocks$new_adj)) {
    adj = logs.blocks$new_adj
  }
  else {
    adj = data.pep.rna$adj
  }
  if (is.null(idx_blocks)) {
    idx_blocks = 1:ncol(adj)
  }
  npeps = ncol(data.pep.rna$peptides_ab)
  nsamples = nrow(data.pep.rna$peptides_ab)
  pep.imputed = matrix(0, nsamples, npeps)
  colnames(pep.imputed) = colnames(data.pep.rna$peptides_ab)
  rownames(pep.imputed) = rownames(data.pep.rna$peptides_ab)
  n_imputations = matrix(0, nsamples, npeps)
  for (iblock in idx_blocks) {
    cur.block = logs.blocks[[iblock]]
    if (is.list(cur.block)) {
      idxpeps = which(adj[,iblock] == 1)
      if (!is.null(cur.block$Xhat)) {
        pep.imputed[, idxpeps] = pep.imputed[, idxpeps] + cur.block$Xhat
        n_imputations[, idxpeps] = n_imputations[, idxpeps] + 1
      }
      else { # Case were all observations of peptide group are available
        pep.imputed[, idxpeps] = data.pep.rna$peptides_ab[, idxpeps]
        n_imputations[, idxpeps] = 1
      }
    }
  }
  return(pep.imputed / n_imputations)
}



#' @title Plot 2 histograms
#' @description Plot 2 histograms on the same graph.
#' xxxxx
#'
#' @param d1 vector of values for the first histogram
#' @param d2 vector of values for the first histogram
#' @param name1 Label for first histogram
#' @param name2 Label for 2nd histogram
#' @param titlename Title of figure
#' @param xlab X-axis label
#' @param freq If True, bins heights correspond to raw counts, otherwise bins are normalized.
#'
#' @import grDevices
#'
plot2hists <- function(d1,
                       d2,
                       name1,
                       name2,
                       titlename,
                       xlab = "",
                       freq = T) {
  c1 <- rgb(173,216,230, maxColorValue = 255, alpha = 100, names = "lt.blue")
  c2 <- rgb(255,192,203, maxColorValue = 255, alpha = 100, names = "lt.pink")
  b <- min(c(d1,d2), na.rm = T) - 0.01
  e <- max(c(d1,d2), na.rm = T)*(1 + 0.1)
  ax <- pretty(b:e, n = 50)
  if (freq) {
    hgA <- hist(d1, breaks = ax, plot = FALSE) # Save first histogram data
    hgB <- hist(d2, breaks = ax, plot = FALSE) # Save 2nd histogram data
    plot(hgA, col = c1, main = titlename, freq = freq,
         ylim = c(0, max(c(hgA$counts, hgB$counts))))
    plot(hgB, col = c2, add = TRUE, freq = freq)
  } else {
    hgA <- hist(d1, breaks = ax, plot = FALSE) # Save first histogram data
    hgA$density = hgA$counts/sum(hgA$counts)*100
    hgB <- hist(d2, breaks = ax, plot = FALSE) # Save first histogram data
    hgB$density = hgB$counts/sum(hgB$counts)*100
    plot(hgA, col = c1, main = titlename, xlab = xlab, freq = freq,
         ylim = c(0, max(c(hgA$density, hgB$density))))
    plot(hgB, col = c2, add = TRUE, freq = freq)
  }
  legend("topleft", c(name1, name2), fill = c(c1, c2))
}

# TODO: Change name of this function
#' @title Plot several densities
#' @description Plot empirical densities estimated by gaussian kernel
#'
#' @param list.values xxx
#' @param titlename xxx
#' @param xlabel xxx
#' 
#' @import ggplot2
#'
#' @return xxx
#' @export
#'
#' @examples
#' NULL
#'
ggplot2hist <- function(list.values, 
                        titlename, 
                        xlabel="") {
  data.hist = data.frame(values = unlist(list.values),
                        group = factor(rep(names(list.values),
                                    unlist(lapply(list.values, length)))))
  g <- ggplot(data.hist, aes(x = values, fill = group)) + xlab(xlabel) +
    # geom_histogram(position = "identity", alpha = 0.2) +
    geom_density(alpha=.2) +
    ggtitle(titlename) +
    theme(legend.title=element_blank(),
          # legend.position = c(0.8, 0.9),
          panel.background = element_blank(),
          aspect.ratio = 1,
          legend.key = element_rect(fill = "white"),
          plot.title = element_text(hjust = 0.5))
  print(g)
}
