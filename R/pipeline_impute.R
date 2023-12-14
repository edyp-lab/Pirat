#' @title Estimate missingness parameters Gamma
#'
#' @param pep.ab.table xxx
#' @param mcar xxx
#'
#' @return A list
#' @export
#'
#' @examples
#' data(bouyssie)
#' estimate_gamma(bouyssie$peptides_ab)
#' 
estimate_gamma = function(pep.ab.table,
                          mcar = FALSE) {
  mv_rates = colMeans(is.na(pep.ab.table))
  mean_abund = colMeans(pep.ab.table, na.rm = T)
  mean_abund_sorted = sort(mean_abund, index.return = T)
  mv_rates_sorted = mv_rates[mean_abund_sorted$ix]
  kernel_size = 10
  probs = rep(0, length(mean_abund) - kernel_size + 1)
  for (i in 1:length(probs)) {
    probs[i] = mean(mv_rates_sorted[i:(i + kernel_size - 1)])
  }
  not0 = probs != 0
  m_ab_sorted = mean_abund_sorted$x[not0]
  probs = probs[not0]
  plot(m_ab_sorted[1:length(probs)], log(probs),
       main=paste("Estimation of missingness parameters with k=",kernel_size), ylab="log(p_mis)", 
       xlab="observed mean")
  res.reg = lm(log(probs) ~ m_ab_sorted[1:length(probs)])
  sum.reg.reg = summary(res.reg)
  print(sum.reg.reg)
  abline(res.reg, col="red")
  mylabel = bquote(italic(R)^2 == .(format(summary(res.reg)$r.squared, digits = 3)))
  text(x = 26, y = -3.2, labels = mylabel)
  
  phi0 = -res.reg$coef[1]
  phi = -res.reg$coef[2]
  if ((phi <= 1e-3) | (sum.reg.reg$coefficients[2,4] > 0.001) | mcar) {
    phi = 0
  }
  print(paste("Gamma0 estimated = ", phi0))
  print(paste("Gamma1 estimated = ", phi))
  sort_mean_abund = sort(mean_abund)
  plot(sort_mean_abund, exp(-(phi0 + phi*sort_mean_abund)))
  # print(paste("prop abundances values for which pmis = 1 : ", 
  #             mean(pep.ab.table <= -phi0/phi, na.rm = T)))
  # print(paste("Nb abundances values for which pmis = 1 : ", 
  #             sum(pep.ab.table <= -phi0/phi, na.rm = T)))
  return(list(phi0 = phi0, phi=phi))
}


#' @title Estimate psi and degrees of freedom
#' 
#' @description Estimate the inverse scale factor and degrees of freedom of 
#' the distribution of columns-wise variances of an abundance table
#'
#' @param obs2NApep Peptide abundance matrix (can contain missing values)
#' 
#' @import MASS
#' @import invgamma
#'
#' @return List containing estimated parameters df (degrees of freedom) and 
#' psi (inverse scale)
#' @export
#'
#' @examples
#' data(bouyssie)
#' obj <- bouyssie
#' idx <- get_indexes_embedded_prots(obj$adj)
#' obj <- rm_pg_from_idx_merge_pg(obj, idx)
#' obs2NApep <- obj$peptides_ab[ ,colSums(is.na(obj$peptides_ab)) <= 0]
#' estimate_psi_df(obs2NApep)
#'
#' 
estimate_psi_df = function(obs2NApep) {
  f <- function(x, a, b)
    b^a/gamma(a) * x^(-(a + 1)) * exp(-b/x)
  
  vars = sort(apply(obs2NApep, MARGIN = 2, FUN = var, na.rm = TRUE))
  hist(vars, 
       30, 
       freq = FALSE, 
       xlab ="Variance completely observed",
       main="Histogram of observed variance and fitted inverse-gamma curve")
  resllk <- MASS::fitdistr( vars, f, list(a=1, b=0.1) )
  alpha <- resllk$estimate[1]
  beta <- resllk$estimate[2]
  curve(invgamma::dinvgamma(x, shape = alpha, rate = beta), 
        add = TRUE, 
        col = "red")
  
  df <- 2 * alpha
  psi <- 2 * beta
  return(list(df = df, psi = psi))
}

#' @title Pirat imputation function
#' @description Imputation pipeline of Pirat. First, it creates PGs. Then,
#' it estimates parameters of the penalty term (that amounts to an
#' inverse-Wishart prior). Second, it estimates the missingness mechanism
#' parameters. Finally, it imputes the peptide/precursor-level dataset with desired extension.  
#'
#' @param data.pep.rna.mis A list containing important elements of the dataset to impute. Must contain:
#' **peptides_ab**, the peptide or precursor abundance matrix to impute, with samples in 
#' row and peptides or precursors in column; **adj**, a n_peptide x n_protein 
#' adjacency matrix between peptides and proteins containing 0 and 1, 
#' or TRUE and FALSE. Can contain: **rnas_ab**, the mRNA normalized count matrix, with samples in 
#' row and mRNAs in column; **adj_rna_pg**, a n_mrna x n_protein 
#' adjacency matrix n_mrna and proteins containing 0 and 1, or TRUE and FALSE; 
#' @param pep.ab.comp The pseudo-complete peptide or precursor abundance matrix, with samples in 
#' row and peptides or precursors in column. Useful only in mask-and-impute 
#' experiments, if one wants to impute solely peptides containing pseudo-MVs.
#' @param alpha.factor Factor that multiplies the parameter alpha of the penalty in the
#' original paper. 
#' @param rna.cond.mask Vector of indexes representing conditions of samples of mRNA table, only mandatory
#' if extension == "T". For paired proteomic and transcriptomic tables, should be c(1:n_samples).
#' @param pep.cond.mask Vector of indexes representing conditions of samples of mRNA table, only mandatory
#' if extension == "T". For paired proteomic and transcriptomic tables, should be c(1:n_samples).
#' @param extension If NULL (default), classical Pirat is applied. If "2", only imputes
#' PGs containing at least 2 peptides or precursors, and remaining peptides are left unchanged.
#' If "S", Pirat-S is applied, considering sample-wise correlations only for singleton PGs.
#' It "T", Pirat-T is applied, thus requiring **rnas_ab** and **adj_rna_pg** 
#' in list **data.pep.rna.mis**, as well as non-NULL **rna.cond.mask** and **pep.cond.mask**.
#' Also, the maximum size of PGs for which transcriptomic data can be used is controlled with **max.pg.size.pirat.t**.
#' @param mcar If TRUE, forces gamma_1 = 0, thus no MNAR mechanism is considered.
#' @param degenerated If TRUE, applies Pirat-Degenerated (i.e. its univariate alternative) as described in original paper.
#' Should not be TRUE unless for experimental purposes.
#' @param max.pg.size.pirat.t When extension == "T", the maximum PG size for which transcriptomic information is used for imputation. 
#'
#' @import progress
#' @import MASS
#' @import invgamma
#' @import graphics
#' @import stats
#'
#' @return The imputed **data.pep.rna.mis$peptides_ab** table.
#' @export
#'
#' @examples
#' data(bouyssie)
#' res.mle.transp <- pipeline_llkimpute(bouyssie)
#' 
pipeline_llkimpute = function(data.pep.rna.mis,
                              pep.ab.comp = NULL,
                              alpha.factor = 2,
                              rna.cond.mask = NULL,
                              pep.cond.mask = NULL,
                              extension = NULL,
                              mcar = FALSE,
                              degenerated = FALSE,
                              max.pg.size.pirat.t = 1) {
  
  set.seed(98765)
  psi_rna = NULL
  
  print("Remove nested prots...")
  idx.emb.prots = get_indexes_embedded_prots(data.pep.rna.mis$adj)
  data.pep.rna.mis = rm_pg_from_idx_merge_pg(data.pep.rna.mis, idx.emb.prots)
  print("Data ready for boarding with Pirat")
  
  # Estimate Gamma distrib peptides
  obs2NApep = data.pep.rna.mis$peptides_ab[
    ,colSums(is.na(data.pep.rna.mis$peptides_ab)) <= 0]
  # ceiling(0.05 * nrow(data.pep.rna.mis$peptides_ab))]
  est.psi.df = estimate_psi_df(obs2NApep)
  df = est.psi.df$df
  psi = est.psi.df$psi
  
  print(paste(c("Estimated DF", df)))
  print(paste(c("Estimated psi", psi)))
  
  # Estimate Gamma distrib RNA
  if (!is.null(rna.cond.mask)) {
    obs2NArna = data.pep.rna.mis$rnas_ab[
      ,colSums(data.pep.rna.mis$rnas_ab == 0) <= 0]
    
    est.psi.df.rna = estimate_psi_df(obs2NArna)
    psi_rna = est.psi.df.rna$psi
  }
  
  # Initial estimates of phi and phi0
  est.phi.phi0 = estimate_gamma(data.pep.rna.mis$peptides_ab, mcar)
  phi = est.phi.phi0$phi
  phi0 = est.phi.phi0$phi0
  nsamples = nrow(data.pep.rna.mis$peptides_ab)
  
  if (is.null(extension) | extension == "2") { # No extension or 2 pep rule
    if (degenerated) { # Degenerated case (only for paper experiments)
      npep = nrow(data.pep.rna.mis$adj)
      data.pep.rna.mis$adj = matrix(as.logical(diag(npep)), npep)
    }
    if (extension == "2") {
      min.pg.size2imp = 2
    }
    else {
      min.pg.size2imp = 1
    }
    res_per_block = impute_block_llk_reset(data.pep.rna.mis,
                                           psi = psi, 
                                           pep_ab_or = pep.ab.comp,
                                           df = df,
                                           nu_factor = alpha.factor,
                                           max_pg_size = 30,
                                           min.pg.size2imp = min.pg.size2imp,
                                           phi0 = phi0, 
                                           phi = phi, 
                                           eps_chol = 1e-4, 
                                           eps_phi = 1e-5, 
                                           tol_obj = 1e-7, 
                                           tol_grad = 1e-5, 
                                           tol_param = 1e-3,
                                           maxiter = as.integer(5000), 
                                           lr = 0.5,
                                           phi_known = T,
                                           max_try = 50, 
                                           max_ls = 500, 
                                           eps_sig = 1e-4, 
                                           nsamples = 1000)
    
    data.imputed = impute_from_blocks(res_per_block, data.pep.rna.mis)
  }
  else if (extension == "S") { # Pirat-S
    res_per_block = impute_block_llk_reset(data.pep.rna.mis,
                                           psi = psi, 
                                           df = df,
                                           pep_ab_or = pep.ab.comp,
                                           nu_factor = alpha.factor, 
                                           max_pg_size = 30, 
                                           min.pg.size2imp = 2,
                                           phi0 = phi0, 
                                           phi = phi, 
                                           eps_chol = 1e-4, 
                                           eps_phi = 1e-5, 
                                           tol_obj = 1e-7, 
                                           tol_grad = 1e-5, 
                                           tol_param = 1e-4,
                                           maxiter = as.integer(5000), 
                                           lr = 0.5, 
                                           phi_known = T,
                                           max_try = 50, 
                                           max_ls = 500, 
                                           eps_sig = 1e-4, 
                                           nsamples = 1000)
    data.imputed = impute_from_blocks(res_per_block, data.pep.rna.mis)
    idx.pgs1 = which(colSums(data.pep.rna.mis$adj) == 1)
    idx.pep.s1 = which(rowSums(data.pep.rna.mis$adj[, idx.pgs1]) >= 1)
    imputed.data.wo.s1 = t(data.imputed[, -idx.pep.s1])
    peps1 = t(data.pep.rna.mis$peptides_ab[, idx.pep.s1])
    cov.imputed = cov(imputed.data.wo.s1)
    mean.imputed = colMeans(imputed.data.wo.s1)
    peps1.imputed = py$impute_from_params(peps1, mean.imputed, cov.imputed, 0, 0)[[1]]
    data.imputed[, idx.pep.s1] = t(peps1.imputed)
    data.imputed = imputed.data
  }
  else if (extension == "T") {
    res_per_block_pirat = impute_block_llk_reset(data.pep.rna.mis,
                                                 psi = psi, 
                                                 df = df,
                                                 nu_factor = alpha.factor,
                                                 max_pg_size = 30, 
                                                 min.pg.size2imp = max.pg.size.pirat.t + 1,
                                                 pep_ab_or = pep.ab.comp,
                                                 phi0 = phi0, 
                                                 phi = phi, 
                                                 eps_chol = 1e-4, 
                                                 eps_phi = 1e-5, 
                                                 tol_obj = 1e-7, 
                                                 tol_grad = 1e-5, 
                                                 tol_param = 1e-4,
                                                 maxiter = as.integer(5000), 
                                                 lr = 0.5, 
                                                 phi_known = T,
                                                 max_try = 50, 
                                                 max_ls = 500, 
                                                 eps_sig = 1e-4, 
                                                 nsamples = 1000)
    data.imputed.pirat = impute_from_blocks(res_per_block_pirat, data.pep.rna.mis)
    
    
    res_per_block_pirat_t <- impute_block_llk_reset_PG(data.pep.rna.mis,
                                                       df = df,
                                                       nu_factor = alpha.factor, 
                                                       rna.cond.mask = rna.cond.mask,
                                                       pep.cond.mask = pep.cond.mask,
                                                       psi = psi, 
                                                       psi_rna = psi_rna,
                                                       max_pg_size = 30,
                                                       pep_ab_or = pep.ab.comp, 
                                                       max.pg.size2imp = max.pg.size.pirat.t, 
                                                       phi0 = phi0, 
                                                       phi = phi, 
                                                       eps_chol = 1e-4, 
                                                       eps_phi = 1e-5, 
                                                       tol_obj = 1e-7,
                                                       tol_grad = 1e-5, 
                                                       tol_param = 1e-4,
                                                       maxiter = as.integer(5000), 
                                                       lr = 0.5, 
                                                       phi_known = T,
                                                       max_try = 50, 
                                                       max_ls = 500, 
                                                       eps_sig = 1e-4, 
                                                       nsamples = 1000)
    data.imputed.pirat.t = impute_from_blocks(res_per_block, data.pep.rna.mis)
    combined <- array(c(m1, m2), dim = c(dim(m1), 2))
    data.imputed = apply(combined, c(1, 2), function(x) mean(x, na.rm = TRUE))
  }
  return(data.imputed)
}