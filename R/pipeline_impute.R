

#' @title xxxx
#' @description Pirat imputation function
#'
#' @param data.pep.rna.mis xxx
#' @param pep.ab.comp xxx
#' @param nu_factor xxx
#' @param cov_ratio xxx
#' @param rna.cond.mask xxx
#' @param pep.cond.mask xxx
#' @param group_pep_solo xxx
#' @param mcar xxx
#' @param protidxs xxx
#' @param max.pg.size2imp xxx
#' @param transpose xxx
#' @param degenerated xxx
#'
#' @import progress
#' @import MASS
#' @import invgamma
#' @import reticulate
#' @import graphics
#' @import stats
#'
#' @return xxx
#' @export
#'
#' @examples
#' data(bouyssie)
#' res.mle <- pipeline_llkimpute(bouyssie)
#' res.mle.transp <- pipeline_llkimpute(bouyssie, transpose = TRUE)
#' res.mle.transp.mcar <- pipeline_llkimpute(bouyssie, transpose = TRUE, mcar = TRUE)
#' 
pipeline_llkimpute = function(data.pep.rna.mis,
                              pep.ab.comp = NULL,
                              nu_factor = 2,
                              cov_ratio = 0,
                              rna.cond.mask = NULL,
                              pep.cond.mask = NULL,
                              group_pep_solo = FALSE,
                              mcar = FALSE,
                              protidxs = NULL,
                              max.pg.size2imp = NULL,
                              transpose = FALSE,
                              degenerated = FALSE) {

  
  #Load_Python_Scripts()
  
  ####### Pas sur qu'il faille le laisser ##############
  npeps = ncol(data.pep.rna.mis$peptides_ab)
  seednum = 543210
  set.seed(seednum)
  ###############################

  set.seed(98765)
  psi_rna = NULL

  print("Remove nested prots x2...")
  idx.emb.prots = get_indexes_embedded_prots(data.pep.rna.mis$adj)
  data.pep.rna.mis = rm_pg_from_idx_merge_pg(data.pep.rna.mis, idx.emb.prots)
  print("data ready to get cooked by CensMLE")

  # Estimate Gamma distrib peptides
  f <- function(x, a, b)
    b^a/gamma(a) * x^(-(a + 1)) * exp(-b/x)

  obs2NApep = data.pep.rna.mis$peptides_ab[ ,colSums(is.na(data.pep.rna.mis$peptides_ab)) <= 0]
      # ceiling(0.05 * nrow(data.pep.rna.mis$peptides_ab))]

  vars = sort(apply(obs2NApep, MARGIN = 2, FUN = var, na.rm = T))
  hist(vars, 30, freq = F, xlab ="Variance completely observed",
       main="Histogram of observed variance and fitted inverse-gamma curve - Peptides")

  resllk = MASS::fitdistr( vars, f, list(a=1, b=0.1) )
  alpha = resllk$estimate[1]
  beta = resllk$estimate[2]

  df = 2 * alpha
  psi = 2 * beta

  print(paste(c("Estimated DF", df)))
  print(paste(c("Estimated psi", psi)))

  curve(invgamma::dinvgamma(x, shape = alpha, rate = beta), add=T, col="red")

  # Estimate Gamma distrib RNA
  if (!is.null(rna.cond.mask)) {
    obs2NArna = data.pep.rna.mis$rnas_ab[
      ,colSums(data.pep.rna.mis$rnas_ab == 0) <= 0]

    vars = sort(apply(obs2NArna, MARGIN = 2, FUN = var, na.rm = T))
    hist(vars, 30, freq = F, xlab ="Variance completely observed",
         main="Histogram of observed variance and fitted inverse-gamma curve - mRNA")

    resllk = fitdistr( vars, f, list(a=1, b=0.1) )
    alpha_rna = resllk$estimate[1]
    beta_rna = resllk$estimate[2]

    curve(invgamma::dinvgamma(x, shape = alpha, rate = beta), add=T, col="red")

    # df_rna = 2 * alpha_rna
    psi_rna = 2 * beta_rna
  }

  # Initial estimates of phi and phi0
  mv_rates = colMeans(is.na(data.pep.rna.mis$peptides_ab))
  mean_abund = colMeans(data.pep.rna.mis$peptides_ab, na.rm = T)
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
       main=paste("Estimation of missingness parameters with k=",kernel_size),
       ylab="log(p_mis)",
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
  print(paste("prop abundances values for which pmis = 1 : ", mean(data.pep.rna.mis$peptides_ab <= -phi0/phi, na.rm = T)))
  print(paste("Nb abundances values for which pmis = 1 : ", sum(data.pep.rna.mis$peptides_ab <= -phi0/phi, na.rm = T)))

  nsamples = nrow(data.pep.rna.mis$peptides_ab)

  if (!is.null(max.pg.size2imp)) {
    pg_good_size = which(colSums(data.pep.rna.mis$adj) <= max.pg.size2imp)
    pep_in_pg_good_size = which(rowSums(data.pep.rna.mis$adj[, pg_good_size]) >= 1)
    protidxs = which(colSums(data.pep.rna.mis$adj[pep_in_pg_good_size, ]) >= 1)
    print(length(protidxs))
  }



  if (is.null(rna.cond.mask)) {
    if (transpose) {
      data.pep.rna.mis$peptides_ab = t(data.pep.rna.mis$peptides_ab)
      data.pep.rna.mis$adj = matrix(1, nrow = ncol(data.pep.rna.mis$peptides_ab), ncol = 1)
      colnames(data.pep.rna.mis$adj) = "Samples"
      rownames(data.pep.rna.mis$adj) = colnames(data.pep.rna.mis$peptides_ab)
      if (!is.null(pep.ab.comp)) {
        pep.ab.comp = t(pep.ab.comp)
      }
    }
    else if (degenerated) {
      npep = nrow(data.pep.rna.mis$adj)
      data.pep.rna.mis$adj = matrix(as.logical(diag(npep)), npep)
    }

    params_imp_blocks = list(tol.na.pep = 100,
                             K_fixed = NULL,
                             df = df,
                             nu_factor = nu_factor,
                             prot.idxs = protidxs,
                             psi = psi,
                             psiratio = cov_ratio,
                             max_pg_size = 30,
                             pep_ab_or = pep.ab.comp)

    params_llkimp = list(phi0 = phi0,
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


    
    res_per_block = do.call(impute_block_llk_reset,
                            c(
                              list(
                                data.pep.rna.mis,
                                reticulate::py$estimate_params_and_impute
                                ),
                              params_imp_blocks,
                              params_llkimp
                              )
                            )

    data.imputed = impute_from_blocks(res_per_block, data.pep.rna.mis, protidxs)

    if (transpose) {
      data.imputed = t(data.imputed)
    }
  }
  else {
    params_imp_blocks = list(df = df,
                             nu_factor = nu_factor,
                             rna.cond.mask = rna.cond.mask,
                             pep.cond.mask = pep.cond.mask,
                             prot.idxs = protidxs,
                             psi = psi,
                             psi_rna = psi_rna,
                             psiratio = cov_ratio,
                             max_pg_size = 30,
                             pep_ab_or = pep.ab.comp)

    params_llkimp = list(phi0 = phi0,
                         phi = phi,
                         eps_chol = 1e-4,
                         eps_phi = 1e-5,
                         tol_obj = 1e-7,
                         tol_grad = 1e-5,
                         tol_param = 1e-3,
                         maxiter = as.integer(5000),
                         lr = 0.5, phi_known = T,
                         max_try = 50,
                         max_ls = 500,
                         eps_sig = 1e-4,
                         nsamples = 1000)

    res_per_block = do.call(impute_block_llk_reset_PG,
                            c(list(data.pep.rna.mis,
                                   py$estimate_params_and_impute),
                              params_imp_blocks,
                              params_llkimp))

    data.imputed = impute_from_blocks(res_per_block, data.pep.rna.mis,
                                      protidxs)

  }
  return(data.imputed)
}
