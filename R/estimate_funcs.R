#' @title Estimate missingness parameters Gamma
#'
#' @param pep.ab.table The peptide or precrursor abundance matrix, with 
#' molecules in columns and samples in row.
#' @param mcar If TRUE, forces gamma_1 = 0.
#'
#' @return A list of the containing missingness parameters gamma_0 and gamma_1.
#' @export
#'
#' @examples
#' data(subbouyssie)
#' estimate_gamma(subbouyssie$peptides_ab)
#' 
#' @importFrom stats lm
#' 
estimate_gamma <- function(pep.ab.table,
    mcar = FALSE) {
    mv_rates <- colMeans(is.na(pep.ab.table))
    mean_abund <- colMeans(pep.ab.table, na.rm = TRUE)
    mean_abund_sorted <- sort(mean_abund, index.return = TRUE)
    mv_rates_sorted <- mv_rates[mean_abund_sorted$ix]
    kernel_size <- 10
    probs <- rep(0, length(mean_abund) - kernel_size + 1)
    for (i in seq(length(probs))) {
        .indices <- seq(from = i, to = (i + kernel_size - 1), by = 1)
        probs[i] <- mean(mv_rates_sorted[.indices])
    }
    not0 <- probs != 0
    m_ab_sorted <- mean_abund_sorted$x[not0]
    probs <- probs[not0]
    # plot(m_ab_sorted[seq(length(probs))], log(probs),
    #     main = paste("Estimation of missingness parameters with k=",kernel_size), 
    #     ylab = "log(p_mis)", 
    #     xlab = "observed mean")
    res.reg <- stats::lm(log(probs) ~ m_ab_sorted[seq(length(probs))])
    sum.reg.reg <- summary(res.reg)
    print(sum.reg.reg)
    # abline(res.reg, col="red")
    # mylabel <- bquote(italic(R)^2 == .(format(summary(res.reg)$r.squared, 
    #     digits = 3)))
    # text(x = 26, y = -3.2, labels = mylabel)
    
    phi0 <- -res.reg$coef[1]
    phi <- -res.reg$coef[2]
    if ((phi <= 1e-3) | (sum.reg.reg$coefficients[2,4] > 0.001) | mcar) {
        phi <- 0
    }
    sort_mean_abund <- sort(mean_abund)
    
    return(list(gamma_0 = phi0, 
        gamma_1 = phi))
}


#' @title Estimate psi and degrees of freedom
#' 
#' @description Estimate the inverse-gamma parameters from the distribution of 
#' observed peptide variances in an abundance table.
#'
#' @param pep.ab.table The peptide or precursor abundance matrix, with 
#' molecules in columns and samples in row (can contain missing values).
#' 
#' @import MASS
#' @import invgamma
#' @importFrom stats var
#'
#' @return List containing estimated fitted hyperparameters df (degrees of 
#' freedom) and psi (inverse scale).
#' @export
#'
#' @examples
#' data(subbouyssie)
#' obj <- subbouyssie
#' # Keep only fully observed peptides
#' obs2NApep <- obj$peptides_ab[ ,colSums(is.na(obj$peptides_ab)) <= 0] 
#' estimate_psi_df(obs2NApep)
#' 
#' 
estimate_psi_df <- function(pep.ab.table) {
    f <- function(x, a, b){
        b^a/gamma(a) * x^(-(a + 1)) * exp(-b/x)
    }
    
    vars <- sort(apply(pep.ab.table, MARGIN = 2, FUN = stats::var, na.rm = TRUE))
    # hist(vars, 
    #     30, 
    #     freq = FALSE, 
    #     xlab ="Variance completely observed",
    #     main="Histogram of observed variance and fitted inverse-gamma curve")
    resllk <- tryCatch(MASS::fitdistr(vars, f, list(a = 1, b = 0.05)), 
                     error = function(e){
                       mu = mean(vars);
                       sigma_sq = stats::var(vars);
                       return(list("estimate" = c(mu^2 / sigma_sq + 2,
                                                mu * (mu^2 / sigma_sq + 1))))})
    if (is.null(resllk)) {
        return(NULL)
    }
    alpha <- resllk$estimate[1]
    beta <- resllk$estimate[2]
    
    # curve(invgamma::dinvgamma(x, shape = alpha, rate = beta), 
    #     add = TRUE, 
    #     col = "red")
    
    df <- 2 * alpha
    psi <- 2 * beta
    return(list(df = df, psi = psi))
}
