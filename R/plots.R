#' @title Empirical density of peptide correlations
#' @description Plot empirical densities of correlations between peptides within
#'  PG and at random, estimated by gaussian kernel. Note that only correlations 
#'  between fully observed peptides are considered here.
#'
#' @param pep.data List representing dataset
#' @param titlename Title of the graph displayed
#' @param xlabel Label of x-axis
#' 
#' @import ggplot2
#'
#' @return The ggplot2 graph
#' @export
#'
#' @examples
#' data(subbouyssie)
#' plot_pep_correlations(subbouyssie, 'test')
#'
plot_pep_correlations <- function(pep.data, 
  titlename = NULL, 
  xlabel = "Correlations") {
  allcors = list()
  for (i in 1:ncol(pep.data$adj)) {
    pep.idx = which(pep.data$adj[, i, drop = FALSE] == 1)
    if (length(pep.idx) != 1) {
      pep_abs_pg = pep.data$peptides_ab[, pep.idx]
      cor_pg = cor(pep_abs_pg)
      mask_tri_low = lower.tri(cor_pg)
      allcors[[i]] = cor_pg[mask_tri_low]
    }
  }
  all_cors_PG_vec = unlist(allcors)
  allcors = list()
  for (i in 1:ncol(pep.data$adj)) {
    pep.idx = sample(nrow(pep.data$adj), sum(pep.data$adj[, i, drop = FALSE]))
    if (length(pep.idx) != 1) {
      pep_abs_pg = pep.data$peptides_ab[, pep.idx]
      cor_pg = cor(pep_abs_pg)
      mask_tri_low = lower.tri(cor_pg)
      allcors[[i]] = cor_pg[mask_tri_low]
    }
  }
  all_cors_rand_vec = unlist(allcors)
  data.hist = data.frame(values = c(all_cors_PG_vec, all_cors_rand_vec),
    group = factor(
      c(rep("Within PG", length(all_cors_PG_vec)),
        rep("Random", length(all_cors_rand_vec)))))
  g <- ggplot2::ggplot(data.hist, 
    ggplot2::aes(x = values, fill = group)) + xlab(xlabel) +
    # geom_histogram(position = "identity", alpha = 0.2) +
    geom_density(alpha=.2, na.rm = TRUE) + xlim(c(-1, 1)) +
    theme(legend.title=element_blank(),
      # legend.position = c(0.8, 0.9),
      panel.background = element_blank(),
      aspect.ratio = 1,
      legend.key = element_rect(fill = "white"),
      plot.title = element_text(hjust = 0.5))
  if (!is.null(titlename)) {
    g <- g + ggtitle(titlename)
  }
  g
}
