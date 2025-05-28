library("QFeatures")

data(hlpsms)
hl <- readQFeatures(hlpsms, quantCols = 1:10, name = "psms")
hl
hl[[1]]
hl[["psms"]]
head(assay(hl[["psms"]]))
head(rowData(hl[["psms"]]))

hl <- aggregateFeatures(hl, "psms", "Sequence",
                        name = "peptides", fun = colMeans)
hl
hl[["peptides"]]
hl <- aggregateFeatures(hl, "peptides", "ProteinGroupAccessions",
                        name = "proteins", fun = colMeans)
hl
hl[["proteins"]]

colData(hl)
hl$tag <- c("126", "127N", "127C", "128N", "128C", "129N", "129C",
            "130N", "130C", "131")
colData(hl)

rowDataNames(hl)
rowData(hl)
rbindRowData(hl, i = c("peptides", "proteins"))

dF <- DataFrame(mean = rowSums(assay(hl[["proteins"]])),
                sd = rowSds(assay(hl[["proteins"]])))
rowData(hl) <- List(proteins = dF)
rowData(hl)[["proteins"]]



stat3 <- hl["P42227-2", , ]
stat3
stat3_df <- data.frame(longFormat(stat3))
stat3_df$assay <- factor(stat3_df$assay,
                         levels = c("psms", "peptides", "proteins"))
library("ggplot2")
ggplot(data = stat3_df,
       aes(x = colname,
           y = value,
           group = rowname)) +
    geom_line() + geom_point() +
    facet_grid(~ assay)

stat <- hl[c("P42227-2", "P42225"), , ]
stat
stat_df <- data.frame(longFormat(stat))
stat_df$stat3 <- ifelse(stat_df$rowname %in% stat3_df$rowname,
                        "STAT3", "STAT1")
stat_df$assay <- factor(stat_df$assay,
                        levels = c("psms", "peptides", "proteins"))

ggplot(data = stat_df,
       aes(x = colname,
           y = value,
           group = rowname)) +
    geom_line() + geom_point() +
    facet_grid(stat3 ~ assay)


hl |>
    subsetByFeature("P42227-2")
hl |>
    subsetByFeature(c("P42227-2", "P42225"))
hl |>
    subsetByFeature("P42227-2") |>
    longFormat() |>
    as.data.frame() |>
    ggplot(aes(x = colname,
               y = value,
               group = rowname)) +
    geom_line() +
    facet_grid(~ assay)



mito_filter <- VariableFilter(field = "markers",
                              value = "Mitochondrion",
                              condition = "==")
mito_filter
qval_filter <- VariableFilter(field = "qValue",
                              value = 0.001,
                              condition = "<=")
qval_filter

filterFeatures(hl, mito_filter)
filterFeatures(hl, qval_filter)

filterFeatures(hl, ~ markers == "Mitochondrion")
filterFeatures(hl, ~ qValue <= 0.001)