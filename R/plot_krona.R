#' Plot Krona chart
#'
#' Plots a Krona chart from the taxonomy data in a com object
#' @param com community data set.
#' @param group a column of the env data to group samples.
#' @param outfile name of the output file.
#' @return An html file with a Krona chart.
#' @keywords taxonomy
#' @export

plot_krona <- function(com, group = NULL, outfile = "krona_plot.html") {

  abund <- cbind(abund = colSums(com$cdm), com$tax[, c("kingdom", "phylum",
    "class", "order", "family", "genus", "species")])
  write.table(abund, file = "temp_taxonomy_total.tsv", sep = "\t", quote = F,
    col.names = F, row.names = F)

  if(!is.null(group)) {
    f <- 0
    files <- c()
    for(i in unique(com$env[, group])) {
      f <- f + 1
      com_sub <- comecol::subset_com(com, var = group, val = i)
      abund <- cbind(abund = colSums(com_sub$cdm), com_sub$tax[, c("kingdom",
        "phylum", "class", "order", "family", "genus", "species")])   
      write.table(abund, file = paste0("temp_taxonomy", f, ".tsv"),
        sep = "\t", quote = F, col.names = F, row.names = F)
      files <- c(files, paste0("temp_taxonomy", f, ".tsv,", i))
    }
    system(paste("ktImportText temp_taxonomy_total.tsv,total",
      paste(files, collapse = " ")))
  } else {
    system("ktImportText temp_taxonomy_total.tsv")
  }
  system(paste("mv text.krona.html", outfile))
  system("rm temp_taxonomy*.tsv")

}
