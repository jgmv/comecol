#' Mixture Model Normalization Of Sequence Reads
#'
#' Normalizes sequence reads in community data matrix with a mixture model using DESeq.
#' @param com a community data object.
#' @param ... further arguments to be passed to 'estimateDispersions'.
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' mm_norm()
mm_norm <- function(com, ...) {
  require(locfit)  
  dds <- DESeq::newCountDataSet(t(com$cdm) + 1, conditions = com$env,
    featureData = Biobase::AnnotatedDataFrame(com$tax))
  dds <- DESeq::estimateSizeFactors(dds)
  dds <- DESeq::estimateDispersions(dds, ...)
  dispcol <- grep("disp\\_", colnames(Biobase::fData(dds)))
  if (any(!is.finite(Biobase::fData(dds)[, dispcol]))) {
    Biobase::fData(dds)[which(!is.finite(Biobase::fData(dds)[, dispcol])),
      dispcol] <- 0
  }
  com$cdm <- t(Biobase::exprs(DESeq::varianceStabilizingTransformation(dds)))
  com$cdm[com$cdm < 0.0] <- 0.0 # transforms to zero negative values
  com <- check_com(com)
  return(com)
  
}

