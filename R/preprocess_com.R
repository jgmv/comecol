#' Preprocess Community Data
#'
#' Performs preprocessing tasks in community data, removing samples and species with few observations, and normalizing abundance data.
#' @param com a community data object.
#' @param min_obs remove samples with a total number of observations below this treshold.
#' @param min_taxa remove species with a total number of observations below this treshold.
#' @param normalize logical, whether to normalize abundances using a mixture model with DESeq.
#' @param method arguments to be passed to 'estimateDispersions'.
#' @param sharingMode arguments to be passed to 'estimateDispersions'.
#' @param fitType arguments to be passed to 'estimateDispersions'.
#' @param ... further arguments to be passed to 'estimateDispersions'.
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' preprocess_com.com()
preprocess_com <- function(com, min_obs = 100, min_taxa = 5, normalize = T,
  method = "blind", sharingMode = "maximum", fitType = "local", ...) {
  
  drop_cols <- colSums(com$cdm) < min_taxa
  drop_rows <- rowSums(com$cdm) <= min_obs
  com$cdm <- com$cdm[!drop_rows, !drop_cols]
  com <- check_com(com)
  
  # mixture model normalization with DSEq
  if(normalize) {
    message("Normalizing with DSEq...")
    com <- mm_norm(com, method = method,
      sharingMode = sharingMode, fitType = fitType, ...)  
  }
  return(com)

}

