#' Subsets Community Data
#'
#' Subsets samples in a community data object.
#' @param com a community data object.
#' @param sel logical expression indicating elements or rows to keep.
#' @param var if 'sel' is not provided, the name of column in 'env' to be used for selection.
#' @param var if 'sel' is not provided, the name of the values in 'var' to keep.
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' subset_com()
subset_com <- function(com, sel, var = NULL, val = NULL) {  
  if(missing(sel)) {
    sel <- com$env[, var] == val  
  }
  sel[is.na(sel)] <- FALSE
  com$cdm <- com$cdm[sel, ]
  com <- check_com(com)
  return(com)  
}
