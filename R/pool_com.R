#' Pool Community Data By Factor
#'
#' Pools samples in a community data object by a factor in the 'env' table.
#' @param com a community data object.
#' @param var the name of a column in 'env' to use for pooling.
#' @param fun function to use for pooling. Default is mean.
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' pool_com()
pool_com <- function(com, var, fun = mean) {
  
  if("phy" %in% names(com)) {
    result <- com[c("cdm", "env", "tax", "seq", "phy")]
  } else {
    result <- com[c("cdm", "env", "tax", "seq")]
  }
    
  result$cdm <- apply(com$cdm, 2, function(x) tapply(x, com$env[, var], fun))
  
  # select only first row per var levels
  result$env <- com$env[!duplicated(com$env[, var]), ]
  row_names <- result$env[, var]
  levels(row_names) <- c(levels(row_names), "NA_values")
  row_names[is.na(row_names)] <- "NA_values"
  rownames(result$env) <- row_names
  
  # check data consistency
  result <- check_com(result)
  
  return(result)
  
}

