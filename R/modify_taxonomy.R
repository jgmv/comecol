#' Modify Taxon Names
#'
#' Modifies taxon names in a taxonomy table.
#' @param x taxonomy table.
#' @param term term used for unclassified/unidentified taxa.
#' @details Adds to unclassified/unidentified taxa the lowest taxon name known.
#' @return A data frame with taxonomic categories.
#' @keywords taxonomy
#' @export
#' @examples
#' modify_taxonomy()
modify_taxonomy <- function(x, term = "unclassified") {
  ncat <- ncol(x)
  for(i in rownames(x)) {
    for(j in 2:(ncat - 1)) {
      if(any(grep(term, x[i, j])) &
        any(grep(term, x[i, j - 1], invert = T))) {
        tag <- paste0("unclassified_", x[i, j - 1])
        if(!(tag %in% levels(x[, j]))) {
          levels(x[, j]) <- c(levels(x[, j]), tag)
          x[i, j] <- tag
        } else {
          x[i, j] <- tag
        }
      } else if(any(grep(term, x[i, j])) &
        any(grep(term, x[i, j - 1]))) {
        tag <- as.character(x[i, j - 1])
        if(!(tag %in% levels(x[, j]))) {
          levels(x[, j]) <- c(levels(x[, j]), tag)
          x[i, j] <- tag
        } else {
          x[i, j] <- tag
        }      
      }
    }
    if(any(grep(term, x[i, ncat - 1]))) {
      tag <- as.character(x[i, ncat - 1])
      if(!(tag %in% levels(x[, ncat]))) {
        levels(x[, ncat]) <- c(levels(x[, ncat]), tag)
        x[i, ncat] <- tag
      } else {
        x[i, ncat] <- tag
      }
    } else if(x[i, ncat] == term) {
      tag <- paste0(x[i, ncat - 1], "_sp")
      if(!(tag %in% levels(x[, ncat]))) {
        levels(x[, ncat]) <- c(levels(x[, ncat]), tag)
      }
      x[i, ncat] <- tag
    }
  }
  x <- droplevels(x)
  return(x)
}
