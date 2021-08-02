#' Taxonomy Table To Tree
#'
#' Generates a tree from the taxonomy table in a 'com' object.
#' @param tax a taxonomy data frame.
#' @param a first column to be included.
#' @param b last column to be included.
#' @param include_rownames include row names as tip labels
#' @return A phylogenetic tree in 'ape' format.
#' @keywords community data
#' @export
#' @examples
#' taxtotree()
taxtotree <- function(tax, a = 1, b = 7, include_rownames = T) {
  
  message("Building phylogenetic tree...")
  tax <- tax[, a:b]
  #string <- tidyr::unite(cbind(tax, rownames(tax)), pathString, sep = "/")
  if(include_rownames) {
    string <- apply(cbind(tax, rownames(tax)), 1,
      function(x) paste(x, collapse = "/"))
  } else {
    string <- apply(tax, 1, function(x) paste(x, collapse = "/"))
  }
  string <- as.data.frame(string)
  colnames(string) <- "pathString"
  phy <- ape::as.phylo(data.tree::as.Node(string))
  message("Done!")
  return(phy)

}

