#' Add Host Phylogenetic Data To Community Data
#'
#' Adds host phylogenetic data from a tree to community data object.
#' @param com a community data object.
#' @param tree tree in 'ape' format with phylogenetic relationships between hosts. The tip labels should much values in a column of 'env' containing the host affiliation.
#' @param plant name of the column in 'env' where the host plant names are stored.
#' @param k the number of eigenvectors retained.
#' @details Returns phylogenetic eigenvalues obtained from a Principal Coordinates Analysis calculated from the phylogenetic data.
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' host_data()
host_data <- function(com, tree, plant = "host_species", k = 4) {

  if(!all(com$env[, plant] %in% tree$tip.label) |
     !all(tree$tip.label %in% com$env[, plant])) {
    message("Some samples are not present everywhere. Aborting...")
    break
  }

  tree_dist <- cophenetic(tree)
  tree_pcoa <- cmdscale(tree_dist, k = k, eig = T)
  host_coord <- vegan::scores(tree_pcoa)[as.character(com$env[, plant]), ]
  rownames(host_coord) <- rownames(com$env)
  colnames(host_coord) <- paste0("host_PC", 1:ncol(host_coord))
  com$host <- host_coord  
  return(com) 

}

