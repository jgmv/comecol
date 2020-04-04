#' Taxonomy Table To Tree
#'
#' Generates a tree from the taxonomy table in a 'com' object.
#' @param com a community data object.
#' @param taxfile the name of the file to be generated containing the taxonomy table.
#' @param treefile the name of the file to be generated containing the taxonomy tree in Newick format.
#' @details The tree calculated from the taxonomic data provided is performed using the perl script 'taxonomy_to_tree.pl' available at 'https://github.com/mr-y/my_bioinfPerls'.
#'
#' Currently, the function relies on bash scripts called with 'system', hence it is not very portable. Further versions may include a better R integration. 
#' @return A phylogenetic tree in 'ape' format.
#' @keywords community data
#' @export
#' @examples
#' taxtotree()
taxtotree <- function(com, taxfile = "tax.txt", treefile = "tax.tre") {

  if(file.exists("scripts/taxonomyToTree.sh")){    
    message("taxonomyToTree.sh found")    
  } else {
      system2(command = "git", args = c("clone",
        "https://github.com/jgmv/bash_seq_analysis.git", "temp"))
      if (dir.exists("scripts")) {
        file.rename("temp/taxonomyToTree.sh", "scripts/taxonomyToTree.sh") 
      } else {
        dir.create("scripts", recursive = T)
        file.rename("temp/taxonomyToTree.sh", "scripts/taxonomyToTree.sh") 
      }
    unlink("temp", recursive = T)
  }
  write.table(cbind(com$tax[, c("kingdom", "phylum", "class", "order",
    "family", "genus", "species")], rownames(com$tax)),
    file = taxfile, quote = F, sep = "\t", col.names = F, row.names = F)
  system(paste(". scripts/taxonomyToTree.sh; taxonomyToTree", taxfile, treefile))
  phy <- ape::read.tree(treefile)
  return(phy)

}

