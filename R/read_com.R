#' Community Data Input
#'
#' Reads community data from files.
#' @param cdm the name of the file with the community data matrix. Rows are samples, and columns are species.
#' @param env the name of the file with a table with environmental data. First column must include sample names as in rows of 'cdm'.
#' @param tax the name of the file with a table with taxonomic information. First column must include species names as in columns of 'cdm'.
#' @param seq the name of the file where sequences for each species in fasta format. Sequence headers must be the species names as in columns of 'cdm'.
#' @param phy the name of the file with a phylogenetic tree in Newick format including the species in 'cdm'. Tip labels must be the species names as in columns of 'cdm'. If no tree is provided, a taxonomic tree can be automatically generated using the information in 'tax', using option 'tree_from_tax = T'.
#' @param file the name of the file where the data will be stored as 'RData' object.
#' @param use_saved read data from 'file' instead of importing them from scratch.
#' @param ... further arguments to be passed to 'read.table'.
#' @details If no phylogenetic tree is provided, the calculation from the taxonomic data provided is performed using the perl script 'taxonomy_to_tree.pl' available at 'https://github.com/mr-y/my_bioinfPerls'.
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' read_com()
read_com <- function(cdm, env, tax, seq = NULL, phy = NULL, file = NULL,
  use_saved = F, ...) {  

  if (use_saved & !file.exists(file)) message("No data, reading objects...")
  if(use_saved & file.exists(file)){    
    com <- readRDS(file)
    message(paste("Reading data from ", file))    
  } else {    
    # read data
    cdm <- read.table(cdm, h = T, row.names = 1, ...)
    env <- read.table(env, h = T, row.names = 1, ...)
    tax <- read.table(tax, h = T, row.names = 1, ...)
    if(!is.null(seq)) seq <- ape::read.dna(seq, format = "fasta")
    if(!is.null(phy)) {
      phy <- ape::read.tree(phy)
    }
    
    # combine data
    com <- list(cdm = cdm, env = env, tax = tax, seq = seq, phy = phy) 
    if(is.null(phy)) {
      #message("No tree loaded. Generating from tax file...")    
      #com$phy <- taxtotree(com$tax)
      message("No tree loaded. Generating random tree...") 
      com$phy <- ape::rtree(ncol(cdm), rooted = F, tip.label = colnames(cdm))
    }
    
    # check data consistency
    com <- check_com(com)
    
    # save data
    if(!is.null(file)) saveRDS(com, file = file)    
  }  
  return(com)  

}

