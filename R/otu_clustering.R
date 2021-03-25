#' Clusters Species By Sequence Similarity
#'
#' Clusters species in a community data object into Operational Taxonomy Units (OTUs) by sequence similariy.
#' @param com a community data object.
#' @param thr threshold of sequence similarity for OTU clustering. In percentage.
#' @param method method of clustering, either 'cd-hit' or 'blastclust'
#' @param file the name of the file where the data will be stored as 'RData' object.
#' @param use_saved read data from 'file' instead of importing them from scratch.
#' @details The function still depends of an external bash script ('otu_clustering.sh'), whcih makes portability problematic. Future changes will aim at solving this problem. 
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' otu_clustering()
otu_clustering <- function(com, thr = 97, method = "cd-hit",
  folder = "OTU_clustering", cleanup_files = F, file = NULL, use_saved = F) {

  if (use_saved & !file.exists(file)) message("No data, reading objects...")
  if(use_saved & file.exists(file)){    
    com <- readRDS(file)
    message("Reading data from ", file)
    return(com)
    stop()    
  }

  # download necessary scripts for clustering
  download.file(url = "https://github.com/jgmv/bash_seq_analysis/archive/master.zip",
    destfile = ".temp.zip")
  unzip(zipfile = ".temp.zip", exdir = folder)
  unlink(".temp.zip")
  
  # export sequence data
  ape::write.dna(com$seq, file = paste0(folder, "/input.fasta"),
    format = "fasta")
  
  # export taxonomy data, format for use in mothur
  tax <- com$tax[, c("kingdom", "phylum", "class", "order", "family", "genus",
    "species")]
  tax[, 1] <- paste(rownames(tax), tax[, 1], sep = "\t")
  tax[, ncol(tax) + 1] <- rep("", nrow(tax))
  write.table(tax, file = paste0(folder, "/taxonomy.csv"),
    quote = F, sep = ";", row.names = F, col.names = F)
  
  # generate clusters
  if (method == "cd-hit") {
    message("Clustering sequences with CD-HIT...")
    system(paste("for i in $(ls scripts/*.sh); do . $i; done; cd_hit_clustering",
      thr / 100, paste0(folder, "/input.fasta ", folder, "/clusters_tab")))
  } else if (method == "blastclust") {
    message("Clustering sequences with blastClust...")
    system2(command = "blastclust",
      args = c("-i", paste0(folder, "input.fasta"), "-o",
      paste0(folder, "/clusters.txt"), "-p F -b T -S", thr), wait = T,
      stdout = T)
  } else {
    stop("method must be 'cd-hit' or 'blastclust'")
  }
  message("Processing clusters, getting consensus taxonomy and guild information...")
  system2(command = "bash", args = c("code/otu_clustering.sh", wait = T,
    stdout = T))
  
  # prepare OTU data
  clusters <- read.table(paste0(folder, "/clusters_tab.csv"), sep = "\t")
  clusters[, 1] <- toupper(clusters[, 1])
  otus <- droplevels(clusters[!duplicated(clusters[, 1]), ])
  colnames(otus) <- c("OTU", "representative")
  
  # tax
  tax <- read.table(paste0(folder, "/consensus_taxonomy.csv"), h = T,
    sep = "\t", row.names = 1)
  rownames(tax) <- otus$OTU
   
  # seq
  seq <- com$seq[as.character(otus$representative)]
  names(seq) <- otus$OTU
  
  # cdm
  cdm <- com$cdm[, as.character(clusters[, 2])]
  cdm <- t(apply(cdm, 1, function(x) tapply(x, clusters[, 1], sum)))
  
  # combine objects
  result_com <- list(cdm = cdm, env = com$env, tax = tax, seq = seq, phy = NULL)

  # phy
  write.table(cbind(result_com$tax[, c("kingdom", "phylum", "class", "order",
    "family", "genus", "species")], rownames(result_com$tax)),
    file = paste0(folder, "/phylo_input.txt"), quote = F,
    sep = "\t", col.names = F, row.names = F)
  #result_com$phy <- taxtotree(result_com)
  message("No tree loaded. Generating random tree...")
  result_com$phy <- ape::rtree(ncol(result_com$cdm), rooted = F, tip.label = colnames(result_com$cdm))
  
  # check data consistency
  result_com <- check_com(result_com)

  # save data
  if(!is.null(file)) saveRDS(result_com, file = file)    
  return(result_com)
  
  # remove temporary files
  if(cleanup_files) {
    unlink(folder, recursive = T)
  } 
} 

