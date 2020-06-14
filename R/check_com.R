#' Check Consistency in Community Data
#'
#' Checks for consistency among datasets within a 'com' list. Removes samples and species that are not present in all datasets, and optionally those with zero observations.
#' @param com a community data object.
#' @param remove_zeros whether to remove samples and species with zero observations.   
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' check_com()
check_com <- function(com, remove_zeros = T) {
 
  # drop all zero rows and columns
  if(remove_zeros) {
    zero_cols <- colSums(com$cdm) == 0
    zero_rows <- rowSums(com$cdm) == 0
    com$cdm <- com$cdm[!zero_rows, !zero_cols]
    if(sum(zero_cols) > 0) {
      message(paste0(sum(zero_cols), " taxa with zero abundances removed."))
    }    
    if(sum(zero_rows) > 0) {
      message(paste0(sum(zero_rows), " samples with zero abundances removed."))
    }
  }

  # check missing taxa
  if(is.null(com$seq)) {
    cdm_miss_taxa <- colnames(com$cdm)[!(colnames(com$cdm) %in%
      rownames(com$tax)) | !(colnames(com$cdm) %in% com$phy$tip.label)]    
    tax_miss_taxa <- rownames(com$tax)[!(rownames(com$tax) %in%
      colnames(com$cdm)) | !(rownames(com$tax) %in% com$phy$tip.label)]    
    phy_miss_taxa <- com$phy$tip.label[!(com$phy$tip.label %in%
      colnames(com$cdm)) | !(com$phy$tip.label %in% rownames(com$tax))]

    # combine missing taxa
    missing_taxa <- c(cdm_miss_taxa, tax_miss_taxa, phy_miss_taxa)
    if(length(missing_taxa) > 0) missing_taxa <- unique(missing_taxa)    
  } else {
    cdm_miss_taxa <- colnames(com$cdm)[!(colnames(com$cdm) %in%
      rownames(com$tax)) | !(colnames(com$cdm) %in% names(com$seq)) |
      !(colnames(com$cdm) %in% com$phy$tip.label)]    
    tax_miss_taxa <- rownames(com$tax)[!(rownames(com$tax) %in%
      colnames(com$cdm)) | !(rownames(com$tax) %in% names(com$seq)) |
      !(rownames(com$tax) %in% com$phy$tip.label)]    
    seq_miss_taxa <- names(com$seq)[!(names(com$seq) %in%
      colnames(com$cdm)) | !(names(com$seq) %in% rownames(com$tax)) |
      !(names(com$seq) %in% com$phy$tip.label)]    
    phy_miss_taxa <- com$phy$tip.label[!(com$phy$tip.label %in%
      colnames(com$cdm)) | !(com$phy$tip.label %in% rownames(com$tax)) |
      !(com$phy$tip.label %in% names(com$seq))]
    
    # combine missing taxa
    missing_taxa <- c(cdm_miss_taxa, tax_miss_taxa, seq_miss_taxa,
      phy_miss_taxa)
    if(length(missing_taxa) > 0) missing_taxa <- unique(missing_taxa)    
  }
  
  # check missing samples
  cdm_miss_samples <- rownames(com$cdm)[!(rownames(com$cdm) %in%
    rownames(com$env))]
  env_miss_samples <- rownames(com$env)[!(rownames(com$env) %in%
    rownames(com$cdm))]
  
  # combine missing samples
  missing_samples <- c(cdm_miss_samples, env_miss_samples)
  if(length(missing_samples) > 0) missing_samples <- unique(missing_samples)
  
  # drop missing data
  if(length(missing_taxa) > 0) {
    com$cdm <- com$cdm[, !(colnames(com$cdm) %in% missing_taxa)]
    com$tax <- droplevels(com$tax[!(rownames(com$tax) %in% missing_taxa), ])
    com$seq <- com$seq[!(names(com$seq) %in% missing_taxa)]
    if(!is.null(com$phy)) com$phy <- ape::drop.tip(com$phy, missing_taxa)
    message(paste0(length(missing_taxa),
      " taxa are missing in some objects. Dropping..."))
  }
  
  if(length(missing_samples) > 0) {
    com$cdm <- com$cdm[!(rownames(com$cdm) %in% missing_samples), ]
    com$env <- droplevels(com$env[!(rownames(com$env) %in% missing_samples), ])
    message(paste0(length(missing_samples),
      " samples are missing in some objects. Dropping..."))
  }
  
  # reorder taxa
  com$cdm <- com$cdm[, rownames(com$tax)]
  com$seq <- com$seq[rownames(com$tax)]
  
  # reorder samples
  for(i in 1:length(names(com))) {
    if(all(rownames(com$env) %in% rownames(com[[i]]))) com[[i]] <-
      com[[i]][rownames(com$env), ]
  }  
  return(com) 
 
}


