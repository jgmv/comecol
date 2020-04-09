#' Phylogenetic Diversity Of Communitiy data
#'
#' Calculates phylogenetic diversity measures per sample for a community data object.
#' @param com a community data object.
#' @param null.model null model to use (see ?ses.mpd').
#' @param runs number of randomizations.
#' @param file the name of the file where the data will be stored as 'RData' object.
#' @param use_saved read data from 'file' instead of importing them from scratch.
#' @return A list with community data.
#' @keywords community data
#' @export
#' @examples
#' phylo_diversity_com()
phylo_diversity_com <- function(com, null.model = "taxa.labels", runs = 100,
  file = "phylo_diversity.RData", use_saved = F) {  

  if (use_saved & !file.exists(file)) message("No data, reading objects...")
  if(use_saved & file.exists(file)){    
    phylo_diversity <- readRDS(file)
    message(paste0("Reading data from ", file))    
  } else {
    
    pd <- picante::ses.pd(com$cdm, com$phy, include.root = F, runs = runs)
    mpd <- picante::ses.mpd(com$cdm, cophenetic(com$phy),
      null.model = null.model, abundance.weighted = T, runs = runs)
    mpd$nri <- 1 - mpd$mpd.obs.z
    mntd <- picante::ses.mntd(com$cdm, cophenetic(com$phy),
      null.model = null.model, abundance.weighted = T, runs = runs)
    mntd$nti <- 1 - mntd$mntd.obs.z
    phylo_diversity <- list(pd = pd, mpd = mpd, mntd = mntd)
  
    # save data
    saveRDS(phylo_diversity, file = file)
  }
  
  com$pd <- phylo_diversity$pd
  com$mpd <- phylo_diversity$mpd
  com$mntd <- phylo_diversity$mntd
  
  return(com)
}

