#' Assigns Fungal Guilds using FungalTraits database
#'
#' Assigns fungal guilds using the FungalTraits database (https://doi.org/10.1007/s13225-020-00466-2), by comparing the genus names.
#' @param x a data frame with taxonomy data.
#' @param secondary whether to assign primary (F) or secondary guilds (T).
#' @param print_missing whether taxa not found in the database should be displayed. 
#' @param glomeromycota_to_AMF whether Glomeromycota are assigned to arbuscular mycorrhizas at the phylum level.
#' @return A vector with the guild assignments.
#' @keywords fungal guild
#' @export
assign_FT_guilds <- function(x, secondary = F, print_missing = F,
  glomeromycota_to_AMF = F) {
  dataset <- system.file("extdata", "fungaltraits.csv",
    package = "comecol")
  ft <- read.csv(dataset, h = T, sep = ",")
  # repeated genera
  #ft[ft$genus %in% ft$genus[duplicated(ft$genus)], ]
  # keep one instance of repeated genera
  ft <- droplevels(ft[!duplicated(ft$genus), ])
  if(print_missing) {
    nf <- unique(x[!(x$genus %in% ft$genus), ])
    if(length(nf) > 0) {
      message("Genera not found:")
      print(nf)
    }    
  }
  if(secondary) {
    result <- ft$secondary_lifestyle[match(x$genus, ft$genus)]
    result[result == ""] <- NA
  } else {
    result <- ft$primary_lifestyle[match(x$genus, ft$genus)]
  }
  if(glomeromycota_to_AMF) {
    if(secondary) {
      result[x$phylum == "Glomeromycota" & is.na(result)] <-
        "root-associated"  
    } else {
      result[x$phylum == "Glomeromycota" & is.na(result)] <-
        "arbuscular_mycorrhizal"
    }
  }
  #result <- droplevels(result)
  return(result)
}
