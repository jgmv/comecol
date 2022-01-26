#' A Phylomatic-like Function
#'
#' Subsets a master phylogenetic tree using a provided list of taxon names, similarly to the 'phylomatic' function at https://phylodiversity.net/.
#' @param tax a vector with taxon names to pick from the tree.
#' @param tree optional, a master tree to be subsetted.
#' @param dataset if no 'tree' is provided, select a built-in dataset: 'fungi' (default) or 'plants'
#' @param keep_closest if a species is not found in the master tree, attempts to select a tip from a species in the same genus.
#' @param sep when keep_closest = T, the separator in the taxon names.
#' @details Differently to phylomatic, the function allows repeated names as input, which will results in repeated tips with zero-length branches.
#'
#' As built in datasets, the function currently provides the phylogenetic tree with fungal genera from Tedersoo et al. (2018) Fungal Divers 1-25 (option 'fungi'), and the tree with plant species from Zanne et al. (2014) Nature 506: 89-92. 
#' @return A phylogenetic tree in ape format.
#' @keywords phylogenetic tree
#' @export
#' @examples
#' x <- c("Agrostis_stolonifer", "Holcus_sp", "Deschampsia_flexuosa", "Calamagrostis_sp")
#' phylomatic_like(x, dataset = "plants")
phylomatic_like <- function(tax, tree = NULL, dataset = "fungi",
  keep_closest = T, sep = "_") {

  if(!is.null(tree)) {
    tree <- tree
  } else {
    if (dataset == "fungi") {
      tree_file <- "Tedersoo_et_al_2018.tre"
    } else if (dataset == "plants") {
      tree_file <- "Zanne_et_al_2014.tre"
    } else if (dataset == "daphne") {
      tree_file <- "DaPhnE_01.tre"
    }
    message(paste("No tree provided. Using", tree_file))
    tree <- ape::read.tree(system.file("extdata", tree_file,
      package = "comecol"))
  }

  # check input string
  tax <- na.omit(tax)
  tax_u <- unique(tax)
  not_found <- as.character(tax_u[!(tolower(tax_u) %in%
    tolower(tree$tip.label))])
  found <- tree$tip.label[tolower(tree$tip.label) %in% tolower(tax_u)]
  if(keep_closest == T) {
    not_found <- cbind(not_found, tolower(vapply(strsplit(not_found, sep),
      '[', 1, FUN.VALUE = character(1))))
    for(i in not_found[, 2]) {
      tree_tip <- grep(paste0("^", i), tree$tip.label)
      if(length(tree_tip) > 0) {
        not_found[not_found[, 2] == i, 2] <- tree$tip.label[tree_tip[1]]
        found <- c(found, tree$tip.label[tree_tip[1]])
      }
    }
  }
  if (any(!(not_found[, 2] %in% found))) {
    message("Dropping taxa not not found in tree:")
    message(paste(not_found[!(not_found[, 2] %in% found), 1], colapse = " "))
  }

  # subset tree
  tree_r <- ape::drop.tip(tree, tree$tip.label[!(tree$tip.label %in% found)])
  tax <- tax[!(tax %in% not_found[, 2])]
  tax <- as.character(tax)
  for(i in 1:length(tax)) {
    if(tax[i] %in% not_found[, 1]) {
      tax[i] <- not_found[not_found[, 1] == tax[i], 2]
    }
  }
  for(i in names(table(tax))) {
    if (table(tax)[i] > 1) {
      for(n in 1:(table(tax)[i] - 1)) {
        node <- grep(tolower(i), tolower(tree_r$tip.label))
        tree_r <- phytools::bind.tip(tree_r, i, edge.length = 0,
          where = node[length(node)])
      }
    }
  }

  # modify output tree tips
  for(i in tax_u[!(tax_u %in% not_found[, 1])]) {
    tree_r$tip.label[tree_r$tip.label == tolower(i)] <- i
  }
  for(i in 1:nrow(not_found)) {
    if (not_found[i, 2] %in% tree_r$tip.label) {
        tree_r$tip.label[tree_r$tip.label == not_found[i, 2]] <- not_found[i, 1]
    }
  }

  # return tree
  return(tree_r)

}
