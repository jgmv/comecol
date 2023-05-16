#' Significance Testing For Variation Partition (2 Vars)
#'
#' Calculates significance of components of an object created with vegan's 'varpart' function, using two explanatory variables.
#' @param vp output from 'varpart' analysis.
#' @details If the response data is a distance matrix, calculates p-values from db-RDA analyses. If it is a table, uses RDA.#'
#' @return A data frame containing the variance explained and p-values per explanatory variable.
#' @keywords community data
#' @export
#' @examples
#' test_vp2()
test_vp2 <- function(vp, cdm = NULL) {
  # retrieve tables from vp
  y  <- eval(parse(text = vp$call[2]))
  X1 <- as.matrix(eval(parse(text = vp$call[3])))
  X2 <- as.matrix(eval(parse(text = vp$call[4])))

  # create an output table
  tab <- rbind(vp$part[[2]][1:4], vp$part[[3]][1:4])
  tab$percVar <- tab[, "Adj.R.squared"] * 100
  tab$P <- rep(NA, nrow(tab))

  if(class(y) == "dist") {
    tab[3, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1 + X2))$Pr[1]
    tab[1, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1))$Pr[1]
    tab[2, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X2))$Pr[1]
    tab[4, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1 + Condition(X2)))$Pr[1]
    tab[5, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X2 + Condition(X1)))$Pr[1]
  } else {
    tab[3, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1 + X2))$Pr[1]
    tab[1, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1))$Pr[1]
    tab[2, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X2))$Pr[1]
    tab[4, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1 + Condition(X2)))$Pr[1]
    tab[5, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X2 + Condition(X1)))$Pr[1]
  }
  return(tab)
}

