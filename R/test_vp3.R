#' Significance Testing For Variation Partition (3 Vars)
#'
#' Calculates significance of components of an object created with vegan's 'varpart' function, using three explanatory variables.
#' @param vp output from 'varpart' analysis.
#' @details If the response data is a distance matrix, calculates p-values from db-RDA analyses. If it is a table, uses RDA.#'
#' @return A data frame containing the variance explained and p-values per explanatory variable.
#' @keywords community data
#' @export
#' @examples
#' test_vp3()
test_vp3 <- function(vp, cdm = NULL) {
  # retrieve tables from vp
  y  <- eval(parse(text = vp$call[2]))
  X1 <- as.matrix(eval(parse(text = vp$call[3])))
  X2 <- as.matrix(eval(parse(text = vp$call[4])))
  X3 <- as.matrix(eval(parse(text = vp$call[5])))

  # create an output table
  tab <- rbind(vp$part[[1]][1:4], vp$part[[2]][1:4], vp$part[[3]][1:4])
  tab$percVar <- tab[, "Adj.R.square"] * 100
  tab$P <- rep(NA, nrow(tab))
  #vegan::showvarparts(3)

  if(class(y) == "dist") {
    tab[7, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1 + X2 + X3))$Pr[1]
    tab[1, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1))$Pr[1]
    tab[2, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X2))$Pr[1]
    tab[3, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X3))$Pr[1]
    tab[4, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1 + X2))$Pr[1]
    tab[5, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1 + X3))$Pr[1]
    tab[6, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X2 + X3))$Pr[1]
    tab[8, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1 + Condition(X2) +
      Condition(X3)))$Pr[1]
    tab[9, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X2 + Condition(X1) +
      Condition(X3)))$Pr[1]
    tab[10, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X3 + Condition(X1) +
      Condition(X2)))$Pr[1]
    tab[16, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X1 + Condition(X3)))$Pr[1]
    tab[17, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X1 + Condition(X2)))$Pr[1]
    tab[18, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X2 + Condition(X3)))$Pr[1]
    tab[19, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X2 + Condition(X1)))$Pr[1]
    tab[20, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X3 + Condition(X1)))$Pr[1]
    tab[21, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X3 + Condition(X2)))$Pr[1]
  } else {
    tab[7, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1 + X2 + X3))$Pr[1]
    tab[1, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1))$Pr[1]
    tab[2, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X2))$Pr[1]
    tab[3, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X3))$Pr[1]
    tab[4, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1 + X2))$Pr[1]
    tab[5, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1 + X3))$Pr[1]
    tab[6, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X2 + X3))$Pr[1]
    tab[8, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1 + Condition(X2) + Condition(X3)))$Pr[1]
    tab[9, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X2 + Condition(X1) + Condition(X3)))$Pr[1]
    tab[10, "P"] <- vegan::anova.cca(vegan::rda(y ~ X3 + Condition(X1) + Condition(X2)))$Pr[1]
    tab[16, "P"] <- vegan::anova.cca(vegan::rda(y ~ X1 + Condition(X3)))$Pr[1]
    tab[17, "P"] <- vegan::anova.cca(vegan::rda(y ~ X1 + Condition(X2)))$Pr[1]
    tab[18, "P"] <- vegan::anova.cca(vegan::rda(y ~ X2 + Condition(X3)))$Pr[1]
    tab[19, "P"] <- vegan::anova.cca(vegan::rda(y ~ X2 + Condition(X1)))$Pr[1]
    tab[20, "P"] <- vegan::anova.cca(vegan::rda(y ~ X3 + Condition(X1)))$Pr[1]
    tab[21, "P"] <- vegan::anova.cca(vegan::rda(y ~ X3 + Condition(X2)))$Pr[1]
  }
  return(tab)
}

