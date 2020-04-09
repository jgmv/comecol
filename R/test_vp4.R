#' Significance Testing For Variation Partition (4 Vars)
#'
#' Calculates significance of components of an object created with vegan's 'varpart' function, using four explanatory variables.
#' @param vp output from 'varpart' analysis.
#' @details If the response data is a distance matrix, calculates p-values from db-RDA analyses. If it is a table, uses RDA.#'
#' @return A data frame containing the variance explained and p-values per explanatory variable.
#' @keywords community data
#' @export
#' @examples
#' test_vp4()
test_vp4 <- function(vp) {
  # retrieve tables from vp
  y  <- eval(parse(text = vp$call[2]))
  X1 <- as.matrix(eval(parse(text = vp$call[3])))
  X2 <- as.matrix(eval(parse(text = vp$call[4])))
  X3 <- as.matrix(eval(parse(text = vp$call[5])))
  X4 <- as.matrix(eval(parse(text = vp$call[6])))

  # create an output table
  tab <- rbind(vp$part[[1]][1:4], vp$part[[2]][1:4], vp$part[[3]][1:4])
  tab$percVar <- tab[, "Adj.R.square"] * 100
  tab$P <- rep(NA, nrow(tab))
  #vegan::showvarparts(4)
  if(class(y) == "dist") {
    tab[15, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1 + X2 + X3 + X4))$Pr[1]
    tab[1, "P"]   <- vegan::anova.cca(vegan::dbrda(y ~ X1))$Pr[1]
    tab[2, "P"]   <- vegan::anova.cca(vegan::dbrda(y ~ X2))$Pr[1]
    tab[3, "P"]   <- vegan::anova.cca(vegan::dbrda(y ~ X3))$Pr[1]
    tab[4, "P"]   <- vegan::anova.cca(vegan::dbrda(y ~ X3))$Pr[1]
    tab[5, "P"]   <- vegan::anova.cca(vegan::dbrda(y ~ X1 + X2))$Pr[1]
    tab[6, "P"]   <- vegan::anova.cca(vegan::dbrda(y ~ X1 + X3))$Pr[1]
    tab[7, "P"]   <- vegan::anova.cca(vegan::dbrda(y ~ X1 + X4))$Pr[1]
    tab[8, "P"]   <- vegan::anova.cca(vegan::dbrda(y ~ X2 + X3))$Pr[1]
    tab[9, "P"]   <- vegan::anova.cca(vegan::dbrda(y ~ X2 + X4))$Pr[1]
    tab[10, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X3 + X4))$Pr[1]
    tab[11, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1 + X2 + X3))$Pr[1]
    tab[12, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1 + X2 + X4))$Pr[1]
    tab[13, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1 + X3 + X4))$Pr[1]
    tab[14, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X2 + X3 + X4))$Pr[1]
    tab[16, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X1 + Condition(X2) +
      Condition(X3) + Condition(X4)))$Pr[1]
    tab[17, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X2 + Condition(X1) +
      Condition(X3) + Condition(X4)))$Pr[1]
    tab[18, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X3 + Condition(X1) +
      Condition(X2) + Condition(X4)))$Pr[1]
    tab[19, "P"]  <- vegan::anova.cca(vegan::dbrda(y ~ X4 + Condition(X1) +
      Condition(X2) + Condition(X3)))$Pr[1]
    tab[32, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X1 + Condition(X2)))$Pr[1]
    tab[33, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X1 + Condition(X3)))$Pr[1]
    tab[34, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X1 + Condition(X4)))$Pr[1]
    tab[35, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X2 + Condition(X1)))$Pr[1]
    tab[36, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X2 + Condition(X3)))$Pr[1]
    tab[37, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X2 + Condition(X4)))$Pr[1]
    tab[38, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X3 + Condition(X2)))$Pr[1]
    tab[39, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X3 + Condition(X2)))$Pr[1]
    tab[40, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X3 + Condition(X4)))$Pr[1]
    tab[41, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X4 + Condition(X1)))$Pr[1]
    tab[42, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X4 + Condition(X2)))$Pr[1]
    tab[43, "P"] <- vegan::anova.cca(vegan::dbrda(y ~ X4 + Condition(X3)))$Pr[1]
  } else {
    tab[15, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1 + X2 + X3 + X4))$Pr[1]
    tab[1, "P"]   <- vegan::anova.cca(vegan::rda(y ~ X1))$Pr[1]
    tab[2, "P"]   <- vegan::anova.cca(vegan::rda(y ~ X2))$Pr[1]
    tab[3, "P"]   <- vegan::anova.cca(vegan::rda(y ~ X3))$Pr[1]
    tab[4, "P"]   <- vegan::anova.cca(vegan::rda(y ~ X3))$Pr[1]
    tab[5, "P"]   <- vegan::anova.cca(vegan::rda(y ~ X1 + X2))$Pr[1]
    tab[6, "P"]   <- vegan::anova.cca(vegan::rda(y ~ X1 + X3))$Pr[1]
    tab[7, "P"]   <- vegan::anova.cca(vegan::rda(y ~ X1 + X4))$Pr[1]
    tab[8, "P"]   <- vegan::anova.cca(vegan::rda(y ~ X2 + X3))$Pr[1]
    tab[9, "P"]   <- vegan::anova.cca(vegan::rda(y ~ X2 + X4))$Pr[1]
    tab[10, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X3 + X4))$Pr[1]
    tab[11, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1 + X2 + X3))$Pr[1]
    tab[12, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1 + X2 + X4))$Pr[1]
    tab[13, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1 + X3 + X4))$Pr[1]
    tab[14, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X2 + X3 + X4))$Pr[1]
    tab[16, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X1 + Condition(X2) +
      Condition(X3) + Condition(X4)))$Pr[1]
    tab[17, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X2 + Condition(X1) +
      Condition(X3) + Condition(X4)))$Pr[1]
    tab[18, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X3 + Condition(X1) +
      Condition(X2) + Condition(X4)))$Pr[1]
    tab[19, "P"]  <- vegan::anova.cca(vegan::rda(y ~ X4 + Condition(X1) +
      Condition(X2) + Condition(X3)))$Pr[1]
    tab[32, "P"] <- vegan::anova.cca(vegan::rda(y ~ X1 + Condition(X2)))$Pr[1]
    tab[33, "P"] <- vegan::anova.cca(vegan::rda(y ~ X1 + Condition(X3)))$Pr[1]
    tab[34, "P"] <- vegan::anova.cca(vegan::rda(y ~ X1 + Condition(X4)))$Pr[1]
    tab[35, "P"] <- vegan::anova.cca(vegan::rda(y ~ X2 + Condition(X1)))$Pr[1]
    tab[36, "P"] <- vegan::anova.cca(vegan::rda(y ~ X2 + Condition(X3)))$Pr[1]
    tab[37, "P"] <- vegan::anova.cca(vegan::rda(y ~ X2 + Condition(X4)))$Pr[1]
    tab[38, "P"] <- vegan::anova.cca(vegan::rda(y ~ X3 + Condition(X2)))$Pr[1]
    tab[39, "P"] <- vegan::anova.cca(vegan::rda(y ~ X3 + Condition(X2)))$Pr[1]
    tab[40, "P"] <- vegan::anova.cca(vegan::rda(y ~ X3 + Condition(X4)))$Pr[1]
    tab[41, "P"] <- vegan::anova.cca(vegan::rda(y ~ X4 + Condition(X1)))$Pr[1]
    tab[42, "P"] <- vegan::anova.cca(vegan::rda(y ~ X4 + Condition(X2)))$Pr[1]
    tab[43, "P"] <- vegan::anova.cca(vegan::rda(y ~ X4 + Condition(X3)))$Pr[1]
  }
  return(tab)
}

