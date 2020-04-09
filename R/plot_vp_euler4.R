#' Plot Euler Diagram From Variation Partition Results (4 Vars)
#'
#' Plots an Euler diagram from an object created with vegan's 'varpart' function, using four explanatory variables.
#' @param vp output from 'varpart' analysis.
#' @param vector with the names of the tables with explanatory variables.
#' @param color vector with colors for the diagram bubbles.
#' @return An Euler plot.
#' @keywords community data
#' @export
#' @examples
#' plot_vp_euler3()
plot_vp_euler4 <- function(vp, names = c("X1", "X2", "X3", "X4"),
  col = c("brown3", "skyblue3", "orange", "chartreuse4")) {
  sec <- vp$part$indfract[-16, 3]
  sec <- ifelse(sec < 0, 0, sec)
  res <- vp$part$indfract[16, 3]
  names(sec) <- c(names,
    paste(names[1], names[2], sep = "&"),
    paste(names[2], names[3], sep = "&"),
    paste(names[1], names[3], sep = "&"),
    paste(names[1], names[4], sep = "&"),
    paste(names[2], names[4], sep = "&"),
    paste(names[3], names[4], sep = "&"),
    paste(names[1], names[2], names[4], sep = "&"),# order correct?
    paste(names[1], names[2], names[3], sep = "&"),
    paste(names[2], names[3], names[4], sep = "&"),
    paste(names[1], names[3], names[4], sep = "&"),
    paste(names[1], names[2], names[3], names[4], sep = "&"))
  vd <- venneuler::venneuler(sec, residuals = res)
  plot(vd, col = col)
  mtext(paste("Residuals = ", round(res, 2), sep = ""), 1, cex = 1)
}

