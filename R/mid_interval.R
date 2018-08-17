#' This is a hidden function of the TriCTree package
#'
#' This function estimates the upper and lower bounds of the selection criteria
#' @param data1 A subset of data with the indicator value of 1
#' @param data2 A subset of data with the indicator value of 0
#' @param col_i Index of the column to be separated
#' @keywords middle, interval, select
#' @author Yan Sun, Yanke Zhu, Tao Liu
#' @export
#' @examples
#' dataxx = simulate_normal(n=40, p=0.8)
#' data1 = data.frame(dataxx[2 * (1:400), ])
#' data2 = data.frame(dataxx[2 * (1:400)-1, ])
#' mid_interval(data1, data2, col_i = 2)
mid_interval = function(data1, data2, col_i) {
  h1 = c(min(data1[, col_i]), max(data1[, col_i]))
  h2 = c(min(data2[, col_i]), max(data2[, col_i]))
  c1 = max(h1[[1]], h2[[1]])
  c2 = min(h1[[2]], h2[[2]])
  return(c(c1, c2))
}
