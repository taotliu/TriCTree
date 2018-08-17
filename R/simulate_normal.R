#' A function for generating variables with normal distribution
#'
#' This function generates normal distributed variables
#' @param p Correlation between two variables
#' @param mean A vector indicating the mean value of classification 0
#' @param n Number of datasets to generate, with 20 observations in each dataset
#' @keywords simulate, normal
#' @author Yan Sun, Yanke Zhu, Tao Liu
#' @export
#' @examples
#' dataxx = simulate_normal(p = 0.8, mean = rep(0, 20), n = 40)
simulate_normal <- function(p = 0.8, mean = rep(0, 20), n = 40) {
  a = rep(1, 20)
  pho = matrix(p, nrow = 20, ncol = 20)
  s = matrix(0, nrow = 20, ncol = 20)
  for (i in 1:20) {
    pho[i, i] = 1
    s[i, i] = a[i]
  }
  sigma = s %*% pho %*% s
  A = chol(sigma)
  dataset = list()
  for (i in 1:n) {
    dataset[[i]] = matrix(0, nrow = 20, ncol = 21)
    dataset[[i]][1:10, 1] = 0
    dataset[[i]][11:20, 1] = 1

    for (j in 1:10) {
      #set.seed(i * 8000 + j)
      x = rnorm(20, mean = 0, sd = 1)
      #x=rgamma(20,scale=1.5, shape=2)
      nx = x %*% A + mean
      dataset[[i]][j, 2:21] = nx
    }
    for (j in 11:20) {
      #set.seed(i * 8000 + j)
      x = rnorm(20, mean = 0, sd = 1)
      nx = x %*% A + mean
      for (k in 5:8) {
        nx[, k] = nx[, k] + 0.4
      }
      for (k in 9:12) {
        nx[, k] = nx[, k] + 0.8
      }
      for (k in 13:16) {
        nx[, k] = nx[, k] + 1.2
      }
      for (k in 17:20) {
        nx[, k] = nx[, k] + 1.6
      }

      dataset[[i]][j, 2:21] = nx
    }
  }
  dataset1 = matrix(0, 0, 21)
  for (i in 1:n) {
    dataset1 = rbind(dataset1, dataset[[i]])
  }
  return(dataset1)
}
