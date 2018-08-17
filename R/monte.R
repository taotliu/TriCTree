#' A monte Carlo method for generating nonnormal variables
#'
#' This function generates nonnormal variables
#' @param nvar Number of variables
#' @param nsub Number of simulated subjects (response vectors)
#' @param cormat The desired correlation matrix
#' @param skewvec A vector of indicator skewness values
#' @param kurtvec A vector of indicator kurtosis values
#' @keywords nonnormal, monte-carlo
#' @author Yan Sun, Yanke Zhu, Tao Liu
#' @export
#' @examples
#' R <- matrix(0.5,20,20)
#' diag(R) <- 1
#' skewvec=rep(3.5,20)
#' kurtvec=rep(20,20)
#' monte(nvar=20, nsub=800,cormat=R,skewvec=skewvec,kurtvec=kurtvec)
monte<-function(nvar, nsub,cormat,skewvec,kurtvec)
{
  seed=rnorm(1)
  call<-match.call()
  mkclusb.prg <- function(nsub, nvar, skewvec, kurtvec, seed, desired.cor, orientation, callnum)
  {
    mkclusc.prg <- function(x, skew = 0, kurt = 0)
    {
      b <- x[1]
      c <- x[2]
      d <- x[3]
      f <- (b^2 + 6 * b * d + 2 * c^2 + 15 * d^2 - 1)
      g <- 2 * c * (b^2 + 24 * b * d + 105 * d^2 + 2) - skew
      h <- 24 * (b * d + c^2 * (1 + b^2 + 28 * b * d) + d^2 * (12 + 48 * b * d + 141 * c^2 + 225 * d^2)) - kurt
      return((f^2 + g^2 + h^2))
    }
    mkclusd.prg <- function(p, r, matr)
    {
      f <- (p * (matr[1, 2] * matr[2, 2] + 3 * matr[1, 2] * matr[2, 4] + 3 * matr[1, 4] * matr[2, 2] + 9 * matr[1, 4] * matr[
        2, 4]) + p^2 * (2 * matr[1, 3] * matr[2, 3]) + p^3 * (6 * matr[1, 4] * matr[2, 4])) - r
      return(f^2)
    }
    bcdvec <- matrix(0, nrow = nvar, ncol = 3)
    for(i in 1:nvar) {
      bcdvec[i,] <- optim(par = c(1.0, .0, .0),fn = mkclusc.prg,method="L-BFGS-B", lower=-2,upper=2, skew = skewvec[i],
                          kurt = kurtvec[i],,control=list(ndeps=rep(1e-7,3)))$par
      bcdvec[i,1]<-bcdvec[i,1]*-1
      bcdvec[i,3]<-bcdvec[i,3]*-1

    }
    avec <-  - bcdvec[, 2]
    constant.mat <- as.matrix(cbind(avec, bcdvec))
    rxx <- desired.cor
    if(orientation == TRUE) {
      rxx <- matrix(c(rep(0, (nvar * nvar))), nrow = nvar, ncol = nvar)
      for(r in 2:nvar) {
        for(col in 1:(r - 1)) {
          rxx[r, col] <- optim(method="L-BFGS-B", par = 0.1, fn = mkclusd.prg,control=list(ndeps=1e-7),lower=-.99,upper=.99,r = desired.cor[r, col],
                               matr = matrix(c(constant.mat[r,  ], constant.mat[col,  ]), nrow = 2, ncol = 4, byrow = TRUE))$par
          rxx[col, r] <- rxx[r, col]
        }
      }
      I <- diag(nvar)
      rxx <- rxx + I
    }
    set.seed(seed)
    within.clus.dev <- matrix(rnorm(nvar * nsub), ncol = nvar, nrow = nsub)
    within.clus.dev <- apply(within.clus.dev, 2, scale)
    floadt <- (chol(rxx))
    X <- within.clus.dev %*% floadt
    one <- rep(1, nsub) # a vector of ones #
    X2 <- X^2
    X3 <- X^3
    for(i in 1:nvar) {
      ytemp <- cbind(one, X[, i], X2[, i], X3[, i])
      w <- matrix(c(constant.mat[i,  ]), ncol = 1)
      X[, i] <- ytemp %*% w
    }
    X <- apply(X, 2, scale) #
    return(X)
  }
  callnum <- 1

  data <- mkclusb.prg(nsub, nvar, skewvec, kurtvec,seed = seed, desired.cor = cormat, orientation =
                        TRUE, callnum = callnum)

  result<-list(data = data, call=call, nsub=nsub, nvar=nvar, cormat=cormat,
               skewvec=skewvec,kurtvec=kurtvec, seed=seed)
  class(result)<-"monte1"
  result

}


