#' A monte Carlo method for generating nonnormal
#'
#' This function generates nonnormal variables as training and testing dataset
#' @param p correlation between each two variables
#' @param skewvec A vector of indicator skewness values
#' @param kurtvec A vector of indicator kurtosis values
#' @keywords nonnormal, monte-carlo
#' @author Yan Sun, Yanke Zhu, Tao Liu
#' @export
#' @examples
#' simulate_nonnormal(p=0.5, skewvec=rep(3.5,20), kurtvec=rep(20,20))
simulate_nonnormal<- function(p=0.5, skewvec=rep(3.5,20), kurtvec=rep(20,20)){
  R <-matrix(p,20,20)
  diag(R) <- 1
  dat=monte(nvar=20, nsub=800,cormat=R,skewvec=skewvec,kurtvec=kurtvec)
  dataxx1=data.frame(cbind(0,dat$data[1:400,]))
  dataxx2=data.frame(cbind(1,dat$data[1:400,]))
  for (i in 5:8) dataxx2[,i]=dataxx2[,i]+0.4
  for (i in 9:12) dataxx2[,i]=dataxx2[,i]+0.8
  for (i in 13:16) dataxx2[,i]=dataxx2[,i]+1.2
  for (i in 17:20) dataxx2[,i]=dataxx2[,i]+1.6
  dataxx=rbind(dataxx1,dataxx2)
  return(dataxx)
}
