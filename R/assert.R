#' The function to classify the suspended data in the last layer
#'
#' This function classifies the data unclassified after the last iteration using glm()
#' @param train_data Training dataset used for model generation
#' @keywords TriCTree, assert
#' @author Yan Sun, Yanke Zhu, Tao Liu
#' @export
#' @examples
#' dataxx = simulate_normal(n=40, p=0.8)
#' dataxx1 = data.frame(dataxx[2*(1:400), ])
#' assert(dataxx1)
assert <- function(train_data){
  y = train_data[,1]
  train_data = data.frame(train_data[,-1])
  model = glm(y~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+X21,family=binomial,data=train_data)
  return(model)
}
