#' A function  used for prediction in the TriCTree package
#'
#' This function predicts the classification of test data with application of the generated classification tree
#' @param model A general linear model to classify the suspended data after the last iteration
#' @param data2 Test dataset
#' @param param The parameters of the classification tree
#' @param prob Prior proportion of the data with indicating value of 1
#' @keywords TriCTree, predict
#' @author Yan Sun, Yanke Zhu, Tao Liu
#' @export
#' @examples
#' dataxx = simulate_normal(n=40, p=0.8)
#' data = data.frame(dataxx[2*(1:400), ])
#' data2 = data.frame(dataxx[2*(1:400)-1, ])
#' param = TriCTree_tripart(X1~., dataxx1)
#' model=assert(dataxx1)
#' TriCTree_predict(model, data2, param=param, prob = 0.5)
TriCTree_predict <- function(model,data2, param,prob=0.5){
  data2=data.frame(data2)
  data2_y = data2[,1]
  pvector = param[[1]]
  model_list = param[[2]]
  pinterval_matrix = param[[3]]
  rown = nrow(data2)
  coln = ncol(data2)
  for ( i in 1:rown){
    if (data2[i,pvector[1]]>pinterval_matrix[1,2]) data2[i,1]=1
    if (data2[i,pvector[1]]<pinterval_matrix[1,1]) data2[i,1]=0
    else{
      sorted=0
      for (j in 2:nrow(pinterval_matrix)){
        pre=predict(model_list[[j-1]],data.frame(as.matrix(data2[i,])),type='response')
        if (pre >= pinterval_matrix[j,2]) {
          data2[i,1]=1
          sorted=1
          break
        }
        if (pre <=pinterval_matrix[j,1]) {
          data2[i,1]=0
          sorted=1
          break
        }
      }
      if(sorted==0) {
        # data2[i,1]=assert(data,data2[i,],prob=prob)
        test_data = data.frame(data2[i,-1])
        p  = predict(model,newdata=test_data,type='response')
        for (i in 1:length(p)){
          if (p[i]<=prob) data2[i,1]=0
          else data2[i,1]=1
        }
      }
    }

  }
  return(data2[,1])
}
