#' A summary function of the classification tree
#'
#' This function generates nonnormal variables
#' @param param Parameters of classification tree
#' @keywords summary, parameter
#' @author Yan Sun, Yanke Zhu, Tao Liu
#' @export
#' @examples
#' dataxx = simulate_normal(n=40, p=0.8)
#' dataxx1 = data.frame(dataxx[2*(1:400), ])
#' param = TriCTree_tripart(X1~., dataxx1)
#' summary_lrtct(param)
summary.trictree <- function(param){

  cat('Classification variable 1 is',param[[1]][1],
      '\n if',param[[1]][1],'<',param[[3]][1,1],',classification=0',
      '\n if',param[[1]][1],'>',param[[3]][1,2],',classification=1',
      '\n else it enters the next iteration \n \n')
  for (i in 2:length(param[[1]])){
    cat('Classification variable',i,'is','\n',names(param[[2]][[i-1]]$coefficients),'\n',param[[2]][[i-1]]$coefficients,
        '\n if model <',param[[3]][1,1],',classification=0',
        '\n if model >',param[[3]][1,2],',classification=1',
        '\n else it enters the next iteration \n \n')
  }
}

