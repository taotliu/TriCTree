#' The main function of the TriCTree package
#'
#' This function estimates the optimal trichotomous classification tree
#' @param formula A formula of format Y ~ X1 + X2 + ..., where Y needs to be binary
#' values 0 or 1, and X1, X2, ... are predictor names in the data
#' @param data A training dataset for estimating the trichotomous classification tree
#' @param minleft Threshold for making a split on the remaining data to be classified; default minleft = 20
#' @param minsplit Threshold for making a split on the suspended data, default minsplit = 5
#' @keywords TriCTree, tripart
#' @author Yan Sun, Yanke Zhu, Tao Liu
#' @export
#' @examples
#' dataxx = simulate_normal(n=40, p=0.8)
#' dataxx1 = data.frame(dataxx[2*(1:400), ])
#' param = TriCTree_tripart(X1~., dataxx1)
TriCTree_tripart <-
  function (formula = X1 ~ .,
            data,
            minleft = 20,
            minsplit = 5) {

    data = model.frame(as.formula(formula), data = data.frame(data))
    rown = nrow(data)
    coln = ncol(data)
    names = names(data)

    pvector = c()
    pinterval_matrix = matrix(0, 0, 2)
    p = 0
    name_list = c()
    model_list = list()
    data1 = subset(data, data[, 1] == 0)
    data2 = subset(data, data[, 1] == 1)

    #record the numbers of rows of three types
    nrow_matrix = matrix(0, nrow = coln, ncol = 3)
    #record the intervals of classification
    interval_matrix = matrix(0, nrow = coln, ncol = 2)
    for (i in 2:coln) {
      interval_set = mid_interval(data1, data2, i)
      interval_matrix[i, 1] = interval_set[1]
      interval_matrix[i, 2] = interval_set[2]
      lsdata1 = subset(data, data[, i] < interval_matrix[i, 1])
      lsdata2 = subset(data,
                       data[, i] >= interval_matrix[i, 1] &
                         data[, i] <= interval_matrix[i, 2])
      lsdata3 = subset(data, data[, i] > interval_matrix[i, 2])
      nrow_matrix[i, 1] = nrow(lsdata1)
      nrow_matrix[i, 2] = nrow(lsdata2)
      nrow_matrix[i, 3] = nrow(lsdata3)
    }
    #seek for optimal classification
    p = which.max(nrow_matrix[, 1] + nrow_matrix[, 3])
    name_list = c(name_list, names[p])
    pvector = c(pvector, p)
    pinterval_matrix = rbind(pinterval_matrix, interval_matrix[p, ])
    data = subset(data, data[, p] >= interval_matrix[p, 1] &
                    data[, p] <= interval_matrix[p, 2])

    for (i in 1:100) {
      if (nrow(data) > minleft) {
        select_data = data[, pvector]
        newdata = data
        rown = nrow(data)    #### Yan: double check this; do we need to redefine this and the following
        coln = ncol(data)
        names = names(data)
        step = list()
        model2_list = list()
        for (i in 2:coln) {
          if (i %in% pvector == FALSE) {
            y = data[, 1]
            candidate_data = cbind(select_data, data[, i])
            candidate_data = data.frame(candidate_data)
            names(candidate_data) = c(names(data)[pvector], names(data[i]))
            model = glm(y ~ ., family = binomial, data = candidate_data)
            step[[i]] = step(model, direction = "both")
            model2_list[[i]] = list(model)
            y.pre = predict(model, data = candidate_data, type = 'response')
            newdata[, i] = y.pre

          }
          else
            next
        }
        newdata[, pvector] = 0
        nrow_matrix = matrix(0, nrow = coln, ncol = 3)
        interval_matrix = matrix(0, nrow = coln, ncol = 2)
        newdata1 = subset(newdata, newdata[, 1] == 0)
        newdata2 = subset(newdata, newdata[, 1] == 1)
        for (i in 2:coln) {
          interval_set = mid_interval(newdata1, newdata2, i)
          interval_matrix[i, 1] = interval_set[1]
          interval_matrix[i, 2] = interval_set[2]
          lsdata1 = subset(newdata, newdata[, i] < interval_matrix[i, 1])
          lsdata2 = subset(newdata,
                           newdata[, i] >= interval_matrix[i, 1] &
                             newdata[, i] <= interval_matrix[i, 2])
          lsdata3 = subset(newdata, newdata[, i] > interval_matrix[i, 2])
          nrow_matrix[i, 1] = nrow(lsdata1)
          nrow_matrix[i, 2] = nrow(lsdata2)
          nrow_matrix[i, 3] = nrow(lsdata3)
        }
        p = which.max(nrow_matrix[, 1] + nrow_matrix[, 3])
        name_list = c(name_list, names[p])
        pvector = c(pvector, p)
        pinterval_matrix = rbind(pinterval_matrix, interval_matrix[p, ])
        coef = step[[p]]$coefficients
        nameset = names(step[[p]]$model)[-1]
        model_list = append(model_list, model2_list[[p]])
        data = cbind(data, newdata[, p])
        sub1 = subset(data, data[coln + 1] < interval_matrix[p, 1])
        sub2 = subset(data, data[coln + 1] >= interval_matrix[p, 1] &
                        data[coln + 1] <= interval_matrix[p, 2])
        sub3 = subset(data, data[coln + 1] > interval_matrix[p, 2])
        sub1 = sub1[, 1:coln]
        sub2 = sub2[, 1:coln]
        sub3 = sub3[, 1:coln]
        n1 = nrow(sub1)
        n2 = nrow(sub2)
        n3 = nrow(sub3)
        n11 = sum(sub1[, 1])
        n10 = n1 - n11
        n21 = sum(sub2[, 1])
        n20 = n2 - n21
        n31 = sum(sub3[, 1])
        n30 = n3 - n31

      }
      if (nrow(data) > minleft &
          nrow(sub1) + nrow(sub3) >= minsplit)
        data = sub2
      else {
        data = data
        break
      }
    }
    modelfit = list(name_list, model_list, pinterval_matrix)
    class(modelfit) <- 'trictree'
    return(modelfit)
  }
