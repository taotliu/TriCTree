# TriCTree

An R package for implementing the "Trichotomous Classification Tree (TriCTree)" algorithm. 

Authors: Yan Sun [cre,aut], Yanke Zhu [aut] and Tao Liu [cre,aut] (tliu@stat.brown.edu) 

Reference: Zhu Y and Fang J (2016). Logistic regression-based trichotomous classification tree and its application in medical diagnosis. *Med Decis Making*, 36(8):973-89. doi: 10.1177/0272989X15618658. 

## Installation

The latest version of the `TriCTree` package is available at GitHub [taotliu/TriCTree](https://github.com/taotliu/TriCTree). It requires the `devtools` package to be installed in R. If you do not have devtools in your R program, use the code `install.packages("devtools")` to install the devtools package first. Then run the following codes to install the `TriCTree` package.

```R

install.packages("devtools")
library(devtools)
devtools::install_github("taotliu/TriCTree")
library(TriCTree)
```

## Example

The following `R` code example demonstrates the use of the `TriCTree` package.

### Generate a simulation with normal distribution 

In the simulate_normal() function you can generate a dataset with 20 variables with a specific mean vector(mean) and correlation matrix(p between each two variables). The default dataset includes 800 rows and can be changed by settting the number of datasets n, 20 observations in each detaset. 

The default mean vector of variables labeled 0 is set 0, and the default mean vector of data with label 1 is assumed as: `(0,0,0,0,0.4,0.4,0.4,0.4,0.8,0.8,0.8,0.8,1.2,1.2,1.2,1.2,1.6,1.6,1.6,1.6)`.

```R
 > dat = simulate_normal(p = 0.8, mean = rep(0, 20), n = 20)
```
The output `dat` is a 800x21 matrix, the first colomn indicating the label of observation(0 or 1). 400 observations of 0 and 400 of 1 are generated.

### Generate a simulation with nonnormal distribution

In the simulate_nonnormal() function you can generate a distribution with specific skewness(skewvec) and kurtosis(kurtvec).and the default correlation(0.5) can be changed. The output format is 'matrix'.

```R
 > simulate_nonnormal(p=0.5, skewvec=rep(3.5,20), kurtvec=rep(20,20))
```

The output `dat` is a 800x21 matrix, the first colomn indicating the label of observation(0 or 1).400 observations of 0 and 400 of 1 are generated.

### Generate the classification tree using the simulated data

We use LRTCT method to generate a classification tree. The default minimum separate number of observations in each iteation is set as 5, and the default teminate condition is that suspended data is less than 20. The input formula helps to choose the variables that can be utilized as classification criterion.

```R
 > data = simulate_normal(n=40, p = 0.8)
 > data1 = data.frame(data[2*(1:400), ])
 > param = TriCTree_tripart(X1~.,data1)
```

The param is a `lrtct`type list, containing the information of the classification result:

```R
 > param[[1]]
 [1] "X20" "X4"  "X5"  "X18"
```

This is the index of variables selected as criterion.

```R
 > param[[2]]
 [1] 
 
Call:  glm(formula = y ~ ., family = binomial, data = candidate_data)

Coefficients:
(Intercept)          X20           X4  
     -4.314        5.215       -4.447  

```

where `param[[2]]` is a list of the linear models. For detailed explanation, use `summary()`.

```R
 > param[[3]]
            [,1]      [,2]
[1,] -0.79020859 2.0126159
[2,]  0.03459975 0.9695618
[3,]  0.07811542 0.9090882
[4,]  0.20952837 0.9057503
```

This is the upper and lower bound of selection criteria. For detailed explanation, use summary().


### Summarize the result of classification

Te summary() function exhibits the results of classification and explains the meaning of each parameter.

```R
 > summary(param)
Classification variable 1 is X20 
 if X20 < -0.7902086 ,classification=0 
 if X20 > 2.012616 ,classification=1 
 else it enters the next iteration
 
```

This is a paragragh to demonstrate how to manipulate the  classification.

### Predict the classification of the test dataset

TriCTree_predict() function is used to predict the classification of the observations in test dataset. 
The model function is the output of assert() function to classify the suspended data in the last layer, and p indicates the prior probability of (type==`0`), 0.5 by default.

```R
 > dat = simulate_normal(p = 0.8, mean = rep(0, 20), n = 20)
 > data1 = data.frame(dat[2*(1:400), ])
 > data2 = data.frame(dat[2*(1:400)-1,])
 > param = TriCTree_tripart(X1~.,data1)
 > model = assert(data1)
 > result = TriCTree_predict(model,data2,param)
```
Result is an array predicting the classification of each observation in the test dataset.

### Making an asserted prediction of data

While there is a possibility that some data in the test dataset remain left after the classification, an assertion of the data type is made. It is not recommended to do so in practice, because these data need to be scrutinized. However the classification can still be accomplished when necessary.

```R
 > dat = simulate_normal(p = 0.8, mean = rep(0, 20), n = 20)
 > model=assert(dat)
```

The output of assert() function is a generalized linear model for complimantary classification.

## Contact

Tao Liu, PhD
tliu@stat.brown.edu














