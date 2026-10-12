library(ivreg)
library(DoubleML)
library(mlr3learners)
library(ranger)

data("SchoolingReturns", package = "ivreg")
head(SchoolingReturns)
SchoolingReturnsPLR <- SchoolingReturns[, c("wage", "education", "experience", "married", "ethnicity", "smsa", "south")]
SchoolingReturnsPLR$wage <- log(SchoolingReturnsPLR$wage)
ols_model <- lm(wage ~ . , data = SchoolingReturnsPLR)
ols_model2 <- lm(wage ~  -1 . , data = SchoolingReturnsPLR)
## die -1 zegt neem geen intercept mee
summary(ols_model)

## Construct data for Double ML 
obj_dml_data_schooling = DoubleMLData$new(SchoolingReturnsPLR,
                                          y_col = "wage",
                                          d_cols = "education",
                                          x_cols = c("experience","married","ethnicity","smsa","south"))
obj_dml_data_schooling$data

learner_l = lrn("regr.ranger",num.trees = 500,min.node.size = 2)
learner_m = lrn("regr.ranger",num.trees = 500,min.node.size = 2)


set.seed(31415)

e <-  DoubleMLPLR$new(
  obj_dml_data_schooling,
  learner_l,
  learner_m,
  n_folds = 5,)
e$fit()
e$summary()
summary(ols_model)

learner_l10 = lrn("regr.ranger",num.trees = 500,min.node.size = 10)
learner_m10 = lrn("regr.ranger",num.trees = 500,min.node.size = 10)

f <-  DoubleMLPLR$new(
  obj_dml_data_schooling,
  learner_l10,
  learner_m10,
  n_folds = 10,)

f$fit()
f$summary()
### 3
SchoolingReturnsPLRIV <- SchoolingReturns[, c("wage", "education", "experience", "married", "ethnicity", "smsa", "south", "nearcollege", "nearcollege2")]

SchoolingReturnsPLRIV$wage <- log(SchoolingReturns$wage)
SchoolingReturnsPLRIV <- na.omit(SchoolingReturnsPLRIV)
class(SchoolingReturnsPLRIV$nearcollege)
SchoolingReturnsPLRIV$nearcollege <- as.numeric(SchoolingReturnsPLRIV$nearcollege)
SchoolingReturnsPLRIV$nearcollege2 <- as.numeric(SchoolingReturnsPLRIV$nearcollege2)
sls <- ivreg(wage ~ education + experience + married + ethnicity + smsa + south | nearcollege + nearcollege2 + experience + married + ethnicity + smsa + south, data =  SchoolingReturnsPLRIV )
## die ziet zelf welke de IV en welke de controls zijn...
summary(sls)



set.seed(31415)
obj_dml_data_schoolingIV = DoubleMLData$new(SchoolingReturnsPLRIV,
                                          y_col = "wage",
                                          d_cols = "education",
                                          z_cols = c("nearcollege", "nearcollege2"),
                                          x_cols = c("experience","married","ethnicity","smsa","south"))
obj_dml_data_schoolingIV$data

learner_l = lrn("regr.ranger",num.trees = 500,min.node.size = 2)
learner_m = lrn("regr.ranger",num.trees = 500,min.node.size = 2)
learner_r = lrn("regr.ranger",num.trees = 500,min.node.size = 2)

f <-  DoubleMLPLIV$new(
  obj_dml_data_schoolingIV,
  learner_m,
  learner_l,
  learner_r, 
  n_folds = 5,)
f$fit()
f$summary()
summary(sls)
e$summary()


### PART 2
library(causalweight)
library(hdm)
data(coupon)
## ATE(x) = g(1,x) - g(0,x), so ATE = E[ ATE(x) ] = E[g(1,x) - g(0,x))
data_2 <- DoubleMLData$new(coupon, 
                           y_col = "dailyspending",
                           d_cols = "coupons",
                           x_cols = c("coupons_preperiod","dailyspending_preperiod","income_bracket","age_range","married", "rented", "family_size"))  
## Conditioneel random, enkel conditioneel random
learner_g = lrn("regr.ranger", num.trees = 500, min.node.size = 5) # dit is voor contineous type
learner_classif_m = lrn("classif.ranger", num.trees = 500, min.node.size = 5) ## Of je coupon of niet gebruikt bineair, dus we gebruiken classifiactie (bineair)

d <- DoubleMLIRM$new(data_2, ml_g = learner_g, ml_m = learner_classif_m, n_folds = 5, score = "ATE") ## Specifieer dat je in de ATE geinteresserd bent 

d$fit()
d$summary()

##PART 3
data(pension)
data_3 <- DoubleMLData$new(pension, 
                           y_col = "net_tfa",
                           d_cols = "p401",
                           z_cols = "e401",
                           x_cols = c("age","db","educ","hown","fsize", "inc", "male", "marr", "pira", "twoearn"))  




