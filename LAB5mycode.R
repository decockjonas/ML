### LAB SESSION 5 
setwd("")
data = read.csv2("data_did.csv")
source("function to be used.R")
library(did)
library(mlr3learners)
library(ranger)
library(DoubleML)
## Estimate the ATT 
## Train the random forest 
learner_g0 = lrn("regr.ranger", num.trees = 1000 , min.node.size = 4) 
learner_m0 = lrn("classif.ranger", num.trees = 1000, min.node.size = 4)

estimation_method_randomforest  = doubleml_did(learner_g = learner_g0, learner_m = learner_m0)

set.seed(31415)

## Get the ATT for each group cohort (here only 1 treated cohort, not staggered)


data$G <- as.numeric(data$G)

did_randomforest <- att_gt(yname = "lemp", tname = "year", idname = "id", gname = "G", xformla = ~ region + lemp.0 + lpop.0 +  lavg_pay.0, 
                           est_method = estimation_method_randomforest, allow_unbalanced_panel = FALSE, data = data )
summary(did_randomforest)


## LASSO as a learner 

learner_g0_lasso = lrn("regr.cv_glmnet", s = "lambda.min") 
learner_m0_lasso = lrn("classif.cv_glmnet", s = "lambda.min")

estimation_method_lasso = doubleml_did(learner_g = learner_g0_lasso, learner_m = learner_m0_lasso)
set.seed(31415)
did_lasso = att_gt(yname = "lemp", tname = "year", idname = "id", gname = "G", xformla = ~ region + lemp.0 + lpop.0 +  lavg_pay.0, 
                   est_method = estimation_method_lasso, allow_unbalanced_panel = FALSE, data = data )

summary(did_lasso)
## The ATT is -0.0332 and is significant  with LASSO as a learner for DID but when we use random forst  -0.0082    and a se of 0.0164 so not significant
## But magintude is rather small.