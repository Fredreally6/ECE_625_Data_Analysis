setwd("C:/Courses/ECE 625 Data Analysis/Project")

#############################For non_land properties############################
nonland = read.csv('non_land_V2.csv')
nonland = na.omit(nonland)
names(nonland)[1] = 'lon'
nonland$neighbourhood = as.factor(nonland$neighbourhood)
nonland$change = as.factor(nonland$change)
nonland$property_type = as.factor(nonland$property_type)
nonland$valuation_group = as.factor(nonland$valuation_group)
nonland$basement_finished = as.factor(nonland$basement_finished)
nonland$has_garage = as.factor(nonland$has_garage)
nonland$has_fireplace = as.factor(nonland$has_fireplace)
nonland$fully_complete = as.factor(nonland$fully_complete)
nonland$walkout_basement = as.factor(nonland$walkout_basement)
nonland$air_conditioning = as.factor(nonland$air_conditioning)
lapply(nonland,class)
# Logistic regression
set.seed(1)
train = sample(1:nrow(nonland),nrow(nonland)*0.9)
glm.fit=glm(change~.-lon-lat-Assessed_value_2016,data=nonland,subset = train, family="binomial")
options(max.print=999999)
summary(glm.fit)
#coef(glm.fit)
prob.train = predict(glm.fit,type="response")
pred.train = rep("Decrease",length(prob.train))
pred.train[prob.train>0.5] = "Increase"
table(pred.train,nonland$change[train])
train_error = 1-mean(pred.train == nonland$change[train])
train_error # training Error

prob.test = predict(glm.fit,nonland[-train,],type="response")
pred.test = rep("Decrease",length(prob.test))
pred.test[prob.test>0.5] = "Increase"
table(pred.test,nonland$change[-train])
test_error = 1-mean(pred.test == nonland$change[-train])
test_error # test Error

# Assume 2015 assessed value unknown
glm.fit2=glm(change~.-lon-lat-Assessed_value_2016-Assessed_value_2015,data=nonland,subset = train, family="binomial")
#summary(glm.fit2)
#coef(glm.fit2)
prob.train2 = predict(glm.fit2,type="response")
pred.train2 = rep("Decrease",length(prob.train2))
pred.train2[prob.train2>0.5] = "Increase"
table(pred.train2,nonland$change[train])
train_error2 = 1-mean(pred.train2 == nonland$change[train])
train_error2 # training Error

prob.test2 = predict(glm.fit2,nonland[-train,],type="response")
pred.test2 = rep("Decrease",length(prob.test2))
pred.test2[prob.test2>0.5] = "Increase"
table(pred.test2,nonland$change[-train])
test_error2 = 1-mean(pred.test2 == nonland$change[-train])
test_error2 # test Error

##############################For land properties#########################
land = read.csv('land.csv')
land = na.omit(land)
names(land)[1] = 'lon'
land$neighbourhood = as.factor(land$neighbourhood)
land$change = as.factor(land$change)
land$property_type = as.factor(land$property_type)
land$basement_finished = as.factor(land$basement_finished)
land$has_garage = as.factor(land$has_garage)
land$has_fireplace = as.factor(land$has_fireplace)
land$walkout_basement = as.factor(land$walkout_basement)
land$air_conditioning = as.factor(land$air_conditioning)
lapply(land,class)
# Logistic regression
set.seed(1)
train_l = sample(1:nrow(land),nrow(land)*0.9)
glm.fit_land=glm(change~.-lon-lat-Assessed_value_2016,data=land,subset = train_l, family="binomial")
options(max.print=999999)
summary(glm.fit_land)
#coef(glm.fit_land)
prob.train_l = predict(glm.fit_land,type="response")
pred.train_l = rep("Decrease",length(prob.train_l))
pred.train_l[prob.train_l>0.5] = "Increase"
table(pred.train_l,land$change[train_l])
train_error_land = 1-mean(pred.train_l == land$change[train_l])
train_error_land # train_ling Error

prob.test_land = predict(glm.fit_land,land[-train_l,],type="response")
pred.test_land = rep("Decrease",length(prob.test_land))
pred.test_land[prob.test_land>0.5] = "Increase"
table(pred.test_land,land$change[-train_l])
test_error_land = 1-mean(pred.test_land == land$change[-train_l])
test_error_land # test Error

# Assume 2015 assessed value unknown
glm.fit_land2=glm(change~.-lon-lat-Assessed_value_2016-Assessed_value_2015,data=land,subset = train_l, family="binomial")
#summary(glm.fit_land2)
#coef(glm.fit_land2)
prob.train_l2 = predict(glm.fit_land2,type="response")
pred.train_l2 = rep("Decrease",length(prob.train_l2))
pred.train_l2[prob.train_l2>0.5] = "Increase"
table(pred.train_l2,land$change[train_l])
train_error2_land = 1-mean(pred.train_l2 == land$change[train_l])
train_error2_land # train_ling Error

prob.test_land2 = predict(glm.fit_land2,land[-train_l,],type="response")
pred.test_land2 = rep("Decrease",length(prob.test_land2))
pred.test_land2[prob.test_land2>0.5] = "Increase"
table(pred.test_land2,land$change[-train_l])
test_error2_land = 1-mean(pred.test_land2 == land$change[-train_l])
test_error2_land # test Error

# ROC Curves
install.packages('ROCR')
library(ROCR)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}   
# For Nonland
rocplot(prob.test,nonland$change[-train])
rocplot(prob.test2,nonland$change[-train],add=T,col="red")
#For land
rocplot(prob.test_land,land$change[-train_l],add=T,col="blue")
rocplot(prob.test_land2,land$change[-train_l],add=T,col="green")
legend(0.58, 0.2, legend=c("Logistic Regression_nonland_test1", "Logistic Regression_nonland_test2", "Logistic Regression_land_test1", "Logistic Regression_land_test2"),
       col=c("black", "red","green","blue"),lty=1,,cex=0.8)

       