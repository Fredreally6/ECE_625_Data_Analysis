
setwd("C:/Courses/ECE 625 Data Analysis/Project")
library(MASS)
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

set.seed(1)
train = sample(1:nrow(nonland),nrow(nonland)*0.9)
nonland.2016 = nonland[-train,]
change.2016 = nonland$change[-train]


#LDA
lda.fit = lda(change~.-lon-lat-Assessed_value_2016,data=nonland,subset = train)
lda.train = predict(lda.fit, nonland[train,])
table(lda.train$class, nonland$change[train])
#            Decrease Increase
#   Decrease    13878     5049
#   Increase     5940    31216
mean(lda.train$class != nonland$change[train])
# [1] 0.1959417
lda.test = predict(lda.fit, nonland.2016)
table(lda.test$class, change.2016)
#            Decrease Increase
#   Decrease     1533      613
#   Increase      654     3432
mean(lda.test$class != change.2016)
# test error = 0.2033055

# if asseseed_value_2015 not known
lda.fit2 = lda(change~.-lon-lat-Assessed_value_2016-Assessed_value_2015,data=nonland,subset = train)
lda.train2 = predict(lda.fit2, nonland[train,])
table(lda.train2$class, nonland$change[train])
#            Decrease Increase
#   Decrease    13877     5049
#   Increase     5941    31216
mean(lda.train2$class != nonland$change[train])
# [1] 0.1959596
lda.test2 = predict(lda.fit2, nonland.2016)
table(lda.test2$class, change.2016)
#           change.2016
#            Decrease Increase
#   Decrease     1533      613
#   Increase      654     3432
mean(lda.test2$class != change.2016)
# [1] 0.2033055

# ---------------QDA----------------
qda.fit = qda(change~.-lon-lat-neighbourhood-property_age_2015-has_fireplace-site_coverage-has_garage-basement_finished-Assessed_value_2016,data=nonland,subset = train)
qda.train = predict(qda.fit, nonland[train,])
table(qda.train$class, nonland$change[train])
mean(qda.train$class != nonland$change[train])

qda.test = predict(qda.fit, nonland.2016)
table(qda.test$class, change.2016)
#            Decrease Increase
#   Decrease     1582     1478
#   Increase      605     2567
mean(qda.test$class != change.2016)
# [1] 0.3342426

# if asseseed_value_2015 not known
qda.fit2 = qda(change~.-lon-lat-neighbourhood-property_age_2015-has_fireplace-site_coverage-has_garage-basement_finished-Assessed_value_2016-Assessed_value_2015,data=nonland,subset = train)
qda.train2 = predict(qda.fit2, nonland[train,])
table(qda.train2$class, nonland$change[train])
mean(qda.train2$class != nonland$change[train])

qda.test2 = predict(qda.fit2, nonland.2016)
table(lda.test2$class, change.2016)
#            Decrease Increase
#   Decrease     1533      613
#   Increase      654     3432
mean(qda.pred2$class != change.2016)
# [1] 0.3352054

#another QDA
qda.fit3 = qda(change~.-lon-lat-neighbourhood-Assessed_value_2016,data=nonland,subset = train)
qda.train3 = predict(qda.fit3, nonland[train,])
table(qda.train3$class, nonland$change[train])
mean(qda.train3$class != nonland$change[train])

qda.test3 = predict(qda.fit3, nonland.2016)
table(qda.test3$class, change.2016)
mean(qda.test3$class != change.2016)
# [1] 0.3096919

qda.fit4 = qda(change~.-lon-lat-neighbourhood-Assessed_value_2016-Assessed_value_2015,data=nonland,subset = train)
qda.train4 = predict(qda.fit4, nonland[train,])
table(qda.train4$class, nonland$change[train])
mean(qda.train4$class != nonland$change[train])

qda.test4 = predict(qda.fit4, nonland.2016)
table(qda.test4$class, change.2016)
mean(qda.test4$class != change.2016)
# [1] 0.3408216

#-----------------------------------for land -----------------------------------------------------------------
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

set.seed(1)
land.train = sample(1:nrow(land),nrow(land)*0.9)
land.2016 = land[-land.train,]
landchange.2016 = land$change[-land.train]


# -----------------LDA-----------------
landlda.fit = lda(change~.-lon-lat-Assessed_value_2016,data=land,subset = land.train)
landlda.train = predict(landlda.fit, land[land.train,])
table(landlda.train$class, land$change[land.train])
#            Decrease Increase
#   Decrease     2734      226
#   Increase      325     1027
mean(landlda.train$class != land$change[land.train])
# [1] 0.1277829
landlda.test = predict(landlda.fit, land.2016)
table(landlda.test$class, landchange.2016)
mean(landlda.test$class != landchange.2016)
# [1] 0.1458333

# if asseseed_value_2015 not known
landlda.fit2 = lda(change~.-lon-lat-Assessed_value_2016-Assessed_value_2015,data=land,subset = land.train)
landlda.train2 = predict(landlda.fit2, land[land.train,])
table(landlda.train2$class, land$change[land.train])
mean(landlda.train2$class != land$change[land.train])
# [1] 0.127551
landlda.test2 = predict(landlda.fit2, land.2016)
mean(landlda.test2$class != landchange.2016)
# [1] 0.1458333

# -----------------QDA----------------
landqda.fit = qda(change~.-lon-lat-neighbourhood-Assessed_value_2016,data=land,subset = train)
landqda.train = predict(landqda.fit, land[land.train,])
table(landqda.train$class, land$change[land.train])
mean(landqda.train$class != land$change[land.train])
# [1] 0.2460575

landqda.test = predict(landqda.fit, land.2016)
table(landqda.test$class, landchange.2016)
mean(landqda.test$class != landchange.2016)
# [1] 0.2354167

# if asseseed_value_2015 not known
landqda.fit2 = qda(change~.-lon-lat-neighbourhood-Assessed_value_2016-Assessed_value_2015,data=land,subset = train)
landqda.train2 = predict(landqda.fit2, land[land.train,])
table(landqda.train2$class, land$change[land.train])
mean(landqda.train2$class != land$change[land.train])
# [1] 0.2479128
landqda.test2 = predict(landqda.fit2, land.2016)
table(landqda.test2$class, landchange.2016)
mean(landqda.test2$class != landchange.2016)
# [1] 0.23125

# ROC Curves
library(ROCR)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}   
# For Nonland
rocplot(lda.test$posterior[,2],change.2016)
rocplot(lda.test2$posterior[,2],nonland$change[-train],add=T,col="red")
rocplot(qda.test3$posterior[,2],nonland$change[-train],add=T,col="yellow")
rocplot(qda.test4$posterior[,2],nonland$change[-train],add=T,col="pink")
#For land
rocplot(landlda.test$posterior[,2],land$change[-land.train],add=T,col="blue")
rocplot(landlda.test2$posterior[,2],land$change[-land.train],add=T,col="green")
rocplot(landqda.test$posterior[,2],land$change[-land.train],add=T,col="gray")
rocplot(landqda.test2$posterior[,2],land$change[-land.train],add=T,col="orange")
legend(0.58, 0.4, legend=c("LDA_nonland_test1", "LDA_nonland_test2","QDA_nonland_test3","QDA_nonland_test4", "LDA_land_test1", "LDA_land_test2","QDA_land_test","QDA_land_test2"),
       col=c("black", "red","yellow","pink","blue","green","gray","orange"),lty=1,,cex=0.8)

