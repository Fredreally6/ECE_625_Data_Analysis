setwd("C:/Courses/ECE 625 Data Analysis/Project")

house_data=read.csv("non_land_v2.csv", stringsAsFactors = TRUE)
house_data$property_age_2015=as.numeric(house_data$property_age_2015)
house_data$building_count=as.numeric(house_data$building_count)
house_data$site_coverage=as.character(house_data$site_coverage)
house_data$site_coverage=as.numeric(house_data$site_coverage)
house_data = na.omit(house_data)

#plot(house_data)

fit = glm(Assessed_value_2015~., data=house_data)
summary(fit)
fit.lm = lm(Assessed_value_2015~., data=house_data)
summary(fit.lm)
par(mfrow=c(2,2))
plot(fit.lm)
# From the diagnostic plots we can see that there appear to be a few outliers
# which we can remove to improve our fit and there is also a few high leverage
# points that can be taken out.
plot(hatvalues(fit.lm))

# Since best subset selection is computationally difficult to achieve, we will
# use both ridge regression and the lasso method to select the optimal
# multiple linear regression model.

# The lasso
library(glmnet)
x = model.matrix(Assessed_value_2015~., data = house_data)[, c(-18,-19)]
y = house_data$Assessed_value_2015
train = sample(1:nrow(x), nrow(x)*0.9)
test = (-train)
x.train = x[train,]
y.train = y[train]
x.test = x[test, ]
y.test = y[test]
grid = 10^seq(10, -2, length=1000)
lasso.mod = glmnet(x[train, ], y[train], alpha=1, lambda=grid)
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha=1, lambda=grid)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)
plot(y.test, lasso.pred)

# We can create a plot of the CV errors vs lambda values
plot(cv.out$lambda, cv.out$cvm, main="CV error vs lambda values",
     type="l", xlim=c(0, 5e+5), ylim=c(8e+11,2e+12))
which.min(cv.out$cvm)
points(cv.out$lambda[which.min(cv.out$cvm)],
       cv.out$cvm[which.min(cv.out$cvm)],col="red")

# From the plotted CV error estimates, we can see that lasso picks a lambda
# value of 