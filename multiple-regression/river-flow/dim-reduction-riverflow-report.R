# Load libraries
#library(caret)
library(glmnet)
library(pls)

# Read in data
rivers <- read.csv("Rivers.csv")[,-c(75,95,96)]

# EDA
hist(rivers$Metric, xlab="Metric", main="Distribution of River Flow Metric", breaks=15)

# Create x and y matrices
x <- as.matrix(rivers[,-1])
y <- as.matrix(rivers[,1])

###### LASSO REGRESSION ######

# Cross validation for finding the best lambda
set.seed(536)
cv.out <- cv.glmnet(x, y, alpha=1, nfolds=102)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

# Fit the model now using the best value of lambda
lasso.mod <- glmnet(x, y, alpha=1, lambda=bestlam)
coefs <- as.matrix(coef(lasso.mod))

# Extract the nonzero coefficients from the model
#options(scipen = 999)
index <- which(coefs != 0)
nonzero <- as.matrix(coefs[coefs != 0], scientific=F)
rownames(nonzero) <- rownames(coefs)[coefs != 0]
nonzero

# Predictions of all observations using lasso model, calculate R-squared
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x)
mean((lasso.pred-y)^2) # MSE
rsq <- 1 - sum((lasso.pred - y)^2) / sum(( mean(y) - y)^2 )
rsq

# Bootstrap confidence intervals
n <- nrow(rivers)
nsim <- 1000
bootstrap_coef <- matrix(0, nrow = nsim, ncol = 95)
for(i in 1:nsim) {
  obs <- sample(1:n, n, replace = TRUE)
  boot_sample <- rivers[obs, ]
  x <- as.matrix(boot_sample[,-1])
  y <- as.matrix(boot_sample[, 1])
  model <- glmnet(x, y, alpha=1, lambda=bestlam)
  bootstrap_coef[i, ] <- matrix(coef(model), nrow=1)
}
cis <- apply(bootstrap_coef, 2, quantile, c(.025, .975))
ests <- apply(bootstrap_coef, 2, median)
colnames(cis) <- rownames(coef(model))
options(scipen=999)
ci.df <- data.frame(Estimate=ests, Lower=t(cis)[,1], Upper=t(cis)[,2])
ci.df.nonzero <- ci.df[index,]

# Cross validation (leave-one-out)
bias <- numeric(n)
mse <- numeric(n)
#width <- numeric(n)
#cvg <- numeric(n)
for (i in 1:n) {
  mod.cv <- glmnet(x[-i,], y[-i], alpha=1, lambda=bestlam)
  pred.cv <- predict(mod.cv, s=bestlam, newx=x)
  single.pred <- pred.cv[i]
  bias[i] <- single.pred - rivers$Metric[i]
  mse[i] <- (rivers$Metric[i] - single.pred)^2
  #width <- 
}
mean(bias)
sqrt(mean(mse))

####### PRINCIPLE COMPONENT REGRESSION ######
pcr.fit <- pcr(Metric~., data=rivers, scale=TRUE, validation ="CV")s
validationplot(pcr.fit.sub,val.type="MSEP") 
summary(pcr.fit)

# Compute MSE on predictions
pcr.pred <- predict(pcr.fit,x[test,],ncomp=12)
mean((pcr.pred-y.test)^2)

# Fit on the full dataset
pcr.fit=pcr(y~x,scale=TRUE,ncomp=20)
validationplot(pcr.fit, val.type='MSEP')
summary(pcr.fit)
coef(pcr.fit)
dim(coef(pcr.fit))

ind <- numeric(ncol(cis))
for (i in 1:ncol(cis)) {
  if (cis[1,i] > 0 & cis[2,i] > 0) ind[i] <- 1
  if (cis[1,i] < 0 & cis[2,i] < 0) ind[i] <- 1
}

# R squared
pcr.pred <- predict(pcr.fit, x, ncomp=12)[,,1]
mean((pcr.pred-y)^2) # MSE
rsq2 <- 1 - sum((pcr.pred-y)^2) / sum((rep(mean(y),length(y)) - y)^2)
rsq2

# Cross validation (leave-one-out)
bias2 <- numeric(n)
mse2 <- numeric(n)
#width2 <- numeric(n)
#cvg2 <- numeric(n)
for (i in 1:n) {
  mod.cv <- pcr(y[-i]~x[-i,], scale=TRUE, validation ="CV")
  pred.cv <- predict(mod.cv, x, ncomp=12)
  single.pred <- pred.cv[i]
  bias2[i] <- single.pred - rivers$Metric[i] 
  mse2[i] <- (rivers$Metric[i] - single.pred)^2
  #width <- 
}
mean(bias2)
sqrt(mean(mse2))


options(scipen=0)


