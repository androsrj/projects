# Libraries
library(MASS)
library(car)
library(bestglm)
library(multcomp)
library(GGally)
library(ggplot2)
library(lmtest)
library(tidyverse)

# Get data
kbb <- read.csv("KBB.csv", stringsAsFactors = TRUE)
kbb$LogMileage <- log(kbb$Mileage)
kbb <- kbb[, c(2:13,1)]
#kbb <- data.frame(NewTrim=as.factor(trim.new), kbb)
#kbb$Price <- log(kbb$Price)
#kbb$Mileage <- log(kbb$Mileage)

# EDA
ggpairs(kbb[,c(1,6:12)])
ggplot(data=kbb, aes(x=Mileage, y=Price)) + geom_point(aes(color=Make)) + geom_smooth(aes(color=Make), se=FALSE, method=lm)
plot(log(kbb$Mileage), log(kbb$Price))
ggplot(data=kbb, aes(x=Type, y=Price, fill=Type)) + geom_boxplot() + 
  xlab("Body Type") + ylab("Resale Price") + ggtitle("Resale Price and Body Type")

# Variable Selection
aic <- bestglm(kbb[,-3:-4], IC="AIC", method="exhaustive")
bic <- bestglm(kbb[,-3:-4], IC="BIC", method="exhaustive")

# Check assumptions of each model
mod.fit <- lm(data=kbb, log(Price)~LogMileage+Type+Model+Cylinder)
mod.int <- lm(data=kbb, log(Price)~LogMileage*Make+Type+Cylinder)

avPlots(mod.fit, ask=FALSE, layout=c(1,1), main='')
hist(stdres(mod.fit), xlab="Standardized Residuals", main="Normality Assumption: Residuals for Model 1")
plot(fitted(mod.fit), stdres(mod.fit), xlab="Fitted Values", ylab="Standardized 
     Residuals", main="Equal Variance Assumption: Model 1") ; abline(h=0)
ks.test(stdres(mod.fit), 'pnorm')
bptest(mod.fit)

avPlots(mod.int, ask=FALSE, layout=c(1,1), main='')
hist(stdres(mod.int), xlab="Standardized Residuals", main="Normality Assumption: Residuals for Model 2")
plot(fitted(mod.int), stdres(mod.int), xlab="Fitted Values", ylab="Standardized 
     Residuals", main="Equal Variance Assumption: Model 2") ; abline(h=0)
ks.test(stdres(mod.int), 'pnorm')
bptest(mod.int)


vif(mod.int)
vif(mod.fit)
anova(lm(data=kbb, log(Price)~LogMileage+Make+Type+Cylinder), mod.int)

df <- data.frame(LogMileage=rep(log(15000), 24), Make=rep(levels(kbb$Make), each=4), 
                 Sound=rep(c(1,0), times=12), Leather=rep(c(0,0,1,1), times=6))
preds <- predict.lm(mod.int, newdata=df, interval='confidence')
head(sort(preds, decreasing=TRUE))
df[which.max(preds),]
preds[which.max(preds)]
data.frame(df, preds)

# Assumptions

# Model 1
avPlots(obj1, ask=FALSE)
hist(stdres(obj1), xlab="Standardized Residuals", main="Distribution of Model Residuals")
plot(fitted(obj1), stdres(obj1), xlab="Fitted Values", ylab="Standardized 
     Residuals", main="Equal Variance Assumption") ; abline(h=0)
ks.test(stdres(obj1), 'pnorm')
bptest(obj1)

# Model 2
avPlots(obj2, ask=FALSE)
hist(stdres(obj2), xlab="Standardized Residuals", main="Distribution of Model Residuals")
plot(fitted(obj2), stdres(obj2), xlab="Fitted Values", ylab="Standardized 
     Residuals", main="Equal Variance Assumption") ; abline(h=0)
ks.test(stdres(obj2), 'pnorm')
bptest(obj2)

# Cross-Validation (leave one out method)
cv <- function(mod, plots=FALSE) {
  n.cv <- nrow(kbb) #Number of CV studies to run
  rpmse <- rep(x=NA, times=n.cv)
  bias <- rep(x=NA, times=n.cv)
  wid <- rep(x=NA, times=n.cv)
  cvg <- rep(x=NA, times=n.cv)
  for(cv in 1:n.cv) {
    ## Select test observations
    test.obs <- cv
    
    ## Split into test and training sets
    test.set <- kbb[test.obs,]
    train.set <- kbb[-test.obs,]
    
    ## Fit a lm() using the training data
    if (mod==1) train.lm <- lm(log(Price)~LogMileage+Type+Model+Cylinder, data=train.set)
    if (mod==2) train.lm <- lm(log(Price)~LogMileage*Make+Type+Cylinder, data=train.set)
    
    ## Generate predictions for the test set
    my.preds <- exp(predict.lm(train.lm, newdata=test.set, interval="prediction"))
    
    ## Calculate bias
    bias[cv] <- mean(my.preds[,'fit']-test.set[['Price']])
    
    ## Calculate RPMSE
    rpmse[cv] <- (test.set[['Price']]-my.preds[,'fit'])^2 %>% mean() %>% sqrt()
    
    ## Calculate Coverage
    cvg[cv] <- ((test.set[['Price']] > my.preds[,'lwr']) & (test.set[['Price']] < my.preds[,'upr'])) %>% mean()
    
    ## Calculate Width
    wid[cv] <- (my.preds[,'upr'] - my.preds[,'lwr']) %>% mean()
    
  }
  
  # Histograms of CV measures
  hist(bias) %>% abline(v=mean(bias), col='red')
  hist(rpmse) %>% abline(v=mean(rpmse), col='red')
  hist(wid) %>% abline(v=mean(wid), col='red')
  hist(cvg) %>% abline(v=mean(cvg), col='red')
  
  # Mean of each CV measure
  return(list(Bias=mean(bias), RMSE=mean(rpmse), Width=mean(wid), Coverage=mean(cvg)))
}

cv(1)
cv(2)
sd(kbb$Price) # Compare to SD

# Prediction
cad <- data.frame(LogMileage=log(17000), Cylinder=6, Model="CTS", Type="Sedan")
prediction <- predict.lm(mod.fit, newdata=cad, interval='prediction')
exp(prediction)

# 15000 miles
miles15 <- data.frame(LogMileage=rep(log(15000),804), Type=kbb$Type, Model=kbb$Model, Cylinder=kbb$Cylinder )
preds15 <- data.frame(kbb, as.data.frame(exp(predict.lm(mod.fit, newdata=miles15, interval='confidence'))))
sortedpreds <- preds15[order(preds15$fit, decreasing=TRUE),]

trim.new <- c( rep("None", 10), rep("CX", 10), rep("CXL", 10), rep("CXS", 10), rep("Custom", 10), 
               rep("Limited", 10), rep("None", 10), rep("Special Ed Ultra", 10), rep("None", 20), 
               rep("DHS", 10), rep("DTS", 10), rep("None", 30), rep("Hardtop", 10), rep("LS", 20), 
               rep("LT", 20), rep("SVM", 20), rep("None", 10), rep("LS", 20), rep("LS Sport", 20), 
               rep("None", 30), rep("LS", 20), rep("LT", 10), rep("None", 30), rep("LS", 10), 
               rep("None", 10), rep("SS", 10), rep("LS MAXX", 10), rep("LS", 10), rep("LT MAXX", 10), 
               rep("LT", 10), rep("MAXX", 10), rep("None", 10), rep("LS", 10), rep("LT", 10), 
               rep("SS", 10), rep("GXP", 10), rep("SE", 10), rep("SLE", 10), rep("GT", 10), 
               rep("None", 10), rep("GT", 10), rep("SE", 10), rep("GT", 10), rep("GTP", 10), 
               rep("None", 30), rep("AWD", 10), rep("GT", 10), rep("None", 10), rep("Linear", 20), 
               rep("Aero", 20), rep("Arc", 40), rep("Linear", 10), rep("Aero", 20), rep("Linear", 4), 
               rep("Quad", 20), rep("None", 30), rep("L300", 10) )