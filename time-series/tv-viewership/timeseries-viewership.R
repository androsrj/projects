# Libraries
library(ggplot2)
library(forecast)
library(tidyverse)

# Get data, make LogViewers variable
tv <- read.csv("viewership.csv", sep=',', header=TRUE)
tv$LogViewers <- log(tv$Viewers)

# Exploratory graphic(s)
ggplot(data=tv, aes(x=ShowNum, y=LogViewers)) + geom_point() + geom_smooth(se=FALSE) + ggtitle("Log of Viewership Over Time")
(r <- cor(tv$ShowNum, tv$LogViewers))
r^2

# Initial LM and explore correlation in residuals
summary(tv.lm <- lm(data=tv, LogViewers~ShowNum))
ACF.resid <- acf(resid(tv.lm), lag.max=25)
ACF.resid.dframe <- data.frame(Lag=ACF.resid$lag, ACF=ACF.resid$acf)
ggplot(data=ACF.resid.dframe, aes(x=Lag, y=ACF)) + geom_col() + ggtitle("ACF of Temporal Correlation in Residuals")

# TS object and ARIMA mode fit
tv.ts <- ts(data=tv$LogViewers, start=c(1,1), frequency=10)
x <- model.matrix(LogViewers~-1+ShowNum, data=tv)
auto.arima(tv.ts, max.p=2, max.q=2, max.P=1, max.Q=1, d=0, D=1, ic="aic", stepwise=FALSE, xreg=x)
my.sarima.model <- Arima(tv.ts, order=c(2,0,0), seasonal=c(0,1,1), xreg=x)

# Show that residuals have been decorrelated
ACF.decorr <- acf(resid(my.sarima.model), lag.max=25)
ACF.dframe.decorr <- data.frame(Lag=ACF.decorr$lag, ACF=ACF.decorr$acf)
ggplot(data=ACF.dframe.decorr, aes(x=Lag, y=ACF)) + geom_col() + ggtitle("ACF of Decorrelated Residuals")

# Validate assumptions
ggplot()+geom_histogram(mapping=aes(x=resid(my.sarima.model))) + xlab("Standardized Residuals") + ylab("Frequency") + ggtitle("Normality Assumption")
ggplot(mapping=aes(y=resid(my.sarima.model), x=fitted(my.sarima.model))) + geom_point() + xlab("Fitted Values") + ylab("Std. Residuals") + ggtitle("Equal Variance Assumption")
ggplot(data=tv, aes(x=ShowNum, y=LogViewers)) + geom_point() + geom_smooth(se=FALSE) + geom_smooth(se=FALSE, method=lm, col='red') + ggtitle("Linearity Assumption")

# Cross-validation
test.ts <- tv[61:70,]
train.ts <- tv[1:60,]
ts.cv <- ts(data=train.ts$LogViewers, start=c(1,1), frequency=10)
test.x <- x[61:70,]
train.x <- x[1:60,]
#auto.arima(ts.cv, max.p=2, max.q=2, max.P=2, max.Q=2, d=0, D=1, ic="aic", stepwise=FALSE,xreg=train.x)
sarima.cv <- Arima(ts.cv, order=c(2,0,0), seasonal=c(0,1,1), xreg=train.x)
preds <- forecast(sarima.cv, h=10, xreg=test.x, level=.95)
rpmse <- (test.ts[['LogViewers']]-preds$mean)^2 %>% mean() %>% sqrt()
cvg <- ((test.ts[['LogViewers']] > preds[['lower']]) & (test.ts[['LogViewers']] < preds[['upper']])) %>% mean()
rpmse
cvg

# Plot of CV vs. observations
a <- rep(NA, 60)
plot.prep <- data.frame(tv$ShowNum, c(a, preds[['mean']]), c(a, preds[['lower']]), c(a, preds[['upper']]))
colnames(plot.prep) <- c("ShowNum", "Mean", "Lower", "Upper")
ggplot(data=tv, aes(x=ShowNum, y=LogViewers)) + geom_point() + geom_point(data=plot.prep, aes(x=ShowNum, y=Mean), col='red') + geom_line(data=plot.prep, aes(x=ShowNum, y=Lower), col='orange') + geom_line(data=plot.prep, aes(x=ShowNum, y=Upper), col='orange') + ggtitle("Predictions and Observations in Cross-Validation")

# Hypothesis testing
deg.f <- nrow(tv) - 4
se <- (vcov(my.sarima.model) %>% diag() %>% sqrt())[[4]]
1-pt(coef(my.sarima.model)[[4]] / se, deg.f)
coef(my.sarima.model)[[4]] + qt(.975, deg.f) * c(-1,1) * se

# Predictions 
ShowNum <- c(71:80)
x.prep <- data.frame(ShowNum)
x.preds <- model.matrix(~-1+ShowNum, data=x.prep)
preds.season <- forecast(my.sarima.model, h=10, xreg=x.preds, level=.95)
preds.season

# Plot predictions
b <- rep(NA, 70)
plot.prep2 <- data.frame(c(tv$ShowNum, 71:80), c(b, preds.season[['mean']]), c(b, preds.season[['lower']]), c(b, preds.season[['upper']]))
colnames(plot.prep2) <- c("ShowNum", "Mean", "Lower", "Upper")
ggplot(data=tv, aes(x=ShowNum, y=LogViewers)) + geom_point() + geom_point(data=plot.prep2, aes(x=ShowNum, y=Mean), col='red') + geom_line(data=plot.prep2, aes(x=ShowNum, y=Lower), col='orange') + geom_line(data=plot.prep2, aes(x=ShowNum, y=Upper), col='orange') + ggtitle("Predictions for log(Viewers) in Season 8")

