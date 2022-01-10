##
## Approaches for the titanic data
##

###################
## Preliminaries ##
###################

## Libraries that I need
library(tidyverse)

## Read in the data and merge into single data frame
titanic.train <- read.csv(file = "titanic-train.csv", stringsAsFactors = FALSE)
titanic.test <- read.csv(file="titanic-test.csv", stringsAsFactors = FALSE)
titanic <- bind_rows(titanic.train, titanic.test)

## Take a quick look at the data
summary(titanic)

###############################
## Data Cleaning / Wrangling ##
###############################

## Remove some variables
titanic <- titanic %>% select(-PassengerId, -Name, -Ticket, -Cabin)

## If Missing Embarkment make an S
titanic$Embarked[titanic$Embarked==""] <- "S"
titanic$Embarked <- as.factor(titanic$Embarked)

## Change Pclass and Sex to a factor
titanic <- within(titanic, {
  Pclass <- as.factor(Pclass)
  Sex <- as.factor(Sex)
})

## Fill in missing ages with MLR prediction
age.lm <- lm(log(Age)~Pclass+Sex+SibSp+Parch+Fare+Embarked, 
             data=titanic)
age.pred <- predict.lm(age.lm, newdata=titanic %>% filter(is.na(Age))) %>% 
  exp() %>% round()
titanic[is.na(titanic$Age),'Age'] <- age.pred

## Fill in missing fare in test set with MLR prediction
fare.lm <- lm(Fare~Pclass+Sex+SibSp+Parch+Age+Embarked, data=titanic)
titanic$Fare[is.na(titanic$Fare)] <- predict.lm(fare.lm, newdata=titanic[is.na(titanic$Fare),]) %>% max(., 0)


### K NEAREST NEIGHBOR ALGORITHM ###
library(class)
library(fastDummies)

# Create data matrices
x <- dummy_cols(titanic %>% select(-Survived)) %>% select(-Sex, -Embarked, -Pclass)
y <- as.factor(titanic$Survived)

# Scale the X's (standardize with same mean and SD)
x <- scale(x)

# Split back into training and test sets
x.train <- x[!is.na(titanic$Survived),]
x.test <- x[is.na(titanic$Survived),]
y.train <- y[!is.na(y)]

# Cross-Validation to find best value of k
set.seed(17)
test.obs <- sample(1:nrow(x.train), round(0.1*nrow(x.train)))
test.x <- x.train[test.obs,]
train.x <- x.train[-test.obs,]
train.y <- y.train[-test.obs]
test.y <- y.train[test.obs]
acc <- rep(NA, 100)
for (k in 1:length(acc)) {
  knn.cv <- knn(train=train.x, cl=train.y, test=test.x, k=k)
  acc[k] <- mean(test.y==knn.cv)
}
qplot(x=1:length(acc), y=acc, geom='line')
best.k <- which.max(acc)
best.k

# Predictions with KNN
knn.preds <- knn(train=x.train, cl=y.train, test=x.test, k=best.k, prob=FALSE)
knn.preds

