# data wrangling

library(tidyverse)

library(forcats)

library(stringr)

library(caTools)

# data assessment/visualizations

library(DT)

library(data.table)

library(pander)

library(ggplot2)

library(scales)

library(grid)

library(gridExtra)

library(corrplot)

library(VIM) 

library(knitr)

library(vcd)

library(caret)

# model

library(xgboost)

library(MLmetrics)

library('randomForest') 

library('rpart')

library('rpart.plot')

library('car')

library('e1071')

library(vcd)

library(ROCR)

library(pROC)

library(VIM)

library(glmnet) 

train <- read_csv('../input/train.csv')

test  <- read_csv('../input/test.csv')

train <- as.data.frame(train)
test <- as.data.frame(test)

median(train$Age, na.rm=TRUE)
median(test$Age, na.rm=TRUE)

train$IsTrainSet <- TRUE
test$IsTrainSet <- FALSE

names(train)
names(test)
dim(train)
dim(test)
head(test)
test$Survived <- NA
full <- rbind(train, test)

dim(full)
tail(full)
table(full$IsTrainSet)
table(full$Embarked)
sum(is.na(full$Embarked))

full$Embarked[which(is.na(full$Embarked))] <- median(full$Embarked, na.rm=TRUE)
sum(is.na(full$Embarked))

sum(is.na(full$Age))
full$Age[which(is.na(full$Age))] <- median(full$Age, na.rm=TRUE)

sum(is.na(full$Fare))
#full$Fare[which(is.na(full$Fare))] <- median(full$Fare, na.rm=TRUE)

boxplot(full$Fare)
boxplot.stats(full$Fare)
upper.whisker <- boxplot.stats(full$Fare)$stats[5]
outlier.filter <- full$Fare < upper.whisker
full[outlier.filter, ]

#running linear reg model to predict missing avlues of Fare insetaed of just replacing by median

fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"

fare.model <- lm(
  formula = fare.equation,
  data = full[outlier.filter, ]
)

fare.row <- full[
  is.na(full$Fare),
  c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
  ]

fare.predictions <- predict(fare.model, newdata=fare.row)

full$Fare[which(is.na(full$Fare))] <- fare.predictions
sum(is.na(full$Fare))

full$Fare[1044]

full$Pclass <- as.factor(full$Pclass)
full$Sex <- as.factor(full$Sex)
full$Embarked <- as.factor(full$Embarked)

train <- full[full$IsTrainSet==TRUE, ]
test <- full[full$IsTrainSet==FALSE, ]
str(train)
str(test)
train$Survived <- as.factor(train$Survived)

equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch +  Fare + Embarked"
formula <- as.formula(equation)

model1 <- randomForest(formula, data=train, tree = 500, mtry = 3, nodesize = 0.01 * nrow(test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch +  Fare + Embarked"

Survived.pred <- predict(model1, newdata=test)

PassengerId <- test$PassengerId

output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived.pred

tail(output.df)

write.csv(output.df, file="Kaggle_submission.csv", row.names=FALSE)
