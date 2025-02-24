# load R libraries
library(tidyverse)
library(dplyr)

## Logistic Regression
# load data
churn <- data.frame(read.csv("Churn-Dataset.csv"))
head(churn)

# change datatype of label (Churn) into factor
churn$Churn <- factor(churn$Churn,
                         levels = c("No","Yes"))
churn %>% head()
str(churn)

# recheck label class and number of Yes and No
class(churn$Churn)
table(churn$Churn)

# data summary and filter out the row with NA value
summary(churn)
sum(is.na(churn))
churn = filter(churn, TotalCharges != 0)
sum(is.na(churn))

# Split data to train test at ration 80:20
set.seed(89)
n <- nrow(churn)
id <- sample(1:n, size = n*0.8)
train_data <- churn[id, ]
test_data <- churn[-id, ]

# Train model using simple logistic regression algorithm with only numeric features
logit_model <- glm(Churn ~ tenure + MonthlyCharges + TotalCharges + numTechTickets + numAdminTickets, data = train_data, family = "binomial")

# set the probability of train in fraction and simply assigned to pred column in term of Ye and No
p_train <- predict(logit_model, type = "response")
train_data$pred <- if_else(p_train >= 0.5, "Yes", "No")

# simply check if the actual churn match with prediction from model and find the average which is simply calculation of accuracy
train_data$Churn == train_data$pred
mean(train_data$Churn == train_data$pred)

# create confusion matrix by hand calculation
conM <- table(train_data$pred, train_data$Churn, dnn = c("Predicted", "Actual"))

# Model evaluation
cat("Accuracy:", (conM[1,1] + conM[2,2]) / sum(conM))
cat("Presision:", conM[2,2] / (conM[2,1]+conM[2,2]))
cat("Recall:", cinM[2,2]/(conM[1,2]+conM[2,2]))

# Test model and find accuracy
p_test <- predict(logit_model, newdata = test_data, type = "response")
test_data$pred <- if_else(p_test >=0.5, "Yes", "No")
(mean(test_data$Churn == test_data$pred))


## Decision Tree
# Libraries needed
library(dplyr)       # for data processing
library(caret)       # for cross validation
library(rpart)       # for building tree model
library(rpart.plot)  # for visualizing the tree

# The data
telecom = read.csv("Churn-Dataset.csv",stringsAsFactors=TRUE)

# The ID is not used in our modeling
telecom = select(telecom,-customerID)
sum(is.na(telecom))
#is.na(telecom)

# Take a look at the data summary
summary(telecom)

# remove na from totalcharge column
telecom = filter(telecom,TotalCharges != 0)
sum(is.na(telecom))
summary(telecom)

#--------------------------------------------------------------------------------
# Split the data into training and testing partition
# - set.seed() function sets the random seed, which fixed the 
#   random number sequence to be generated.
# - sample() is a function for drawing random values from the vector x
# - x is the vector of values to be drawn from
# - size is the number of values we want to draw
# - replace=TRUE means than we can draw a value more than once
# - prob is a vector defining the draw probabilities of each value in x
set.seed(87100111)
pname = sample(x=c("train","test"),
               size=nrow(telecom),
               replace=TRUE,
               prob=c(0.8,0.2))

# Create training and testing partition
data_train = telecom[pname=="train",]
data_test = telecom[pname=="test",]

# Take a look at the distribution of the target categories
# There are many "NO" and very little "Yes", i.e. imbalance
table(data_train$Churn)

# Define cross validation settings
# method="cv" means cross validation
# number=10 means 10-fold cross validation
# sampling="up" means up-sampling the target class with less observations
train_ctl = trainControl(method="cv", number=10, sampling="up")

# Build a classification model using decision tree
model <- train(Churn ~ . , data = data_train, trControl=train_ctl, method = "rpart")

# You can try visualizing the final tree but it is too big to see
# rpart.plot(model$finalModel)

# There are 2 different types of prediction.
# By default, the predicted category will be produced (i.e. type="raw")
# We can also request predicted probability of each category (i.e. type="prob")
train_pred = predict(model)
train_pred_prob = predict(model, type="prob")
train_pred_df = cbind(train_pred_prob,predicted=train_pred)

test_pred = predict(model,newdata=data_test)
test_pred_prob = predict(model, newdata=data_test,type="prob")
test_pred_df = cbind(test_pred_prob,predicted=test_pred)

# Cross-validated confusion matrix
print(confusionMatrix(model))

# Confusion matrix and other accuracy measures using test partition
# - first argument is the predicted value
# - second argument is the true value
test_accuracy =  confusionMatrix(predict(model,newdata=data_test),data_test$Churn,positive="Yes", mode = "prec_recall")
print(test_accuracy)

# Variable importance
print(varImp(model))

# Store variable importance in a dataframe
var_importance = varImp(model)$importance
var_importance = arrange(var_importance,desc(Overall))

#------------------------------------------------------

#try logistic regression
model_logi <- train(Churn ~ . , data = data_train, trControl=train_ctl, family = "binomial", method= "glm")
train_pred_logi = predict(model_logi)
train_pred_prob_logi = predict(model_logi, type="prob")
train_pred_df_logi = cbind(train_pred_prob_logi,predicted=train_pred_logi)

test_pred_logi = predict(model_logi,newdata=data_test)
test_pred_prob_logi = predict(model_logi, newdata=data_test,type="prob")
test_pred_df_logi = cbind(test_pred_prob_logi,predicted=test_pred_logi)

print(confusionMatrix(model_logi))

test_accuracy_logi =  confusionMatrix(predict(model_logi,newdata=data_test),data_test$Churn,positive="Yes", mode = "prec_recall")
print(test_accuracy_logi)
