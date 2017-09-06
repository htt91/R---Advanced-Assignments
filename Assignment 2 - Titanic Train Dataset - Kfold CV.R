# STEP 1: START #

install.packages("gmodels")
install.packages("Hmisc")
install.packages("pROC")
install.packages("ResourceSelection")
install.packages("car")
install.packages("caret")
install.packages("dplyr")
library(gmodels)
library(Hmisc)
library(pROC)
library(ResourceSelection)
library(car)
library(caret)
library(dplyr)
install.packages("InformationValue")
library(InformationValue)

cat("\014") # Clearing the screen

# STEP 1: END #

setwd("C:/YYYYYY/AMMA 2017/data_2017/Titanic")
titanic_data= read.csv("train.csv")

###understanding data
View(titanic_data)
str(titanic_data)
head(titanic_data)

###selecting necessary variables
titanic_final= titanic_data[c("Pclass" ,"Sex" ,"Age" ,"SibSp", "Parch","Survived")]
View(titanic_final)
str(titanic_final)
summary(titanic_final)

###Missing value imputation
titanic_final$Age[is.na(titanic_final$Age)]= mean(titanic_final$Age[!is.na(titanic_final$Age)])
summary(titanic_final)

###Splitting data 
set.seed(1234)
df.titanic <- runif(nrow(titanic_final))
titanic_train <- titanic_final[df.titanic <= 0.7,]
titanic_test<- titanic_final[df.titanic> 0.7,]


########################### Decision Tree ###############################

install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")

fit = rpart(Survived ~ Pclass+Sex+Age, titanic_train, method = "class")
plot_fit = plot(fit,uniform = TRUE, main = "Classification Tree for Titanic")
text(fit,use.n = TRUE, all = TRUE, cex =.8)

rpart.plot(fit)


####################################### Random Forest ####################################################  

install.packages("randomForest")
library("randomForest")
?randomForest
rf= randomForest(Survived ~ Pclass+Sex+Age+Parch+SibSp, data=titanic_train, mtry = 2, importance = TRUE)
print(rf)
summary(rf)
varImpPlot(rf)

titanic_train$randomprob = predict(rf,type=c("response"))
titanic_train$randompred = ifelse(titanic_train$randomprob>=0.5, 'pred_yes','pred_no')
View(titanic_train)

#Confusion Matrix
table(titanic_train$randompred,titanic_train$Survived)

##Index for Measuring Training Data
accuracy_random = (344+168)/(344+71+39+168)
print(accuracy_random)

###Roc curve of training data
titanic_train_roc <- roc(Survived ~ prob , data = titanic_train)
plot(titanic_train_roc)
auc(titanic_train_roc)




################################# SVM #######################################

install.packages("e1071")
library("e1071")
vm = svm(Survived ~ Pclass+Sex+Age+Parch+SibSp, data=titanic_train, mtry = 2, importance = TRUE)
print(vm)


titanic_train$svm_prob = predict(vm,type=c("response"))
titanic_train$svm_pred = ifelse(titanic_train$svm_prob>=0.5, 'pred_yes','pred_no')
View(titanic_train)

#Confusion Matrix
table(titanic_train$svm_pred,titanic_train$Survived)

##Index for Measuring Training Data
accuracy_svm = (342+175)/(342+64+41+175)
print(accuracy_svm)

###Roc curve of training data
titanic_train_roc <- roc(Survived ~ prob , data = titanic_train)
plot(titanic_train_roc)
auc(titanic_train_roc)
