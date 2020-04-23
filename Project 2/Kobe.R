#setting file location
setwd("C:\\Users\\che\\Desktop\\SMU Books\\DS 6372\\Project2")

#reading a excel file
library(readxl)
kobe=read_excel("Kobe.xlsx")

##Data exploration#
dim(kobe)   #to know number of rows and columns
str(kobe)   #give type of columns

### top 5 lower and top 5 higest and unique, mean,
#all percentile show NA, missing also
install.packages("Hmisc")
library(Hmisc)
describe(kobe)

#removing unnessary variables
kobe=kobe[,-c(1,4,5,21,22,24,26)]

#
library(questionr)
describe(kobe)
#

#divide dataset into train and test
set.seed(1)
sampling=sort(sample(nrow(kobe), nrow(kobe)*.7))
length(sampling)

#train dataset
train<-kobe[sampling,]

#test dataset
test<-kobe[-sampling,]

#dividing numeric and categorical variable 
train_num=train[,sapply(train,is.numeric)]
train_cat=train[,!sapply(train,is.numeric)]

#convert character to factor 
train_cat[]=lapply(train_cat,factor)

#convert factor to date format
train_cat$game_date=as.Date(train_cat$game_date)

str(train_cat)

#findCorrelation
library(caret)
cor1=cor(train_num[,-10])   #finding correlation matrix
index=findCorrelation(cor1,cutoff = 0.7)       #checking multicollinearity
train_num=train_num[,-index]   #remove dependent variable
train=cbind(train_num,train_cat)   #bind categorical and numerical datasets

#finding importance feature using random-forest
##install.packages("randomForest")
library(randomForest)
rf =randomForest(shot_made_flag ~.,data = train)

#importance feature using random-forest
importance(rf)

#using plot 
varImpPlot(rf)   #select top 15 imporant feature

#logistic regression modelling 
myresult=glm(data=train,shot_made_flag~opponent+action_type+season+
             seconds_remaining+lon+attendance+avgnoisedb+minutes_remaining
             +shot_distance+game_date+arena_temp+period+shot_zone_area+combined_shot_type
             ,family=binomial)

summary(myresult)

#using step 
reduced<-step(myresult,direction="backward")

# iteration:2
myresult=glm(data=train,shot_made_flag ~ action_type + season + seconds_remaining + attendance + 
  minutes_remaining + shot_distance + arena_temp + period + 
  shot_zone_area,family=binomial)
summary(myresult)

## ***** Model Diagnostics ***** ##

# Checking For Multicollinearity
library(car)
vif(myresult)
# Variables => Ideally vif values should be < 5. Choosing vif cut-off value of 5, 
# 4 of the variables have vif of > 5 , showing Multicollinearity and should be removed from the model.
# Vars to remove from model are mou_Mean, avgmou, avg3qty, avg6mou


#creating dummy variable
install.packages("fastDummies")
library(fastDummies)
train_dummy=fastDummies::dummy_cols(train)


##select significant variables
train_dummy1=train_dummy[,c(7,33,26,24,27,46,54,53,41,56,29,31,37,32,29,38,67,65,59,58,51,42,36,25,22,21,20,32,37,38,40,43,52,62,89,90,92,93,5,8,2,6,9,3,15)]

#Iteration 3
myresult1=glm(data=train_dummy1,shot_made_flag ~.,family=binomial)
summary(myresult1)

#select significant variable 
train=train_dummy1[,c(1,3,5,8,9,11,12,13,16,19,24,26,27,31,35:45)]

#iteration:4
myresult=glm(data=train,shot_made_flag ~.,family=binomial)
summary(myresult)


#creating column for predicted values
train$predicted <- myresult$fitted.values

#install.packages("ROCR")
library(ROCR)

# The prediction function of the ROCR library basically creates 
# a structure to validate our predictions with actual values

pred<-prediction(train$predicted,train$shot_made_flag)

perf <- performance(pred,"acc")
class(perf)
perf
# x values contain the cut-off probabilities
#use @ to access the slots

class(perf@x.values)
cutoffprob <- as.numeric(unlist(perf@x.values))

class(perf@y.values)
accuracies <- as.numeric(unlist(perf@y.values))

#create DataFrame for cutoffprob and accuracy
cutoffs <- data.frame(cutoffprob, accuracies )

# In the decreasing order of accuracy
cutoffs <- cutoffs[order(cutoffs$accuracies, decreasing=TRUE),]

# Pick cutoff for which Accuracy is highest 
train$predclass <- ifelse(train$predicted>0.5284462,1,0)

# Kappa values and Confusion Matrix from caret package
library(caret)





library(irr)

kappa2(data.frame(train$shot_made_flag,train$predclass))

#confusion matrix to see accuracy,sensitivity 
confusionMatrix(as.factor(train$shot_made_flag),as.factor(train$predclass), positive = "1")


## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")


#area under curve
auc<-performance(pred,"auc")
auc




##test this model on this testing data set.
test_dummies=fastDummies::dummy_cols(test)
test_dummies$pred <- predict(myresult,type = "response",newdata =  test_dummies)
pred<-prediction(test_dummies$pred,test_dummies$shot_made_flag)

perf <- performance(pred,"acc")
class(perf)
# x values contain the cut-off probabilities
#use @ to access the slots

class(perf@x.values)
cutoffprob <- as.numeric(unlist(perf@x.values))

class(perf@y.values)
accuracies <- as.numeric(unlist(perf@y.values))

#create DataFrame for cutoffprob and accuracy
cutoffs <- data.frame(cutoffprob, accuracies )

# In the decreasing order of accuracy
cutoffs1 <- cutoffs[order(cutoffs$accuracies, decreasing=TRUE),]

# Pick cutoff for which Accuracy is highest 
test_dummies$predclass <- ifelse(test_dummies$pred>0.5284462,1,0)

# Kappa values and Confusion Matrix from caret package
library(caret)
library(irr)

kappa2(data.frame(test_dummies$shot_made_flag,test_dummies$predclass))

#confusion matrix to see accuracy,sensitivity 
confusionMatrix(as.factor(test_dummies$shot_made_flag),as.factor(test_dummies$predclass), positive = "1")


## computing a simple ROC curve (x-axis: fpr, y-axis: tpr)
perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")


#area under curve
auc<-performance(pred,"auc")
auc



####prediction for new file project2predict
project_predict=read_excel("project2Pred.xlsx")
project2predict_dummies=fastDummies::dummy_cols(project_predict)
project2predict_dummies$pred <- predict(myresult,type = "response",newdata = project2predict_dummies)
project2predict_dummies$predclass <- ifelse(project2predict_dummies$pred>0.5284462,1,0)


#new file
project2prediction=project2predict_dummies[,c(1:29,234,235)]
write.csv(project2prediction,"project2prediction.csv")