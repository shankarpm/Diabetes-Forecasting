#Load the data set from diabetes spreadsheet
dataset<-read.csv("Data\\Diabetes.csv",head=T)

head(dataset)
colnames(dataset)
nrow(dataset)

set.seed(2)
dataset$ind<-sample(2,nrow(dataset),replace=TRUE,prob=c(0.7,0.3))

head(dataset)
#split the datasets into training and test
trainData<-dataset[(dataset$ind==1),]
testData<-dataset[(dataset$ind==2),]
head(trainData)

trainData<-trainData[,-52]
testData<-testData[,-52] 

head(trainData)

############train the model using ML DECISION TREE with selected datapoints and training dataset#########################
dtModel<-rpart(Class.variable~Number.of.times.pregnant
				+Plasma.glucose.concentration
				+Diastolic.blood.pressure
				+Triceps.skin.fold.thickness
				+X2.Hour.serum.insulin
				+Body.mass.index
				+Diabetes.pedigree.function
				+Age..years., 
		data=trainData,
		control=rpart.control(minsplit=10))

dtModel

plot(dtModel)
text(dtModel)

####Predict####
predDT<-predict(dtModel,testData,type=c("class"))
length(predDT)
length(testData$Class.variable)
#cbind(as.character(testData$Class.variable),as.character(pred))

#####confusion matrix######
table(as.character(testData$Class.variable),as.character(predDT))
#    NO YES
#NO  127  25
#YES  29  55

#Accuracy of the above datapoints using Decision Tree - 77%

####Predict-Prob#####
predDTProb<-predict(dtModel,testData,type=c("prob"))
head(predDTProb[,2])
length(predDTProb)
  
attributes(predDTProb)
aucDT <- roc(ifelse(testData$Class.variable=="YES",1,0), predDTProb[,2])
plot(aucDT)
aucDT
print(aucDT$auc)
#Area under the curve: 0.7815

#see the importance of all the selected datapoints
dtModel$variable.importance

dtModelFull<-rpart(Class.variable~., data=trainData, control=rpart.control(minsplit=10))

predDTProbAllDataPoints<-predict(dtModelFull,testData,type=c("class"))

#see the importance of all the datapoints
dtModelFull$variable.importance

#####confusion matrix with all data points######
table(as.character(testData$Class.variable),as.character(predDTProbAllDataPoints))
#     NO YES
#NO  116  36
#YES  45  39

#Accuracy of the above datapoints using Decision Tree - 65%

predDTProbAllDataPointsProb<-predict(dtModelFull,testData,type=c("prob"))
 
aucDTAllDataPoints <- roc(ifelse(testData$Class.variable=="YES",1,0), predDTProbAllDataPointsProb[,2])
plot(aucDTAllDataPoints)
print(aucDTAllDataPoints$auc)

#Area under the curve: 0.61
#####New Prediction######
#New<-read.csv("New.csv",head=T)
#predict(dt,New,type=c("class"))

#train the model using ML RANDOM FOREST with selected datapoints and training dataset#########################
###########RANDOM FOREST###################################################################
  
library(randomForest)

#train the model using ML Random Forest with selected datapoints and training dataset
rfModel<-randomForest(Class.variable~Number.of.times.pregnant
				+Plasma.glucose.concentration
				+Diastolic.blood.pressure
				+Triceps.skin.fold.thickness
				+X2.Hour.serum.insulin
				+Body.mass.index
				+Diabetes.pedigree.function
				+Age..years., 
		data=trainData,ntree=600,mtry=4)


rfModel

attributes(rfModel)

rfModel$importance

#Predict the model using Random forest
predRF<-predict(rfModel,testData,type=c("class"))

predRFProb<-predict(rfModel,testData,type=c("prob")) 

#####confusion matrix of Random Forest######
table(as.character(testData$Class.variable),as.character(predRF))
#     NO YES
#NO  127  25
#YES  23  61


#Accuracy of the above datapoints using Random Forest - 79.66%
attributes(predRF)
aucRF <- roc(ifelse(testData$Class.variable=="YES",1,0), predRFProb[,2])
plot(aucRF)
#Area under the curve: 0.866
print(aucRF$auc)
dt$variable.importance

#PRedicting the Model using all the data points using Random Forest##############
datasetRF <-read.csv("Data\\Diabetes.csv",head=T, na.strings=c("", "?", "NA", "NULL"))
 
datasetRF$ind<-sample(2,nrow(datasetRF),replace=TRUE,prob=c(0.7,0.3))
 
#split the datasets into training and test
#trainDataRF<-datasetRF[(datasetRF$ind==1),]
#testDataRF<-datasetRF[(datasetRF$ind==2),]
#trainDataRF<-trainDataRF[,-52]
#testDataRF<-testDataRF[,-52] 
#head(trainDataRF)
#trainDataRF[1,]
#trainData[1,]

f <- as.formula(paste(names(trainData)[1], "~", paste(names(trainData)[2:21], collapse=" + "))) 
#f <- as.formula(paste(names(trainDataRF)[1], "~", paste(names(trainDataRF)[2:21], collapse=" + "))) 

rfAllDataPointsModelFull <-randomForest(f, data=trainData,ntree=600,mtry=4)

predRFProbAllDataPoints<-predict(rfAllDataPointsModelFull,testData,type=c("class"))
 
#####confusion matrix with first 50% data points######
table(as.character(testData$Class.variable),as.character(predRFProbAllDataPoints))
#    NO YES
#NO  135  17
#YES  32  52

#Accuracy of the above datapoints using Decision Tree - 79%
predRFProbAllDataPointsProb<-predict(dtModelFull,testData,type=c("prob"))

aucRFAllDataPoints <- roc(ifelse(testData$Class.variable=="YES",1,0), predRFProbAllDataPointsProb[,2])
plot(aucRFAllDataPoints)
print(aucRFAllDataPoints$auc)

#Area under the curve: 0.61

#train the model using ML NAIVE BAYES with selected datapoints and training dataset#########################
####################NAIVE BAYES#################################################################
  
library(e1071)

NBModel<-naiveBayes(Class.variable~Number.of.times.pregnant
				+Plasma.glucose.concentration
				+Diastolic.blood.pressure
				+Triceps.skin.fold.thickness
				+X2.Hour.serum.insulin
				+Body.mass.index
				+Diabetes.pedigree.function
				+Age..years., 
		data=trainData)

attributes(NBModel)

NBModel$apriori
NBModel$tables


predNB<-predict(NBModel,testData,type=c("class"))
 

###Confusion Matrix####
table(testData$Class.variable,predNB)

#     NO YES
#NO  129  23
#YES  28  56
#Accuracy of the above datapoints using Decision Tree - 78.3%
#predNB[,2]
predNBProb<-predict(NBModel,testData,type=c("raw"))
aucNB <- roc(ifelse(testData$Class.variable=="YES",1,0), predNBProb[,2])
plot(aucNB)

 #Area under the curve: 0.8398
print(aucNB$auc)
 
#PRedicting the Model using all the data points using Naive bias##############

NBModelAllDP<-naiveBayes(Class.variable~., data=trainData)

predNBclass<-predict(NBModelAllDP,testData,type=c("class"))
predNBProb<-predict(NBModelAllDP,testData,type=c("raw"))

###Confusion Matrix####
table(testData$Class.variable,predNBclass)
 
#    NO YES
#NO  120  32
#YES  29  55
#Accuracy of the above datapoints using Decision Tree - 74.3%

auc <- roc(ifelse(testData$Class.variable=="YES",1,0), predNBProb[,2])
plot(auc)
#Area under the curve: 0.7564


####################Logistic Regression#################################################################
  
trainDataLR <- trainData
trainDataLR$ouput <-  ifelse(trainDataLR$Class.variable=="YES",1,0)
testDataLR <- testData
testDataLR$ouput <-  ifelse(testDataLR$Class.variable=="YES",1,0)

head(trainDataLR)
head(testDataLR)

glmModel<-glm(ouput~Number.of.times.pregnant
                    +Plasma.glucose.concentration
                    +Diastolic.blood.pressure
                    +Triceps.skin.fold.thickness
                    +X2.Hour.serum.insulin
                    +Body.mass.index
                    +Diabetes.pedigree.function
                    +Age..years., 
                    data=trainDataLR,family = "binomial")

attributes(glmModel)
 
predLM1<-predict(glmModel,testDataLR,type=c("response")) 

survival <- vector()
for(i in 1:length(predLM1)) {
  if(predLM1[i] > .5) {
    survival[i] <- 1
  } else {
    survival[i] <- 0
  }
}
length((survival))
length(testDataLR$ouput)
###Confusion Matrix####
table(testDataLR$ouput,survival)
#   0   1
#0 135  17
#1  27  57
#Accuracy of the above datapoints using Logistic regression - 81%
  
aucGLM <- roc(testDataLR$ouput, survival)
plot(aucGLM)

#Area under the curve: 0.7834
print(aucGLM$auc) 

 
 

