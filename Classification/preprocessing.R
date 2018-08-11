#loading libraries
library(mlbench)
library(ggplot2)
library(lubridate)
library(caret)
library(readr)
library(data.table)
#loading the data
Accident<- read.csv(file = "mydata.csv")
head(Accident)
#checking for missing data
summary(Accident$comfort)
dim(Accident)
summary(Accident)
attach(Accident)
#categorical data
Accident$Speed_limit=factor(Accident$Speed_limit,levels = c('20','30','40','50','60','70'), labels = c(1,2,3,4,5,6))

library(caTools)
set.seed(1234)
# splits the data in the ratio mentioned in SplitRatio. 
#After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
split<-sample.split(Accident, SplitRatio = 0.8)
split
train_data<-subset(Accident,split==TRUE)
test_data<-subset(Accident,split==FALSE)

#............support vector machine.........
library(e1071)
library(caret)
set.seed(1234)
t<-tune(svm,Accident_Severity~.,data=train_data,ranges = list(cost=c(0.001,0.01,0.1,1,10,100)))
summary(t)  # best cost = 0.2
reg2=svm(Accident_Severity~.,data=train_data,cost = 0.2,scale=F,gamma = 1)
reg2

q<-predict(reg2,test_data)
q
plot(q)
#confusionMatrix
comp<-(1-mean(q!=test_data$Accident_Severity))*100
accuracy<-(sum(diag(comp))/sum(comp))*100
accuracy

set.seed(1234)
reg3=svm(Accident_Severity~.,data=train_data,kernel="linear",cost = 0.2,scale=F)
 reg3

q<-predict(reg3,test_data)
q
confusionMatrix(q,test_data$Accident_Severity)

comp<-ftable(q,test_data$Accident_Severity)
accuracy<-(sum(diag(comp))/sum(comp))*100
accuracy #.......98%......

#.................rpart decision tree........
library(rpart)
# grow tree 
set.seed(1234)
fit<-rpart(Accident_Severity~.,data=train_data,method = "class")
fit
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

tree.predict <- predict(fit,test_data,type = "class")
accuracy_decisionTree<-(1-mean(tree.predict!=test_data$Accident_Severity))*100
accuracy_decisionTree
plot(tree.predict)
#......accuracy 82%
#....random forest.....
library(randomForest)
