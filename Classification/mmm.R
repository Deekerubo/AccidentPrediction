#perfome linear regression on training data 
reg<- lm(Accident_Severity~.,data=train_data)
#predicttestdata using liner modelling
predict<- predict(reg,newdata=test_data)
predict
plot(predict)
#testing the posibility of accident severity with values less than
Tab<- table(test_data$Accident_Severity, predict>2.8 )
Tab
#calculate the miscalculation error
mcrate<- 1-sum(diag(Tab))/sum(Tab)
mcrate
View(mcrate)
plot(mcrate)
#............rpart decison tree.......
library(rpart)
#library(plyr)
#library(dplyr)
#training model1
model1<- rpart(Accident_Severity~., train_data, method ="class")
model1<-predict(model1, test_data,type ="class")
View(model1)
summary(model1)
plot(model1)