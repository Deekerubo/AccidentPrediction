library(cluster)
library(fpc)
library(rpart)
library(clusterSim)
#kmeans clustering
Accident<- read.csv(file = "mydata.csv")
View(Accident)
#clear missing values
Data<-na.omit(Accident)
dim(Data)
summary(Data)
Acc<-Data
View(Acc)
#Encoding  categorical data
Acc$Speed_limit=factor(Acc$Speed_limit,levels = c('20','30','40','50','60','70'), labels = c(1,2,3,4,5,6))
#scaling the data
#Scaled_Acc<- as.matrix(scale(Acc))

Acc.feature<- Acc
View(Acc.feature)
#remove other variables to perfome clustering on 3 variables
Acc.feature$Police_Force<- NULL 
Acc.feature$Accident_Severity <- NULL
Acc.feature$Location<- NULL
Acc.feature$Speed_limit<- NULL
Acc.feature$Light_Conditions<- NULL
Acc.feature$Weather_Conditions<- NULL
Acc.feature$Urban_or_Rural_Area<- NULL
View(Acc.feature)
scale.Acc<-scale(Acc.feature)
View(scale.Acc)
#Elbow Method for finding the optimal number of clusters
set.seed(1234)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- scale.Acc
wss <- sapply(1:k.max,function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#perfom clustering on the remaining 3 variables
results<- kmeans(data, 3)
results
#check cluster components
results$size
results$cluster
results$centers
results$totss
results$withinss    
results$tot.withinss
results$betweenss
results$iter
results$ifault
#plotting
clusplot(Acc.feature,results$cluster, color = TRUE, shade = TRUE)
plot(Acc.feature)
Accident<- read.csv(file = "mydata.csv")

data_set <- mydata
attach(data_set)

target <- Accident_Severity
nobs <- nrow(data_set)
form <- formula(paste(target, "~ ."))
#eval(pars
#Dataset Division
form_column <- eval(parse(text=paste("data_set",target,sep="$")))
new_data_set <- subset(data_set, is.na(data_set))
nobs <- nrow(data_set) # 4571 observations 
train <- sample(nobs, 0.6*nobs) # 2742 observations
trainset <- data_set[train, ]
validate <- sample(setdiff(seq_len(nobs), train), 0.15*nobs) # 685 observations
validateset <- data_set[validate, ]
test <- setdiff(setdiff(seq_len(nobs), train), validate) # 1144 observations
testset <- data_set[test,]

#Accidents <- setdiff(colnames(data_set),list(target))#,ignore))
#dsFormula <- as.formula(paste(target,paste(Accidents,collapse=' + '),sep=" ~ "))
model <- rpart(data_set, data=trainset, method="class")
model
model$variable.importance
summary(model)
cp <- printcp(model)  #Complexity Parameter
predicted <- predict(model, newdata=testset)
# Extract the relevant variables from the dataset.
pred_loc <- ncol(data_set)
sdata <- subset(data_set[test,]) 

variance <-  round((predicted - sdata[,pred_loc])/sdata[,pred_loc] * 100,2)
variance_value <- (predicted - sdata[,pred_loc])
res <- cbind(sdata, predicted,variance_value,variance)
# New data prediction
predicted_new <- predict(model, newdata=data_set)
res_new <- cbind(predicted_new,data_set)
names(res_new)[1]<-paste(target, "_predicted")
head(res_new)

