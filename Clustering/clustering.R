library(cluster)
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


