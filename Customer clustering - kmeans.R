##k-means Clustering in R

library(dplyr)

#Read and explore the data

##--------------------------------------Step1 : Reading and exploring the dataset ---------------------------------------------
setwd("C:\\xxxxx\\k means Clustering")
data<-read.csv("cust.csv")

dim(data)
str(data)
glimpse(data)
names(data)
summary(data)

colSums(is.na(data))
#Frequency distribution
table(data$Channel)
table(data$Region)


names(data)
sample<-data[,3:8]
dim(sample)
str(sample)
names(sample)

##--------------------------------------Step2 : Scaling the data ---------------------------------------------
#(column  - mean(column))/sd(column)
#Repeat for all columns

list<-names(sample)
scaled_data<-data.frame(rownum<-1:440)
head(scaled_data)
for(i in 1:length(list))
{
  
  x<-(sample[,i]-mean(sample[,i]))/(sd(sample[,i]))
  scaled_data<-cbind(scaled_data,x)
  names(scaled_data)[i+1]<-paste("scaled_",list[i])
  print(list[i])
  
}
head(scaled_data)

scaled_data<-scaled_data[,-1]
# 
sample<-cbind(sample,scaled_data)
names(sample)
head(sample)
sample


##--------------------------------------Step3 : kmeans algorithm ---------------------------------------------

#syntax : kmeans(scaled_data,k) ; where k refers to the number of clusters
set.seed(200)

fit.km<-kmeans(sample[,7:12],4)
fit.km
fit.km$cluster

#We will get a list object
fit.km$size #No of observations in each cluster
fit.km$withinss #Within sum of squares metric for each cluster
fit.km$totss #The total sum of squares
fit.km$tot.withinss #Total within-cluster sum of squares, i.e., sum(withinss)
fit.km$betweenss #The between-cluster sum of squares, i.e. totss-tot.withinss

Fit <- fit.km$betweenss/fit.km$totss #when withinss = 0, Fit = 1
Fit

##--------------------------------------Step4 : find the optimal number of clusters (k value) ---------------------------------------------

#Create a screeplot-plot of cluster's tot.withinss wrt number of clusters

wss<-1:15

for (i in 1:15)  #How many clusters or 'k' to form? check for where the curve straightens
  
{
  wss[i]<-kmeans(sample[,7:12],i)$tot.withinss
}


number<-1:15
#Plot graph to find optimal cluster value(k)
plot(number,wss)#So k=7 to 10 clusters in this example

#Shortlised optimal number of clusters : between 6 and 9

#Better plot using ggplot2
library(ggplot2)
data1<-data.frame(wss,number)
p<-ggplot(data1,aes(x=number,y=wss),color="red")
p+geom_point()+scale_x_continuous(breaks=seq(1,20,1))

##--------------------------------------Step5a : Rerun the algorithm with k=7(optimal no)---------------------------------------------

#Build 7 cluster model
set.seed(100)
head(sample)
fit.km<-kmeans(sample[,7:12],7)

##Merging the cluster output with original data
sample$cluster<-fit.km$cluster
head(sample)

#We will get a list object
fit.km$size #No of observations in each cluster
fit.km$withinss #Within sum of squares metric for each cluster
fit.km$totss #The total sum of squares
fit.km$tot.withinss #Total within-cluster sum of squares, i.e., sum(withinss)
fit.km$betweenss #The between-cluster sum of squares, i.e. totss-tot.withinss

Fit <- fit.km$betweenss/fit.km$totss #when withinss = 0, Fit = 1
Fit

##--------------------------------------Step5b : Profile the clusters---------------------------------------------


#Cluster wise Aggregates
cmeans<-aggregate(sample[,1:6],by=list(sample$cluster),FUN=mean)
cmeans


#Population Aggregates
apply(sample[,1:6],2,mean)
apply(sample[,1:6],2,sd)

cmeans
list1<-names(cmeans)

#Z score calculation
#z score =population_mean - group_mean /population_sd
for(i in 1:length(list1))
{
y<-(cmeans[,i+1] - apply(sample[,1:6],2,mean)[i])/(apply(sample[,1:6],2,sd)[i])
#(sample mean - population mean)/sample sd
#apply(sample[,1:6],2,mean)
#for each row in Fresh subtract from population mean and divide by population sd
cmeans<-cbind(cmeans,y)
names(cmeans)[i+7]<-paste("z",list1[i+1],sep="_")
print(list1[i+1])
}
cmeans
ncol(cmeans)
cmeans<-cmeans[,-14]
fit.km$size
cmeans$size_p <- fit.km$size/nrow(sample)
cmeans

##--------------------------------------Step6a : Rerun the algorithm with k=9(in order to compare)---------------------------------------------


#Build a 9 clusters
set.seed(200)
fit.km<-kmeans(sample[,7:12],9)

sample$cluster<-fit.km$cluster
head(sample)

#We will get a list object
fit.km$size #No of observations in each cluster
fit.km$withinss #Within sum of squares metric for each cluster
fit.km$totss #The total sum of squares
fit.km$tot.withinss #Total within-cluster sum of squares, i.e., sum(withinss)
fit.km$betweenss #The between-cluster sum of squares, i.e. totss-tot.withinss

Fit <- fit.km$betweenss/fit.km$totss #when withinss = 0, Fit = 1
Fit

##--------------------------------------Step6a : Rerun the algorithm with different k(in order to compare)---------------------------------------------

#check for cluster sizes such that no cluster is too big
fit.km<-kmeans(sample[,7:12],9)
fit.km$size
fit.km<-kmeans(sample[,7:12],8)
fit.km$size
fit.km<-kmeans(sample[,7:12],7)
fit.km$size
fit.km<-kmeans(sample[,7:12],6)
fit.km$size
fit.km<-kmeans(sample[,7:12],5)
fit.km$size
fit.km<-kmeans(sample[,7:12],4)
fit.km$size


#9 and 8 seems to have smaller numbers. Trying 9 first in below code
#For 9, cluster number six seems to be low spending throughout. Rest of them fail to show a business reason, they are either small or have no meaning
#For 8, cluster 7 is low spending cluster, cluster 2 is Fresh, 5 is Milk, 4 is frozen, 1 and 8 can't be classified
#For 7, not much improvement
#For 6

sample$cluster<-fit.km$cluster
head(sample)

#We will get a list object
fit.km$size #No of observations in each cluster
fit.km$withinss #Within sum of squares metric for each cluster
fit.km$totss #The total sum of squares
fit.km$tot.withinss #Total within-cluster sum of squares, i.e., sum(withinss)
fit.km$betweenss #The between-cluster sum of squares, i.e. totss-tot.withinss

Fit <- fit.km$betweenss/fit.km$totss #when withinss = 0, Fit = 1
Fit


##--------------------------------------Step6b : Profile the clusters---------------------------------------------


#Cluster summary - actuals
cmeans1<-aggregate(sample[,1:6],by=list(sample$cluster),mean)

#cluster summary z values
#Population summary
apply(sample[,1:6],2,mean)
apply(sample[,1:6],2,sd)
cmeans1

#z-score calculation function
list2<-names(cmeans1)
#z score =population_mean - group_mean /population_sd
for(i in 1:length(list2))
{
  print(i)
  y<-(cmeans1[,i+1] - apply(sample[,1:6],2,mean)[i])/(apply(sample[,1:6],2,sd)[i])
  cmeans1<-cbind(cmeans1,y)
  names(cmeans1)[i+7]<-paste("z",list2[i+1],sep="_")
  print(list2[i+1])
}

cmeans1
ncol(cmeans1)
cmeans1<-cmeans1[,-14]
names(cmeans1)
cmeans1$size_p <- fit.km$size/nrow(sample)*100
cmeans1

#Population summary
apply(sample[,1:6],2,mean)
apply(sample[,1:6],2,sd)
#Are Fresh and Frozen same?
cor(sample$Fresh,sample$Frozen) #not necessarily

#Note : The higher the value of z, the different is the group from the population

#If still not satisfied, again run an iteration


##--------------------------------------Step7 : Visualise the clusters---------------------------------------------

#Plotting groups across two dimensions

library(ggplot2)
data2<-cbind(data,sample)

#For cluster7
#Milk Vs Grocery
p<-ggplot(data2,aes(x=Milk,y=Grocery))
p+geom_point(aes(colour=as.factor(cluster)))
  
#Across Region
p+geom_point(aes(colour=as.factor(cluster)))+
  facet_grid(Region~.)

#Across Channel
p+geom_point(aes(colour=as.factor(cluster)))+
  facet_grid(Channel~.)

#For cluster9

#Milk Vs Grocery
p<-ggplot(data2,aes(x=Milk,y=Grocery))
p+geom_point(aes(colour=as.factor(cluster9)))

#Across Region
p+geom_point(aes(colour=as.factor(cluster9)))+
  facet_grid(Region~.)

#Across Channel
p+geom_point(aes(colour=as.factor(cluster9)))+
  facet_grid(Channel~.)

#Analysis
p+geom_point(aes(colour=as.factor(cluster9),size=Fresh))+
  facet_grid(Region~.)

p+geom_point(aes(colour=as.factor(cluster9),size=Frozen))+
  facet_grid(Region~.)

p+geom_point(aes(colour=as.factor(cluster9),size=Detergents_Paper))+
  facet_grid(Region~.) #You see some impact

p+geom_point(aes(colour=as.factor(cluster9),size=Delicassen))+
  facet_grid(Region~.) 

#Further Deep dive
p+geom_point(aes(colour=as.factor(cluster9),size=Detergents_Paper))+
  facet_grid(Region~Channel) #You see some impact

