options(scipen=10) #A penalty to be applied when deciding to print numeric values in fixed or exponential notation. Positive values bias towards fixed and negative towards scientific notation: fixed notation will be preferred unless it is more than scipen digits wider.

##--------------------------------------Step1 : Read the data---------------------------------------------
setwd("C:\\xxxxxxx\\Heirarchichal Clustering")
europe<-read.csv("europe.csv")
names(europe)
dim(europe)
head(europe)
summary(europe)
#Initial findings:
#Luxembourg has the lowest area
#Ukraine has the highest inflation
#Spain and Greece have the highest unemployment


##--------------------------------------Step2 : Scaling data + Finding Distance Measures---------------------------------------------

#Distance calculation : Euclidean
#Method : Average

#Daisy function in R scales the data and computes the distance measures
library(cluster)
??daisy
?dist
dmatrix<-daisy(europe[-1],metric="euclidean",stand=TRUE) #first column is categorical which is not used
#stand = TRUE is to normalize the data
#default normalization method is subtract by mean and divide by stdev (z)
#If there are multiple variable types like continous(ratio)/ordinal/binary, then Gower's similarity is used
summary(dmatrix) #Dissimilarity matrix
#there are 378 because (378+378+28) is 28 square. So only the lower triangle of dissimilarity matrix is stored
#Our data has 28 rows
#The dissimilarity matrix is symmetric and therefore only lower half can be considered
#Also the diagonal containing 28 rows is omitted
class(dmatrix)
str(dmatrix)
head(dmatrix)

distmatrix<-dist(dmatrix)
distmatrix
#scroll up to see the whole matrix

str(distmatrix) #size is 28, diagonal and upper triangle are false
class(dmatrix)
?dist
?daisy
#Writing out the file
d<-as.matrix(distmatrix)
class(d)
head(d)
write.csv(d,"distmatrix.csv",row.names=F)

##--------------------------------------Step3 : Hierarchcal clusterting algorithm---------------------------------------------

#hclust 
euroclust<-hclust(distmatrix,method="average")
str(euroclust)
#euroclust<-hclust(distmatrix,method="complete")
?hclust

##--------------------------------------Step4 : Plotting a Dendogram---------------------------------------------

plot(as.dendrogram(euroclust))

#labels for each of the objects being clustered
euroclust$labels
euroclust$labels<-europe$Country
plot(as.dendrogram(euroclust))
#Luxembourg has the lowest area
#Ukraine has the highest inflation
#Spain and Greece have the highest unemployment

rect.hclust(euroclust, 5) #draw five rectangles or 5 clusters (can be selected by choosing 4th horizontal line from top because the y axis is the distance between those 2 clusters)
#If it doesn't run, then keep the output which doesn't show graph open and then run dendrogram and hclust code again

#To get flat clustering : 
k<-cutree(euroclust,k = 5)
k
#Once you have k which is the cluster no, attach it to your dataset
europe$cluster<-k
europe
##--------------------------------------Step5 : Examining the hclust object---------------------------------------------


#The cluster height used in the dendogram
euroclust$height #Note there are n-1 or 27 heights which is the distance for (n-1) that get formed

#In the dendrogram, the y-axis is simply the value of this distance metric between clusters. For example- 
#-if you see two clusters merged at a height x, it means that the distance between those clusters was x.

#query to create euroclust
euroclust$call

#distance measure used
euroclust$dist.method

##----------------------------------Step6 : Slicing the dendogram to get finite number of clusters---------------------------------------------

rect.hclust(euroclust, 4) #draw five rectangles or 5 clusters (can be selected by choosing 4th horizontal line from top because the y axis is the distance between those 2 clusters)
#If it doesn't run, then keep the output which doesn't show graph open and then run dendrogram and hclust code again

#To get flat clustering : 
k<-cutree(euroclust,k = 4)



##----------------------------------Step7 : Profiling clusters---------------------------------------------
#You can now profile your data similar to kmeans clustering example

#Cluster wise Summaries
cmeans<-aggregate(europe[,2:8],
                  by=list(europe$cluster),
                  FUN=mean)
names(cmeans)
dim(cmeans)
cmeans

#Population Summaries
apply(europe[,2:8],2,mean)
apply(europe[,2:8],2,sd)

#Z value normalisation

#Function to calculate Z values
list<-names(cmeans)
list
#z score =population_mean - group_mean /population_sd
for(i in 1:length(list))
{
  y<-(cmeans[,i+1] - apply(europe[,2:8],2,mean)[i])/(apply(europe[,2:8],2,sd)[i])
  cmeans<-cbind(cmeans,y)
  names(cmeans)[i+8]<-paste("z",list[i+1],sep="_")
  print(list[i+1])
}
ncol(cmeans)
cmeans<-cmeans[,-16]
cmeans
write.csv(cmeans,"cmeans.csv",row.names=F)
x<-table(europe$cluster,europe$Country)
x
cluster_size <- rowSums(x)
cluster_size

#can use sqldf
library(sqldf)
sqldf("select count(*) from europe group by cluster")

##----------------------------------Step8 : Other Packages to Plot a dendogram---------------------------------------------

#ggdendro
install.packages('ggdendro')
library(ggdendro)
# basic option

ggdendrogram(euroclust, 
             rotate = FALSE, 
             theme_dendro = TRUE, 
             color = "tomato")

#A2R
#load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
#colored dendrogram
op = par(bg = "#EFEFEF")
A2Rplot(euroclust, 
        k = 5, 
        cex=0.2,
        boxes = FALSE, 
        col.up = "grey50", 
        col.down = c("green","blue", "black","red","brown"))
