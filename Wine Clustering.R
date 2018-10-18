install.packages("rattle")
library(rattle)
#Rattle imports stats, utils, ggplot2, graphics, methods, stringi, stringr, tidyr,dplyr, XML, rpart.plot
data(wine,package="rattle")
head(wine)
names(wine)
class(wine)
str(wine)
colSums(is.na(wine))

#wine$type_1<-ifelse(wine$Type==1,1,0)
#wine$type_2<-ifelse(wine$Type==2,1,0)
#wine$type_3<-ifelse(wine$Type==3,1,0)

for(i in c(1,2,3))
{

x<-ifelse(wine$Type==(i),1,0)
wine<-cbind(wine,x)
names(wine)[i+1]<-paste("type_",(i))

}
names(wine)
summary(wine)
head(wine)
wine<-wine[,-1]
head(wine)

#scaled data=(data-mean)/standard deviation

scaled_data<-data.frame(rownum<-1:178)
list<-names(wine)
for(i in 1:length(list))
{
  x<-(wine[,i]-mean(wine[,i]))/(sd(wine[,i]))
  scaled_data<-cbind(scaled_data,x)
  names(scaled_data)[i+1]<-paste("scaled_",names(wine)[i])
}

head(scaled_data)
names(scaled_data)

scaled_data<-scaled_data[,-1]

number<-1:15
wss<-1:15

for(i in 1:15)
  
{
  wss[i]<-kmeans(scaled_data,i)$tot.withinss
  
}

plot(number,wss)

wine_bind<-cbind(wine,scaled_data)
names(wine_bind)

#now we will find K Values

#let say K=3
set.seed(101)
fit.km<-kmeans(wine_bind[,17:32],3)
fit.km$size
fit.km$cluster
wine_bind$cluster<-fit.km$cluster
names(wine_bind)

cmeans<-aggregate(wine_bind[,1:16],by=list(wine_bind$cluster),mean)
names(cmeans)

for(i in 1:length(names(cmeans)))
{
  y<-(mean(wine_bind[,i])-(cmeans[,i+1]))/(sd(wine_bind[,i]))
  cmeans<-cbind(cmeans,y)
  names(cmeans)[17+i]<-paste("z_",names(cmeans)[i+1])
  
}





