#install.packages("gains")
#install.packages("irr")
#install.packages("e1071")

library(gains)#for gains,pred
library(dplyr) #for all data manipulations
library(irr) #for kappa2
library(caret) #for confusionMatrix,boxcox,knn,cars
library(e1071) # for cmeans,clustering - naive bayes, random forest, bagged etc

######################################
dm<- read.csv("dm.csv")
dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm
dm$Target

names(dm)
dm[,-10]->dm
summary(dm)

dm$History1<-ifelse(is.na(dm$History),"Missing",as.character(dm$History))
dm$History1<-as.factor(dm$History1)
dm$History1
dm<-dm[,-8]
summary(dm)

#################################

set.seed(200)
index<-sample(nrow(dm),0.7*nrow(dm),replace=F)
index
length(index)

######################################

train<- dm[index,]
test<- dm[-index,]

head(train)
head(test)

names(train)
mod<- glm(train$Target~., data=train[,-9],family="binomial")
summary(mod)
summary(train$Age)

#Build refined model
mod1<-glm(formula = Target~ Age+Location+Salary+Children+Catalogs+History1, family="binomial",data=train)
summary(mod1)

train$AgeYoung_d<-ifelse(train$Age=="Young",1,0)
train$History1_med_d<-ifelse(train$History1=="Medium",1,0)
test$AgeYoung_d<-ifelse(test$Age=="Young",1,0)
test$History1_med_d<-ifelse(test$History1=="Medium",1,0)

#Refined model
mod2<-glm(formula = Target~ AgeYoung_d+Location+Salary+Children+Catalogs+History1_med_d, family="binomial",data=train)
summary(mod2)

#Now do predictions using test data
pred<- predict(mod2,type="response",newdata=test)
pred

#Percentage of good /bad customers in original data?
table(dm$Target)
table(dm$Target)/nrow(dm) 

#Grouping predictions to good/bad as per  good customer rate in overall data
pred_binary <- ifelse(pred>=0.39,1,0) #0.39 is % of good customer rate in original data
table(pred_binary)

#To check accuracy of model
confusionMatrix(as.factor(pred_binary),as.factor(test$Target),positive="1")

#Take a vector of actual responses and a vector of predictions and construct gains table to evaluate the predictions
gains(test$Target,predict(mod2,type="response",newdata=test),groups=10)
#So with 210 records(70%) itself, we are able to capture almost all(99%) good customers

#Check deciles of probability
quantile(pred, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = TRUE, type = 7)

