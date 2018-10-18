#Brand perception analysis using Logistic regression

#Call installed libraries
library(gains)
library(dplyr)
library(irr)
library(caret)
library(e1071)
library(car)
library(Matrix)

#Extract csv file
fulldata<-read.csv("BrandA_Analysis.csv")

summary(fulldata)
str(fulldata)
names(fulldata)
dim(fulldata)

#Take only columns related to Brand A

names(new_fulldata)
new_fulldata<-fulldata[,c("X23","X2","X9","X16","X30")]

#Using glm  

ifelse((new_fulldata$X23>=5),1,0)->new_fulldata$X23
head(new_fulldata)

mod1 <- glm(new_fulldata$X23 ~ X2+X9+X16+X30, data = new_fulldata,family="binomial")
summary(mod1)
?glm
table(new_fulldata$X2)
#Substitute values for X23
new_fulldata$X23<- ifelse(new_fulldata$X23<=4,0,1)
table(new_fulldata$X30)
table(new_fulldata$X16)
table(new_fulldata$X2)
table(new_fulldata$X9)

#checking 50% of X23 - Overall Perception of Brand A
new_fulldata%>%filter(X23<=4)%>%nrow()-> X23_lessthan5
X23_lessthan5/nrow(new_fulldata)  #More than 50%

#Checking respondents having good perception about brand A - score above 5
new_fulldata%>%filter(X23>=5)%>%nrow()-> X23_morethanor5
X23_morethanor5 #11954

#Probability of saying no to X2 - Use of farm grown products
new_fulldata%>%filter(X2==1)%>%nrow()-> X2_Yes
X2_Yes/nrow(new_fulldata) #P=0.7931
log(1/(1-0.7931)) #1.5755
X2_Yes

#Probability of saying no to X16 - Use of natural oils
new_fulldata%>%filter(X16==1)%>%nrow()-> X16_Yes
X16_Yes

#Probability of saying no to X9 - Use of natural oils
new_fulldata%>%filter(X9==1)%>%nrow()-> X9_Yes
X9_No

#Probability of saying no to X23 - Good perception
new_fulldata%>%filter(X23==1)%>%nrow()-> X23_Good
X23_Good

nrow(new_fulldata)
