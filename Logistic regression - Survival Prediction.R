#"titanic" survival prediction using logistic regression

install.packages("titanic")
install.packages("lava")
library(titanic)
library(lava)

#library(help=titanic)

dim(titanic_train)
dim(titanic_test)

titanic_full<- titanic_train
tf<- titanic_full

summary(titanic_full)
str(titanic_full)

substr(trim(titanic_full$Cabin),1,1)-> CheckNA
CheckNA

table(tf$Cabin)

#Calculating mean age based on Pclass
tf%>%filter(!is.na(Age),Pclass==1)%>%summarise(mean(Age))-> NonMissing1
tf%>%filter(!is.na(Age),Pclass==2)%>%summarise(mean(Age))-> NonMissing2
tf%>%filter(!is.na(Age),Pclass==3)%>%summarise(mean(Age))-> NonMissing3

NonMissing1 #38.23
NonMissing2 #29.87
NonMissing3 #25.14

#Imputing data for missing Age
tf[is.na(tf$Age)&tf$Pclass==3,"Age"] <- 25.14
tf[is.na(tf$Age)&tf$Pclass==2,"Age"] <- 29.87
tf[is.na(tf$Age)&tf$Pclass==1,"Age"] <- 38.23


str(tf)
new_tf<-tf[,-c(1,4,9,11)] #Removing insignificant attributes
str(new_tf)

#Converting character data to factors for model data preparation
new_tf$Sex<-as.factor(new_tf$Sex)
new_tf$Embarked<-as.factor(new_tf$Embarked)

#Removing "" values from Embarked
new_tf[!new_tf$Embarked=="",]->new_tf1

new_tf1

#Initial Model creation with all factors
mod<- glm(new_tf1$Survived~., data=new_tf1,family="binomial")
summary(mod)

#Modifying initial model to include only significant factors
mod1<- glm(new_tf1$Survived~Pclass+Sex+Age+SibSp,data=new_tf1,family="binomial" )
summary(mod1)

#####################################

set.seed(200)

index<-sample(nrow(new_tf1),0.7*nrow(new_tf1),replace=F)
length(index)
train<- new_tf1[index,]
test<- new_tf1[-index,]
names(train)

######################################

mod2<- glm(train$Survived~., data=train[,-1],family="binomial")
summary(mod2)

#####################################################

#Final model
mod3<- glm(train$Survived~Pclass+Sex+Age+SibSp, data=train[,-1],family="binomial")
summary(mod3)

##############################################

#FInding predictions/probabilities
pred<- predict(mod3,type="response",newdata=test)
head(pred)


table(tf$Survived)/nrow(tf)

pred_binary <- ifelse(pred>=0.3838,1,0)  ###.38% contains whole of '1' values , means Survived=1
pred_binary

test[!is.na(test$Survived),]->test1
summary(test1)

confusionMatrix(as.factor(pred_binary),as.factor(test1$Survived),positive="1")

gains(test1$Survived,predict(mod3,type="response",newdata=test1),groups=10)


##################################################

#Prediction for survival chance
pred<- predict(mod3,type="response",newdata=titanic_test)

