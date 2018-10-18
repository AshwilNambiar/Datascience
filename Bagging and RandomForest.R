setwd("C:\\xxxxx\\Ensembles\\Data")
hr=read.csv("hr.csv")
head(hr)
str(hr)
#Renaming 'sales' column
names(hr)[names(hr)=="sales"]<-"Department"
unique(hr$Department)
colSums(is.na(hr))

#Data Partition
index=sort(sample(nrow(hr),nrow(hr)*0.8))
train=hr[index,]
test=hr[-index,]

which(colnames(hr)=="left")
X_train=train[,-7]
y_train=train$left

X_test=test[,-7]
y_test=test$left

library(caret)
set.seed(1234)

#Bagging
control=trainControl(method="cv",number=10)
btree=train(X_train,as.factor(y_train),method="treebag",trControl=control,verbose=F,keepX=TRUE,coob=TRUE)

library(ROCR)
predb=predict(btree$finalModel,newdata=X_test,type="prob")
predicted=prediction(predb[,2],y_test)
auc=performance(predicted,"auc")
auc=unlist(slot(auc,"y.values"))
auc

#OOB Estimate
print(btree$finalModel)

#Feature Importance
bagimp=varImp(btree)
plot(bagimp)

#RandomForest
control=trainControl(method="cv",number=10)
tune=expand.grid(mtry=c(3,6,9))
rf=train(X_train,as.factor(y_train),method="rf",trControl=control,tuneGrid=tune,verbose=F)

predrf=predict(rf$finalModel,newdata=X_test,type="prob")
predicted=prediction(predrf[,2],y_test)
auc=performance(predicted,"auc")
auc=unlist(slot(auc,"y.values"))
auc

#OOB Estimate
print(rf$finalModel)

#Feature Importance
rfimp=varImp(rf)
plot(rfimp)