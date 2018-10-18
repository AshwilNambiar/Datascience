setwd("C:\\xxxxx\\Ensembles\\Data")
hr=read.csv("hr.csv")
head(hr)
dim(hr)

colSums(is.na(hr))
str(hr)

unique(hr$sales)
#Renaming 'sales' column
names(hr)[names(hr)=="sales"]<-"Department"
unique(hr$Department)


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
library(gbm)
set.seed(1234)
control=trainControl(method="cv",number=10)
tune<-expand.grid(.n.trees=seq(70,300,10),.shrinkage=c(0.002,0.01,0.1),.interaction.depth=c(1,3,9),.n.minobsinnode=rep(3,14))

#Parameter Selections
mod=train(X_train,as.factor(y_train),method="gbm",trControl=control,tuneGrid=tune,verbose=F)
print(mod)


tune2<-expand.grid(.n.trees=300,.shrinkage=0.1,.interaction.depth=9,.n.minobsinnode=3)
#tune1<-data.frame(n.trees=300,shrinkage=0.1,interaction.depth=9,n.minobsinnode=3)

#Final Model
model=train(X_train,as.factor(y_train),method="gbm",tuneGrid=tune2,verbose=F)

#Accuracy
pred=predict(model,newdata=X_test,type="prob")
library(ROCR)

predicted=prediction(pred[,2],y_test)
auc=performance(predicted,"auc")
auc=unlist(slot(auc,"y.values"))
auc

#Feature Importance
summary(model)

imp=as.data.frame(as.matrix(summary(model)))

Featureimp=ggplot(imp,aes(x=var,y=rel.inf,fill=var))
Featureimp+geom_bar(stat="identity")+geom_text(label=imp$rel.inf,vjust=-1)


#PDP
plot(model$finalModel,i.var=3,col="blue")

