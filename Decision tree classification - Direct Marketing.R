##Classification Trees using rpart()
library(dplyr)
library(irr)
library(rpart) #contains relevant functions for classification as well as regression decision trees
library(caret)
#Tree plotting
#install.packages("rattle")
library(rattle) #Contains multiple utils, dplyr, ggplot etc
#install.packages("rpart.plot")
library(rpart.plot)
library(RColorBrewer)

setwd("C:\\xxxxx\Decison Trees")
dm<-read.csv("dm.csv")
summary(dm)

dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm #Creating new factor variable for AmountSpent
names(dm)
dm<- dm[,-10] #removing Amount spent

#Data Prep - Handle missing value
summary(dm$History)
dm$History1<-ifelse(is.na(dm$History),"Missing",as.character(dm$History))
dm$History1<-as.factor(dm$History1)
summary(dm$History1)


str(dm) #Checking which attributes can be converted to factors and then converting
dm$Children<-as.factor(dm$Children)
dm$Catalogs<-as.factor(dm$Catalogs)


names(dm)
dm<-dm[,-8]
mod<-rpart(Target~.,data=dm,
           control=rpart.control(maxdepth =5),method="class",parms=list(split="gini"))
fancyRpartPlot(mod, cex = 0.7) #rattle package, cex is used to control the font sizes
fancyRpartPlot(mod)
mod #to see the actual rules
class(mod)

#there are just three depths (horizintal lines) because there is a default cp value of 0.01
#When 0s have high percentage after the split the color is green and the topmost number inside box is 0
#When 1s have high percentage after the split the color is blue and the topmost number inside box is 1
#%age is the size of the parent/child node as a % of the overall rows of data
#method = 'class' for a classification tree and 'anova' for a regression tree
#split must decrease the overall lack of fit by a factor of 0.01 (cost complexity factor) before being attempted
#gini is the default split method in R, so you can also not specify it in the above code

#let us just run by cp =0.01 and see the result
mod<-rpart(Target~.,data=dm,
           control=rpart.control(cp=0.01),method="class",parms=list(split="gini"))
fancyRpartPlot(mod)
#there are just 3 depths which means cp is overriding the maxdepth (since I gave maxdepth = 5 and it still created 3 horizontal lines/splits)

mod<-rpart(Target~.,data=dm,
           control=rpart.control(cp=0.002),method="class",parms=list(split="gini"))
fancyRpartPlot(mod)
#there are 8 depths
#cp can be imagined as how much the missclassification rate is decreasing by making the split
#So when cp values are less, even for small values of missclassification rate decrease there is a split

mod<-rpart(Target~.,data=dm,
           control=rpart.control(cp=0.002, maxdepth=5),method="class",parms=list(split="gini"))
fancyRpartPlot(mod)
#there are 5 depths

mod1<-rpart(Target~.,data=dm,
           control=rpart.control(cp=0.01, maxdepth=5),method="class",parms=list(split="gini"))
fancyRpartPlot(mod1)
#again back to three splits because of cp value. For a cp value of 0.01 it doesn't go beyond 3 splits

mod<-rpart(Target~.,data=dm,
           control=rpart.control(cp=0.002, maxdepth=5),method="class",parms=list(split="gini"))
fancyRpartPlot(mod)
#we observe small blocks below 3% being split. Apply minsplit in the next code to specify minimum block size
#since we have 1000 records we will specify that 100 or block size should be atleast 10% to be split
#minsplit specifies the minimum size of the node that can be split (parent node) and not the node size after split 
#to specify a limit on the number of records in the terminal node minsplit can be used (parent becomes terminal when it can no more be split)
mod<-rpart(Target~.,data=dm[,-9],
           control=rpart.control(minsplit=100,cp=0.002, maxdepth=5),method="class",parms=list(split="gini"))
fancyRpartPlot(mod)
#the result contains only blocks that are greater than 10% undergoing splitting

#CHECK FOR OVERFITTING
mod<-rpart(Target~.,data=dm[,-9],
           control=rpart.control(minsplit=100,cp=0.002, maxdepth=5),method="class",parms=list(split="gini"))
fancyRpartPlot(mod)
printcp(mod)
plotcp(mod)
#misslcassification error estimation to understand printcp output
table(dm$Target)
predict(mod, type="class")
sum(predict(mod, type="class")!=dm$Target) #111 are missclassified
sum(predict(mod, type="class")!=dm$Target)/nrow(dm) #0.111 percent are missclassified
y <- sum(predict(mod, type="class")!=dm$Target)/nrow(dm) #y is missclassification error
#Relative error is the ratio of missclassified error to the root node error which is 399/1000 = 0.399
y/0.399 #since root node error is constant across splits in printcp output we can as well consider y as relative error
#relative error is for training data whereas x error is for validation data
#R uses 10-fold cross validation method. Defintion of this is in the end of this code (which is why the calculated relative error above has a tiny tiny difference with the one in printcp output)
#If xerror(cross validated error) is continuously decreasing and therefore there is no overfitting
#If xerror decreases and increases, that means the model starts overfitting at that point
#for different runs of same above code the errors will be different because of different cross validation data chosen by R
#run it a few times and ensure that xerror is continuously decreasing
#so the model is good and can be finalized
#nsplit represents the number of parent nodes that have been split. 
#cp is equal to difference in relative error by splitting that node. In the printcp output check the difference when the nsplit has 2 consecutive values
#Although there are six splits printcp prints only 4 as R output limits them. So only when consecutive outputs are present cp is the difference between relative values

#suppose we had a overfitting case like the one below
mod<-rpart(Target~.,data=dm[,-9],
           control=rpart.control(minsplit=50,cp=0.002),method="class",parms=list(split="gini"))
fancyRpartPlot(mod)
printcp(mod)
plotcp(mod)
#xerror decreases and then increases (not always)
#not always because every time we run the above query we will get different xerrors due to different-
#-cross validation data R choses by itself
#rerun the above code to see different xerrors
#We can also use the standard thumbrule to cut-off cp. That is take cp in plotcp for which xerror is less than minimum(xerror)+xstd and prune the tree using prune command
#Note that "cp value in printcp and rpart" is different from "cp value in plot cp and prune"
#this is because:
#cp0 <- p.rpart[, 1L] which is printcp's cp (in R's source code)
#cp <- sqrt(cp0 * c(Inf, cp0[-length(cp0)])) which is plotcp's cp (in R's source code) - Plotcp cp = geometric mean (princtcp cp)
#we need to consider only plotcp's cp for taking cut-off. The horizontal line in plot cp corresponds to minimum(xerror)+xstd
#The y axis values of relative error for the horizontal line is calculated as the relative error corresponding to xerror = xerror+xstd (therefore it always appears inflated in y axis)
#now take the cp whose xerror is just below it. 0.035 and 0.0039 are close
#then we will apply that as below
mod1<-prune(mod,cp= 0.035)
fancyRpartPlot(mod1)
plotcp(mod1)
printcp(mod1)

#another way of plotting but difficult to interpret
#plot(mod, margin=0.1, main="Classification Tree for Direct Marketing")
#text(mod, use.n=TRUE, all=TRUE, cex=.7)

#Rules derivation
mod1
node4
#if history1={Low,Medium,Missing} and Salary < 58650, then 0 (bad) 
#If History1={low,medium,missing}.... excel sheet

#Confusion Matrix
actual<-dm$Target
predicted<-predict(mod,type = "class")

head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)
head(predicted)

predicted<-as.factor(predicted)
actual<-as.factor(actual)
#actual<-as.numeric(actual)
confusionMatrix(predicted,actual,positive="1")

mod1$variable.importance

#kappa metric
#kappa2(data.frame(actual,predicted))

#ROC curve analysis
install.packages("ROCR")
library(ROCR)
pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)

#10 fold cross validation?
#In k-fold cross-validation, the original sample is randomly partitioned into k equal size subsamples.- 
#-Of the k subsamples, a single subsample is retained as the validation data for testing the model, -
#-and the remaining k-1 subsamples are used as training data. The cross-validation process is then -
#-repeated k times (the folds), with each of the k subsamples used exactly once as the validation data.- 
#-The k results from the folds can then be averaged (or otherwise combined) to produce a single estimation.- 
#-The advantage of this method is that all observations are used for both training and validation, -
#-and each observation is used for validation exactly once.
#Summarizing:
#Step A. It divides data into 10 parts. 
#Step B. Build model on 9 parts(train) and cross validate on remaining 1 part.
#Step B above is performed on 10 combinations (10 different sets of 9 parts and 1 part)
#Models can be averaged, but in decision tree since we want best decision tree the best one model out of 10-
#- with least cross validation error is displayed

