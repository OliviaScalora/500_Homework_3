#Homework 3
mydata <- read.csv("https://raw.githubusercontent.com/OliviaScalora/500_Homework_3/main/Logistic Regression Data.csv")
head(mydata)

#install.packages("packagename")
install.packages("aod")
install.packages("ggplot2")
install.packages("rms")
install.packages("gmodels")
#install.packages("boot")
install.packages("DAAG")
install.packages("ROCR")

#library(packagename)
library(aod)
library(ggplot2)
library(rms)
library(gmodels)
library(boot)
library(DAAG)
library(ROCR)
#Tabulations
summary(factor(mydata$DRINKING_D))
summary(factor(mydata$COLLISION_))
summary(factor(mydata$FATAL_OR_M))
summary(factor(mydata$OVERTURNED))
summary(factor(mydata$CELL_PHONE))
summary(factor(mydata$SPEEDING))
summary(factor(mydata$AGGRESSIVE))
summary(factor(mydata$DRIVER1617))
summary(factor(mydata$DRIVER65PLUS))

#Alternative way of tabulating (and obtaining proportions for each category)
DRINKING_D.tab <- table(mydata$DRINKING_D)
prop.table(DRINKING_D.tab)
COLLISION.tab <- table(mydata$COLLISION_)
prop.table(COLLISION.tab)
FATAL_OR_M.tab <- table(mydata$FATAL_OR_M)
prop.table(FATAL_OR_M.tab)
OVERTURNED.tab <- table(mydata$OVERTURNED)
prop.table(OVERTURNED.tab)
CELL_PHONE.tab <- table(mydata$CELL_PHONE)
prop.table(CELL_PHONE.tab)
SPEEDING.tab <- table(mydata$SPEEDING)
prop.table(SPEEDING.tab)
AGGRESSIVE.tab <- table(mydata$AGGRESSIVE)
prop.table(AGGRESSIVE.tab)
DRIVER1617.tab <- table(mydata$DRIVER1617)
prop.table(DRIVER1617.tab)
DRIVER65PLUS.tab <- table(mydata$DRIVER65PLUS)
prop.table(DRIVER65PLUS.tab)



#Cross-Tabulations
CrossTable(mydata$COLLISION_, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$FATAL_OR_M, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$OVERTURNED, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$CELL_PHONE, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$SPEEDING, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$AGGRESSIVE, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$DRIVER1617, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(mydata$DRIVER65PLUS, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)

#Means by group
tapply(mydata$PCTBACHMOR, mydata$DRINKING_D, mean) 
tapply(mydata$MEDHHINC, mydata$DRINKING_D, mean) 

#Independent samples t-test
t.test(mydata$PCTBACHMOR~mydata$DRINKING_D)
t.test(mydata$MEDHHINC~mydata$DRINKING_D)

#Logistic Regression
mylogit <- glm(DRINKING_D ~ FATAL_OR_M + OVERTURNED + CELL_PHONE + SPEEDING + AGGRESSIVE + DRIVER1617 + DRIVER65PLUS + PCTBACHMOR + MEDHHINC, data=mydata, family = "binomial")
summary(mylogit)

mylogit1 <- glm(DRINKING_D ~ FATAL_OR_M + OVERTURNED + CELL_PHONE + SPEEDING + AGGRESSIVE + DRIVER1617 + DRIVER65PLUS, data=mydata, family = "binomial")
summary(mylogit1)

exp(cbind(OR = coef(mylogit), confint(mylogit)))

OR <- exp(coef(mylogit))
CONFINT <- exp(confint(mylogit))

#Histogram of fitted values (predicted probabilities of y = 1)
fit <-mylogit$fitted.values
hist(fit)


#a is a matrix combining the vectors containing y and y-hat in matrix a; first variable is
#DRINKING_D, which is y; second variable is fit, which is y-hat

a <- cbind(mydata$DRINKING_D, fit)

#b is matrix a, just sorted by the variable fit
b <- a[order(a[,2]),]

#Calculating variable c which is 1 if y-hat (second column of matrix b) is greater
#than or equal to 0.05 and 0 otherwise.

#Other cut-offs can be used here!
c <- (b[,2] >= 0.06365151)

#Creating matrix d which merges matrixes b and c
d <- cbind(b,c)

#Let's label the columns of matrix d for easier reading

colnames(d) <- c("Observed.DRINKING_D","Probability.DRINKING_D","Prob.Above.Cutoff")

#Converting matrix to data frame
e=as.data.frame(d)

#Cross-tabulation
CrossTable(e$Prob.Above.Cutoff, e$Observed.DRINKING_D, prop.r=FALSE,prop.chisq=FALSE, chisq=FALSE,prop.t=FALSE)


#ROC Curve
#For more info, see: https://hopstat.wordpress.com/2014/12/19/a-small-introduction-to-the-rocr-package/

#Here, predictions are estimated probabilities (i.e., p or y-hat values)
#Also, labels are actual y-values


#a <- cbind(mydata$DRINKING_D, fit)
#From above, we see that matrix a has 2 columns: 
#1. The first one is mydata$DRINKING_D, which are actual 
#values of y (i.e., labels)
#2. The second one is fit, which are predicted, or fitted values
#of y (i.e., predictions)

#Let's make the names of the variables easy to understand
colnames(a) <- c("labels","predictions")
head(a)
roc <- as.data.frame(a)

pred <- prediction(roc$predictions, roc$labels)
#Below, tpr = true positive rate, another term for sensitivity
#fpr = false positive rate, or 1-specificity
roc.perf = performance(pred, measure = "tpr", x.measure="fpr")
plot(roc.perf)
abline(a=0,b=1)

#Optimal cut-point, if you want to weigh specificity and
#sensitivity equally.
#There are a couple ways to identify the optimal cut point.
#One is the so-called Youden Index, which identifies the cut-off
#point for which (sensitivity + specificity) is maximized.

#Another one, calculated using the code below, is the cut-off
#value for which the ROC curve has the minimum distance from
#the upper left corner of the graph, where specificity = 1 and
#sensitivity = 1. (This is just a different way of maximizing 
#specificity and sensitivity). This is where the 
#d = (x - 0)^2 + (y-1)^2
#in the code below comes in.

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
#This will print the optimal cut-off point and the corresponding
#specificity and sensitivity 
print(opt.cut(roc.perf, pred))

#Area under the curve
#Source: http://gim.unmc.edu/dxtests/roc3.htm
#The prediction accuracy of the model depends on how well the 
#model predicts 1 responses as 1's and 0 responses as 0's. 
#Accuracy is measured by the area under the ROC curve. An area 
#of 1 represents a perfect test (prediction); an area of .5 
#represents a worthless test (prediction). A rough guide for
#interpreting area under ROC Curves:
# .90-1 = excellent (A)
# .80-.90 = good    (B)
# .70-.80 = fair    (C)
# .60-.70 = poor    (D)
# .50-.60 = fail    (F)

#These might be somewhat conservative estimates, and there will
#be statisticians who will say that area > .7 is just fine.

auc.perf = performance(pred, measure ="auc")
auc.perf@y.values