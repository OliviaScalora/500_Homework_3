#Homework 3

#install.packages("packagename")
#install.packages("aod")
#install.packages("ggplot2")
#install.packages("rms")
#install.packages("gmodels")
#install.packages("boot")
#install.packages("DAAG")
#install.packages("ROCR")

#library(packagename)
library(aod)
library(ggplot2)
library(rms)
library(gmodels)
library(boot)
library(DAAG)
library(ROCR)
library(tidyr)
library(dplyr)
library(MASS)
library(rsq)
library(kableExtra)
library(tidyverse) #for ggplot
#library(sf) #for maps
#library(cowplot) #for plotgrid
library(classInt)#for jenks breaks
library(rgdal)
library(RColorBrewer)
library(broom)
library(maptools)
library(spdep)
library(spgwr)
library(rgeos)
library(ape)
library(tmap)
library(sp)
library(spatialreg)
library(kableExtra)
library(knitr)
#install.packages('DT')
library(DT)

#Step 1 -------------------------------------
mydata <- read.csv("https://raw.githubusercontent.com/OliviaScalora/500_Homework_3/main/Logistic Regression Data.csv")
head(mydata)

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

#Step 2 -------------------------------------
#2.a
#Alternative way of tabulating (and obtaining proportions for each category)
DRINKING_D.tab <- table(mydata$DRINKING_D)
prop.table(DRINKING_D.tab)
#proportion of crashes that involved a drunk driver

#        0           1 
#0.9426944   0.0573056 

#additional proportion table commands
#COLLISION.tab <- table(mydata$COLLISION_)
#prop.table(COLLISION.tab)
#FATAL_OR_M.tab <- table(mydata$FATAL_OR_M)
#prop.table(FATAL_OR_M.tab)
#OVERTURNED.tab <- table(mydata$OVERTURNED)
#prop.table(OVERTURNED.tab)
#CELL_PHONE.tab <- table(mydata$CELL_PHONE)
#prop.table(CELL_PHONE.tab)
#SPEEDING.tab <- table(mydata$SPEEDING)
#prop.table(SPEEDING.tab)
#AGGRESSIVE.tab <- table(mydata$AGGRESSIVE)
#prop.table(AGGRESSIVE.tab)
#DRIVER1617.tab <- table(mydata$DRIVER1617)
#prop.table(DRIVER1617.tab)
#DRIVER65PLUS.tab <- table(mydata$DRIVER65PLUS)
#prop.table(DRIVER65PLUS.tab)


#2.b 
#Cross-Tabulations
#i.
#CrossTable(mydata$COLLISION_, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
Cross_Fatal<-CrossTable(mydata$FATAL_OR_M, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
Cross_Fatal_tbl<-
  cbind(as.data.frame(Cross_Fatal$t)%>%filter(x==1)%>%rename(N=Freq),
        as.data.frame(Cross_Fatal[["prop.tbl"]])%>%filter(x==1)%>%rename('percentage'=Freq,x1=x,y1=y))%>%
  dplyr::select(-x1,-y1)%>%
  mutate(Variable='FATAL_OR_M')
         

#.ii
Cross_Over<- CrossTable(mydata$OVERTURNED, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
Cross_Over_tbl<-
  cbind(as.data.frame(Cross_Over$t)%>%filter(x==1)%>%rename(N=Freq),
      as.data.frame(Cross_Over[["prop.tbl"]])%>%filter(x==1)%>%rename('percentage'=Freq,x1=x,y1=y))%>%
  dplyr::select(-x1,-y1)%>%
  mutate(Variable='OVERTURNED')

Cross_Cell<-CrossTable(mydata$CELL_PHONE, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
Cross_Cell_tbl<-
  cbind(as.data.frame(Cross_Cell$t)%>%filter(x==1)%>%rename(N=Freq),
        as.data.frame(Cross_Cell[["prop.tbl"]])%>%filter(x==1)%>%rename('percentage'=Freq,x1=x,y1=y))%>%
  dplyr::select(-x1,-y1)%>%
  mutate(Variable='CELL_PHONE')

Cross_Speed<-CrossTable(mydata$SPEEDING, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
Cross_Speed_tbl<-
  cbind(as.data.frame(Cross_Speed$t)%>%filter(x==1)%>%rename(N=Freq),
        as.data.frame(Cross_Speed[["prop.tbl"]])%>%filter(x==1)%>%rename('percentage'=Freq,x1=x,y1=y))%>%
  dplyr::select(-x1,-y1)%>%
  mutate(Variable='SPEEDING')

Cross_Agg<-CrossTable(mydata$AGGRESSIVE, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
Cross_Agg_tbl<-
  cbind(as.data.frame(Cross_Agg$t)%>%filter(x==1)%>%rename(N=Freq),
        as.data.frame(Cross_Agg[["prop.tbl"]])%>%filter(x==1)%>%rename('percentage'=Freq,x1=x,y1=y))%>%
  dplyr::select(-x1,-y1)%>%
  mutate(Variable='AGGRESSIVE')

Cross_1617<-CrossTable(mydata$DRIVER1617, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
Cross_1617_tbl<-
  cbind(as.data.frame(Cross_1617$t)%>%filter(x==1)%>%rename(N=Freq),
        as.data.frame(Cross_1617[["prop.tbl"]])%>%filter(x==1)%>%rename('percentage'=Freq,x1=x,y1=y))%>%
          dplyr::select(-x1,-y1)%>%
          mutate(Variable='DRIVER1617')

Cross_65P<-CrossTable(mydata$DRIVER65PLUS, mydata$DRINKING_D, prop.r=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
Cross_65P_tbl<-
  cbind(as.data.frame(Cross_65P$t)%>%filter(x==1)%>%rename(N=Freq),
        as.data.frame(Cross_65P[["prop.tbl"]])%>%filter(x==1)%>%rename('percentage'=Freq,x1=x,y1=y))%>%
  dplyr::select(-x1,-y1)%>%
  mutate(Variable='DRIVER65PLUS')

# Putting table together - there might be a simpler way of doing this but this ends up working
Cross_Joined<-rbind(Cross_Fatal_tbl,Cross_Over_tbl,Cross_Cell_tbl,Cross_Speed_tbl,Cross_Agg_tbl,Cross_1617_tbl,Cross_65P_tbl)

CrossTablex<- Cross_Joined %>%spread(x, Variable)%>%filter(y==0)%>%rename(N_1=N,'percentage_1' = 'percentage')
CrossTabley<- Cross_Joined %>%spread(x, Variable)%>%filter(y==1)%>%
  dplyr::select(-y,-'1')%>%
  cbind(CrossTablex)%>%dplyr::select(-y)%>%rename(Variable = '1')
CrossTable<-CrossTabley%>%mutate(Total_N = N+N_1,
         '%' = round((percentage *100),2),
         '%_1' = round((percentage_1 *100),2))%>% dplyr::select(-percentage, -percentage_1)%>%
  arrange(factor(Variable, levels = c('FATAL_OR_M', 'OVERTURNED', 
                                      'CELL_PHONE', 'SPEEDING', 
                                      'AGGRESSIVE', 'DRIVER1617', 
                                      'DRIVER65PLUS')))%>%
  bind_cols(data.frame(Description = c(
  "Crash resulted in fatality or major injury",
  "Crash involved an overturned vehicle",
  "Driver was using cell phone",
  "Crash involved speeding vat",
  "Crash involved aggressive driving",
  "Crash involved at least one driver who was 16 or 17 years old",
  "Crash involved at least one driver who was at least 65 years old")))%>%
  bind_cols(data.frame('X2 p.value' = c(
    Cross_Fatal[["chisq"]][["p.value"]],
    Cross_Over[["chisq"]][["p.value"]],
    Cross_Cell[["chisq"]][["p.value"]],
    Cross_Speed[["chisq"]][["p.value"]],
    Cross_Agg[["chisq"]][["p.value"]],
    Cross_1617[["chisq"]][["p.value"]],
    Cross_65P[["chisq"]][["p.value"]])))

#iii.
#Output Table
CrossTable[, c(3,7,1,5,2,6,4,8)]%>%
  kable(col.names = c("Variable", "","N","%","N","%","N", "X2 P.value"),
                                     align = c('r','l','c','c','c','c','c','c'))%>%
  kable_material()%>%
  add_header_above(c("","","No Alcohol Involved\n(Drinkind_D = 0)" = 2, "Alcohol Involved\n(Drinkind_D = 1)" = 2,"Total", ""),
                                                align = c('r','c'))%>%
  column_spec(1,bold = TRUE)%>%
  column_spec(5,border_left = TRUE)%>%
  column_spec(c(3:4),background = '#f1f1f1')%>%
  column_spec(c(5:6),background = '#e1e1e1')
  
#iv.
#Chi-Square test 
#H0 - the proportion of fatalities for crashes that involve drunk drivers is the same as the proportion
#of fatalities for crashes that dont involve drunk drivers
#Ha - the proportion of fatalities for crashes that involve drunk drivers is different than 
# the proportion of fatalities for crashes that don't involve drunk drivers.
#added X2 pvalue column to table above ^

#2.c
#i.
#Means by group
meanbach<-as.data.frame(t(tapply(mydata$PCTBACHMOR, mydata$DRINKING_D, mean)))%>%mutate(Variable= 'PCTBACHMOR')%>%
  rename('mean.0'='0', 'mean.1'='1')
# 16.56986 16.61173 

meanHHinc<-as.data.frame(t(tapply(mydata$MEDHHINC, mydata$DRINKING_D, mean)))%>%mutate(Variable= 'MEDHHINC')%>%
  rename('mean.0'='0', 'mean.1'='1')
# 31483.05 31998.75 

#SD by group
sdbach<-as.data.frame(t(tapply(mydata$PCTBACHMOR, mydata$DRINKING_D, sd)))%>%mutate(Variable= 'PCTBACHMOR')%>%
  rename('sd.0'='0', 'sd.1'='1')
# 18.21426 18.72091

sdHHinc<-as.data.frame(t(tapply(mydata$MEDHHINC, mydata$DRINKING_D, sd)))%>%mutate(Variable= 'MEDHHINC')%>%
  rename('sd.0'='0', 'sd.1'='1')

sdHHinc<-sdHHinc%>%spread(sd,x)
# 16930.1 17810.5

#save pvalues for table (step iii). 
ttest_pvalues<-as.data.frame(c((t.test(mydata$PCTBACHMOR~mydata$DRINKING_D)$p.value),
                               (t.test(mydata$MEDHHINC~mydata$DRINKING_D)$p.value)))

#ii.
#Mean and SD table
MeanSD_tbl<- merge(x=(merge(x=meanHHinc, y=sdHHinc, all= TRUE)),
                   y=(merge(x=meanbach, y=sdbach,all=TRUE)),
                   all=TRUE)%>%arrange(desc(Variable))%>%
  bind_cols(data.frame(Description = c(
    "% with a bachelor's degree or more",
    "Median household income")))%>%
  bind_cols(ttest_pvalues)

#output for markdown
MeanSD_tbl[, c(1,6,2,4,3,5,7)]%>%
  kable(col.names = c("Variable","","Mean","SD","Mean","SD","t-test p-value"),
                                     align = c('r','l','c','c','c','c','c'))%>%
  kable_material()%>%
  add_header_above(c("","","No Alcohol Involved\n(Drinkind_D = 0)" = 2,
                     "Alcohol Involved\n(Drinkind_D = 1)" = 2,""),
                   align = c('r','c'))%>%
  column_spec(1,bold = TRUE)%>%
  column_spec(5,border_left = TRUE)%>%
  column_spec(c(3:4),background = '#f1f1f1')%>%
  column_spec(c(5:6),background = '#e1e1e1')

#iii.
#added t-test p-value to table above^^


#2.d Using the instructions from Assignment 1, examine the Pearson correlations
#between all the predictors, (both binary and continuous). Is there evidence
# of severe multicollinearity here?

pred_var <- mydata%>%dplyr::select(DRINKING_D, FATAL_OR_M, OVERTURNED, CELL_PHONE, SPEEDING,
                                   AGGRESSIVE,DRIVER1617,DRIVER65PLUS,PCTBACHMOR,MEDHHINC)
pcorr <- cor(pred_var, method="pearson")

#correlation matrix for markdown
pcorr%>%kable()%>%kable_material()%>%
  column_spec(1,bold = TRUE)

#Observe that there is not severe multicollinearity (i.e., no correlations where
# r>.8 or r<-.8), so we can include all four predictors in the regression.

#Step 3 -------------------------------------
#everything below was existing code -Olivia


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
c <- (b[,2] >= 0.05)

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