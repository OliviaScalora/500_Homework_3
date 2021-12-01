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
DRINKING_D.tab%>%mutate(Count = Freq)%>%
  kable(format = "html", align = "ll", caption = "Drunk Driving Counts")%>%kable_paper()%>%kable_styling(full_width=F) 
prop.table(DRINKING_D.tab)%>%kable(format = "html", align = "ll", caption = "Drunk Driving Proportion")%>%kable_material()%>%kable_styling(full_width=F)
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

  ## 3.a.i ---- 
#Logistic Regression
mylogit <- glm(DRINKING_D ~ FATAL_OR_M + OVERTURNED + CELL_PHONE + SPEEDING + AGGRESSIVE + DRIVER1617 + DRIVER65PLUS + PCTBACHMOR + MEDHHINC, data=mydata, family = "binomial")
  
  ## 3.a.ii ---- 
# SAVING LOGSITIC REGRESSION OUTPUT AS LOGITOUTPUT
logitoutput<-summary(mylogit)

# SAVING COEFFS ESTIMATES, STANDARD ERROR, Z-VALUES, AND P-VALUES AS LOGITCOEFFS
logitcoeffs<-logitoutput$coefficients
logitcoeffs

# SAVING ODDS RATIONS AND 95% CONFIDENCE INTERVALS FOR THE ODDS RATIOS AS OR_CI
or_ci<- exp(cbind(OR=coef(mylogit), confint(mylogit)))

# APPENDING COEFFS WITH ODDS RATIOS AND CONFIDENCE INTERVALS
finallogitoutput<- cbind(logitcoeffs, or_ci)
finallogitoutput

# MAKING IT PRETTY

finallogitoutput%>%kable(format = "html", align = "rlllllll", caption = "Logit Results")%>%
  kable_material("hover")%>%kable_styling(full_width=F)

  ## 3.a.iii ---- 

#### bringing in iterate thresholds fn 
# Iterate Thresholds Chapter 6, 7 (left in Chapters)
iterateThresholds <- function(data, observedClass, predictedProbs, group) {
  #This function takes as its inputs, a data frame with an observed binomial class (1 or 0); a vector of predicted #probabilities; and optionally a group indicator like race. It returns accuracy plus counts and rates of confusion matrix #outcomes. It's a bit verbose because of the if (missing(group)). I don't know another way to make an optional parameter.
  observedClass <- enquo(observedClass)
  predictedProbs <- enquo(predictedProbs)
  group <- enquo(group)
  x = .01
  all_prediction <- data.frame()
  
  if (missing(group)) {
    
    while (x <= 1) {
      this_prediction <- data.frame()
      
      this_prediction <-
        data %>%
        mutate(predclass = ifelse(!!predictedProbs > x, 1,0)) %>%
        count(predclass, !!observedClass) %>%
        summarize(Count_TN = sum(n[predclass==0 & !!observedClass==0]),
                  Count_TP = sum(n[predclass==1 & !!observedClass==1]),
                  Count_FN = sum(n[predclass==0 & !!observedClass==1]),
                  Count_FP = sum(n[predclass==1 & !!observedClass==0]),
                  Rate_TP = Count_TP / (Count_TP + Count_FN),
                  Rate_FP = Count_FP / (Count_FP + Count_TN),
                  Rate_FN = Count_FN / (Count_FN + Count_TP),
                  Rate_TN = Count_TN / (Count_TN + Count_FP),
                  Sensitivity = Count_TP / (Count_TP + Count_FN),
                  Specificity = Count_TN / (Count_TN + Count_FP),
                  MissClass_Rate = (Count_FP + Count_FN)/(Count_FP + Count_FN + Count_TP + Count_TN), 
                  Accuracy = (Count_TP + Count_TN) / 
                    (Count_TP + Count_TN + Count_FN + Count_FP)) %>%
        mutate(Threshold = round(x,2))
      
      all_prediction <- rbind(all_prediction,this_prediction)
      x <- x + .01
    }
    return(all_prediction)
  }
  else if (!missing(group)) { 
    while (x <= 1) {
      this_prediction <- data.frame()
      
      this_prediction <-
        data %>%
        mutate(predclass = ifelse(!!predictedProbs > x, 1,0)) %>%
        group_by(!!group) %>%
        count(predclass, !!observedClass) %>%
        summarize(Count_TN = sum(n[predclass==0 & !!observedClass==0]),
                  Count_TP = sum(n[predclass==1 & !!observedClass==1]),
                  Count_FN = sum(n[predclass==0 & !!observedClass==1]),
                  Count_FP = sum(n[predclass==1 & !!observedClass==0]),
                  Rate_TP = Count_TP / (Count_TP + Count_FN),
                  Rate_FP = Count_FP / (Count_FP + Count_TN),
                  Rate_FN = Count_FN / (Count_FN + Count_TP),
                  Rate_TN = Count_TN / (Count_TN + Count_FP),
                  Sensitivity = Count_TP / (Count_TP + Count_FN),
                  Specificity = Count_TN / (Count_TN + Count_FP),
                  MissClass_Rate = (Count_FP + Count_FN)/(Count_FP + Count_FN + Count_TP + Count_TN),
                  Accuracy = (Count_TP + Count_TN) / 
                    (Count_TP + Count_TN + Count_FN + Count_FP)) %>%
        mutate(Threshold = round(x,2))
      
      all_prediction <- rbind(all_prediction,this_prediction)
      x <- x + .01
    }
    return(all_prediction)
  }
}

#prepping
fit <-mylogit$fitted.values

#a is a matrix combining the vectors containing y and y-hat in matrix a; first variable is
#DRINKING_D, which is y; second variable is fit, which is y-hat

a <- cbind(mydata$DRINKING_D, fit)

#b is matrix a, just sorted by the variable fit
b <- a[order(a[,2]),]
b=as.data.frame(b)
colnames(b) <- c("Observed.DRINKING_D","Probability.DRINKING_D")

    #### Don't Need This Stuff, that is folded ----
    
    #Calculating variable c which is 1 if y-hat (second column of matrix b) is greater
    #than or equal to 0.05 and 0 otherwise.
    
    #Other cut-offs can be used here!
    c <- (b[,2] == 0.02 | b[,2] == 0.03|b[,2] == 0.05|(b[,2] > 0.06 & b[,2] < 0.11)| b[,2] == 0.15 | b[,2] == 0.2 | b[,2] == 0.5)
    
    c2<-(b[,2] == 0.02)
    c3<-(b[,2] == 0.07)
    
    #Creating matrix d which merges matrixes b and c
    d2 <- cbind(b,c2)
    d3 <- cbind(b,c3)
    
    #Let's label the columns of matrix d for easier reading
    colnames(d3) <- c("Observed.DRINKING_D","Probability.DRINKING_D","Prob.Above.Cutoff")
    
    #Converting matrix to data frame
    e3=as.data.frame(d3)
    e<-e%>%dplyr::mutate("f0.02" = ifelse(Probability.DRINKING_D >= 0.02, 1, 0), 
                        "0.03" = ifelse(Probability.DRINKING_D >= 0.03, 1, 0), 
                        "0.05" = ifelse(Probability.DRINKING_D >= 0.05, 1, 0), 
                        "0.07" = ifelse(Probability.DRINKING_D >= 0.07, 1, 0), 
                        "0.08" = ifelse(Probability.DRINKING_D >= 0.08, 1, 0),
                        '0.09' = ifelse(Probability.DRINKING_D >= 0.09, 1, 0), 
                        '0.10' = ifelse(Probability.DRINKING_D >= 0.10, 1, 0), 
                        '0.15' = ifelse(Probability.DRINKING_D >= 0.15, 1, 0), 
                        '0.20'= ifelse(Probability.DRINKING_D >= 0.20, 1, 0), 
                        '0.50' = ifelse(Probability.DRINKING_D >= 0.50, 1, 0))
    
    TP <- e%>%mutate(Count_TN = sum(n["f0.02"==0 & !!"Observed.DRINKING_D"==0]))
    ,
                Count_TP = sum(n["0.02"==1 & !!"Observed.DRINKING_D"==1]),
                Count_FN = sum(n["0.02"==0 & !!"Observed.DRINKING_D"==1]),
                Count_FP = sum(n["0.02"==1 & !!"Observed.DRINKING_D"==0]))
    table <- data.frame(TP = )
    
    
    e%>%kable()%>%kable_material_dark()
    
    #Cross-tabulation
    CrossTable(e3$Prob.Above.Cutoff, e$Observed.DRINKING_D, prop.r=FALSE,prop.chisq=FALSE, chisq=FALSE,prop.t=FALSE)
    
    37Sensitivity = Count_TP / (Count_TP + Count_FP),
    Specificity = Count_TN / (Count_TN + Count_FN),
    MissClass_Rate = (Count_FP + Count_FN)/(Count_FP + Count_FN + Count_TP + Count_TN), 
# ----

whichThreshold <- 
  iterateThresholds(
    data=b, observedClass = Observed.DRINKING_D, predictedProbs = Probability.DRINKING_D)


# looking for these threshes:

ourthresh <- whichThreshold %>%filter(Threshold == 0.02 | Threshold == 0.03|Threshold == 0.05|(Threshold > 0.06 & Threshold < 0.11)| Threshold == 0.15 | Threshold == 0.2 | Threshold == 0.5)%>%
  dplyr::select(Threshold, Sensitivity, Specificity, MissClass_Rate)

ourthresh%>%kable(format = "html", align = "llll", caption = "Goodness of Fit Metrics",
                  col.names = c("Cut Off Value", "Sensitivity", "Specificity", "Missclassification Rate"))%>%
  kable_material("hover")%>%kable_styling(full_width=F)%>% 
  row_spec(10, bold = T,background = "#f0d560")





  ## 3.a.iv-v ---- 
#ROC Curve 
#For more info, see: https://hopstat.wordpress.com/2014/12/19/a-small-introduction-to-the-rocr-package/

#Here, predictions are estimated probabilities (i.e., p or y-hat values)
#Also, labels are actual y-values


a <- cbind(mydata$DRINKING_D, fit)

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

#Optimal cut-point:
  #if you want to weigh specificity and sensitivity equally.
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

opt.cut=  function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

  #This will print the optimal cut-off point and the corresponding
  #specificity and sensitivity 

cut.ind = function(per, pred){mapply(FUN=function(x, y, p){
  d = (x - 0)^2 + (y-1)^2
  ind = which(d == min(d))
  c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
    cutoff = p[[ind]])
})}

opt.cut2(roc.perf, pred)

print(opt.cut(roc.perf, pred))%>%kable(format = "html", align = "ll", caption = "Optimal Cutoff")%>%
  kable_material("hover")%>%kable_styling(full_width=F)%>%row_spec(3, bold=TRUE, background = "#f0d560")

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
print(auc.perf@y.values)%>%kable(col.names= c("Area Under the Curve"), format = "html", align = "c")%>%kable_material()%>%kable_styling(full_width=F, position = "center")



#Step 3 ROUND TWO  -------------------------------------

## 3.a.i ROUND TWO ---- 
#Logistic Regression
mylogit2 <- glm(DRINKING_D ~ FATAL_OR_M + OVERTURNED + CELL_PHONE + SPEEDING + AGGRESSIVE + DRIVER1617 + DRIVER65PLUS, data=mydata, family = "binomial")

## 3.a.ii ROUND TWO---- 
# SAVING LOGSITIC REGRESSION OUTPUT AS LOGITOUTPUT
logitoutput2<-summary(mylogit2)

# SAVING COEFFS ESTIMATES, STANDARD ERROR, Z-VALUES, AND P-VALUES AS LOGITCOEFFS
logitcoeffs2<-logitoutput2$coefficients
logitcoeffs2

# SAVING ODDS RATIONS AND 95% CONFIDENCE INTERVALS FOR THE ODDS RATIOS AS OR_CI
or_ci2<- exp(cbind(OR=coef(mylogit2), confint(mylogit2)))

# APPENDING COEFFS WITH ODDS RATIOS AND CONFIDENCE INTERVALS
finallogitoutput2<- cbind(logitcoeffs2, or_ci2)
finallogitoutput2

# MAKING IT PRETTY

finallogitoutput2%>%kable(format = "html", align = "rlllllll", caption = "Logit Results: Round Two")%>%
  kable_paper("hover")%>%kable_styling(full_width=F)

## Final Step AIC -------------------------------------------


aic <- AIC(mylogit, mylogit2)%>%as.data.frame()
rownames(aic) <- NULL    
aic%>%mutate(Model = c("Logit 1", "Logit 2"))%>% rename("Degrees of Freedom" = df)%>%
  dplyr::select(Model, "Degrees of Freedom", AIC)%>%kable(format = "html", align = "lll", caption = "AIC Results")%>%
  kable_paper("hover")%>%kable_styling(full_width=F)


## Paul Allison q ---

(20/1000)*100
(200/10000)*100
nrow(mydata[which(mydata$DRINKING_D == 0), ])/nrow(mydata[which(mydata$DRINKING_D == 1), ]) 

(nrow(mydata[which(mydata$DRINKING_D == 1), ])/nrow(mydata))*100   
nrow(mydata[which(mydata$DRINKING_D == 0), ])
nrow(mydata)
nrow(mydata[which(mydata$DRINKING_D == 1), ])     
