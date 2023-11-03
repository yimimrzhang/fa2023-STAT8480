############################
####### Quiz Eight #########
############################

library(tidyverse)
organics<-read_csv("organics.csv",na=c(".", "NA", "", "?"))

organics <- organics %>% 
  mutate(across(where(is.character) | TargetBuy, as.factor)) 

organics <- as.data.frame(organics)  # for building DM models
row.names(organics) <- organics$ID
organics <- organics %>% select(-c(ID,TargetAmt,DemCluster))

library(caTools)
set.seed(4321)
split = sample.split(organics$TargetBuy, SplitRatio = 0.5)  



#################################
####### Transformation  #########
#################################

organics.xf<-organics

organics.xf <- organics.xf %>% 
  mutate(across(c(PromTime, PromSpend), ~ log(.+1)))


#############################
####### Imputation  #########
#############################

organics.imp<-organics.xf

vars.na <- organics.imp %>% select_if(colSums(is.na(.))>0) %>% names

mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

organics.imp <-organics.imp %>% 
  mutate(across(where(is.factor) & !TargetBuy, ~replace_na(.,mode(.[split])))) %>%  # Nominal Input: By Mode #
  mutate(across(where(is.numeric), ~replace_na(.,mean(.[split],na.rm = TRUE))))     # Numeric Input: By Mean #


# Create Missing Value Flag #
organics.imp[paste(vars.na, "NA", sep=".")] <- ifelse(is.na(organics[vars.na]), 1, 0) 

organics.imp <- organics.imp %>% mutate(across(ends_with(".NA"), as.factor))


#######################
#### Random Forest ####
#######################

## part 1 ##
organics.rf<-organics.imp

minor<-unname(summary(organics.rf$TargetBuy[split])[2])

library(randomForest)
set.seed(4321)
RF <- randomForest(TargetBuy ~., data=organics.rf[split,],
                   ntree = 201,
                   strata= organics.rf$TargetBuy[split], 
                   sampsize=c(minor,minor),
                   importance = TRUE)

# Make predictions #
library(caret)
RF.class<- predict(RF, newdata=organics.rf[!split,], type="response")
fscore<-confusionMatrix(table(RF.class,organics.rf$TargetBuy[!split]),
                        positive = "1")$byClass["F1"]   
fscore


# Variable importance #
RF$importance
varImpPlot(RF)  



## part 2 ##

#### Parameter Tuning: mtry ####
m<-seq(2,5)
fscore.seq<-numeric()

for(i in 1:length(m)){ 
  set.seed(4321)
  rf <- randomForest(TargetBuy ~., data=organics.rf[split,],
                     ntree = 201,
                     strata= organics.rf$TargetBuy[split], 
                     sampsize=c(minor,minor),
                     mtry=m[i])
  
  rf.class<- predict(rf, newdata=organics.rf[!split,], type="response")
  fscore.seq[i]<-confusionMatrix(table(rf.class,organics.rf$TargetBuy[!split]),
                                 positive = "1")$byClass["F1"]
}

plot(m, fscore.seq, pch=19 , col="blue", type="b",
     ylab="F-score",xlab="Number of Predictors considered at each split")


m.best<- m[which.max(fscore.seq)] 
max(fscore.seq)




## part 3 ##
set.seed(4321)
RF.final <- randomForest(TargetBuy ~., data=organics.rf[split,],
                         ntree = 201,
                         strata= organics.rf$TargetBuy[split], 
                         sampsize=c(minor,minor),
                         importance = TRUE,
                         mtry=m.best)



############ Scoring #############

Mean <-organics.xf %>%
  filter(split) %>% 
  summarise(across(where(is.numeric), ~ mean(.,na.rm = TRUE))) 

Mode <- organics.xf %>%
  filter(split) %>% 
  summarise(across(where(is.factor), mode))

save(Mean, Mode, RF.final, file="myscore_organics2.RData")
load("myscore_organics2.RData")

scoring<- function(organics,Mean, Mode, RF) {
  raw<-organics
  organics <- organics %>% 
    mutate(across(where(is.character), as.factor)) 
  
  organics <- as.data.frame(organics)  # for building DM models
  row.names(organics) <- organics$ID
  organics <- organics %>% select(-c(ID,DemCluster))
  
  #################################
  ####### Transformation  #########
  #################################
  
  organics.xf<-organics
  
  organics.xf <- organics.xf %>% 
    mutate(across(c(PromTime, PromSpend), ~ log(.+1)))
  
  
  #############################
  ####### Imputation  #########
  #############################
  
  organics.imp<-organics.xf
  
  vars.na <- organics.imp %>% select_if(colSums(is.na(.))>0) %>% names
  
  # numeric impute: By mean #
  vars_imp <- organics.imp %>% select_if(is.numeric) %>% replace_na(as.list(Mean))
  organics.imp[names(vars_imp)] <- vars_imp
  
  
  # nominal impute: By mode #
  vars_imp.cat <- organics.imp %>% select_if(is.factor) %>% replace_na(as.list(Mode))
  organics.imp[names(vars_imp.cat)] <- vars_imp.cat
  
  # Create Missing Value Flag #
  organics.imp[paste(vars.na, "NA", sep=".")] <- ifelse(is.na(organics[vars.na]), 1, 0) 
  
  organics.imp <- organics.imp %>% mutate(across(ends_with(".NA"), as.factor))
  
  ########################
  ##### Random Forest ####
  ########################
  
  organics.rf<-organics.imp
  
  # Make predictions #
  library(caret)
  RF.class<- predict(RF, newdata=organics.rf, type="response")
  RF.prob<- predict(RF, newdata=organics.rf, type="prob")[,2]
  
  raw[c("pred.prob","pred.class")] <- data.frame(RF.prob, RF.class)
  raw
}
  
organics.score <-read_csv("scoreorganics.csv", na=c(".", "NA", "", "?"))

organics.score.pred<-scoring(organics.score, Mean, Mode, RF.final)

summary(organics.score.pred$pred.class)
prop.table(table(organics.score.pred$pred.class))








