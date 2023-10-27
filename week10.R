###########################################
######## Week 10   Random Forest ##########
###########################################

## import data ##
library(tidyverse)
data<-read_csv("pva97nk_raw.csv", na=c(".", "NA", "", "?"))
summary(data)


## data cleaning and define measure levels ##
data<- data %>% 
  mutate(across(where(is.character) & !c(StatusCat96NK, DemGender, DemHomeOwner),parse_number)) %>% 
  mutate(across(where(is.character) | c(TargetB, StatusCatStarAll, DemCluster), as.factor))


## data modification/correction ##
## Replace DemMedIncome ##
data <- data %>% mutate(DemMedIncome=replace(DemMedIncome,DemMedIncome==0, NA))


data <- as.data.frame(data)  # for building DM models
#row.names(data) <- data$ID
#data <- data %>% select(-c(ID,TargetD)) # use var names for consistency


## Partitioning ##
library(caTools)
set.seed(1234)
split = sample.split(data$TargetB, SplitRatio = 0.5) 


#################################################################
####################    With Under-Sampling   ###################
#################################################################

## Under-Sampling ##
library(caret)
set.seed(4321)
downSampledTrain <- downSample(x = data[split,] %>% select(-TargetB),
                               y = data$TargetB[split],
                               ## keep the class variable name the same:
                               yname = "TargetB")

summary(downSampledTrain)

## Combine downsampled train with validation ##
data.down <- data %>% filter(ID %in% downSampledTrain$ID | !split)


# Indicator for balanced train data #
split.down<-ifelse(data.down$ID %in% downSampledTrain$ID, TRUE, FALSE)

sum(split.down)


# Prep for building DM models
row.names(data.down) <- data.down$ID # save ID as row names
data.down <- data.down %>% select(-c(ID,TargetD)) 

######################
##### Imputation #####
######################

data.imp<-data.down # pass on data

# check missing data
vars.na <- data.imp %>% select_if(colSums(is.na(.))>0) %>% names
vars.na

# numeric impute: By mean #
data.imp<-data.imp %>%
  mutate(across(where(is.numeric), ~replace_na(.,mean(.[split.down],na.rm = TRUE))))


# create missing value flag #
data.imp[paste(vars.na, "NA", sep=".")] <- ifelse(is.na(data.down[vars.na]), 1, 0)  
data.imp <- data.imp %>% mutate(across(ends_with(".NA"), as.factor))




#######################
#### Random Forest ####
#######################

data.rf<-data.imp


library(randomForest)
set.seed(1234)
RF <- randomForest(TargetB ~., data=data.rf[split.down,] %>% select(!DemCluster),
                   ntree = 1000,
                   importance = TRUE)

print(RF)
plot(RF)  # OOB error is OOB MISC (in black)


# Make predictions #
RF.class<- predict(RF, data.rf[!split.down,], type="response")
fscore<-confusionMatrix(RF.class,data.rf$TargetB[!split.down],
                        positive = "1")$byClass["F1"]  
fscore

# Variable importance #
RF$importance
varImpPlot(RF)  




#### Parameter Tuning: mtry ####

# m<-round(seq(from=2, to=28, length.out = 5))
m<-seq(2,7)
fscore.seq<-numeric()

for(i in 1:length(m)){ 
  set.seed(1234)
  rf <- randomForest(TargetB ~., data=data.rf[split.down,] %>% select(!DemCluster),
                     ntree=1000, mtry=m[i])
  
  rf.class<- predict(rf, newdata=data.rf[!split.down,], type="response")
  fscore.seq[i]<-confusionMatrix(rf.class,data.rf$TargetB[!split.down],
                                 positive = "1")$byClass["F1"]
}

plot(m, fscore.seq, pch=19 , col="blue", type="b",
     ylab="F-score",xlab="Number of Predictors considered at each split")


m.best<- m[which.max(fscore.seq)] 
# assign m.best to mtry in the above RF 
# and then pick appropriate ntree



#######################################################################
#######################    Without Under-Sampling   ###################
#######################################################################

row.names(data) <- data$ID
data <- data %>% select(-c(ID,TargetD)) # use var names for consistency


######################
##### Imputation #####
######################

data.imp<-data # pass on data


# check missing data
vars.na <- data.imp %>% select_if(colSums(is.na(.))>0) %>% names
vars.na

# numeric impute: By mean #
data.imp<-data.imp %>%
  mutate(across(where(is.numeric), ~replace_na(.,mean(.[split],na.rm = TRUE))))


# create missing value flag #
data.imp[paste(vars.na, "NA", sep=".")] <- ifelse(is.na(data[vars.na]), 1, 0)  
data.imp <- data.imp %>% mutate(across(ends_with(".NA"), as.factor))



#######################
#### Random Forest ####
#######################

data.rf<-data.imp


minor<-unname(summary(data.rf$TargetB[split])[2])

library(randomForest)
set.seed(1234)
RF <- randomForest(TargetB ~., data=data.rf[split,] %>% select(!DemCluster),
                   ntree = 500, 
                   strata= data.rf$TargetB[split], 
                   sampsize=c(minor,minor),
                   importance =TRUE)
print(RF)
plot(RF)  



# Make predictions #
library(caret)
RF.class<- predict(RF, newdata=data.rf[!split,], type="response")
fscore<-confusionMatrix(RF.class, data.rf$TargetB[!split],
                        positive = "1")$byClass["F1"]  
fscore


# Variable importance #
RF$importance
varImpPlot(RF)  


















