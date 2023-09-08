## import data ##
library(tidyverse)
data<-read_csv("pva97nko.csv", na=c(".", "NA", "", "?"))


## data cleaning and define measure levels ##

data<- data %>% 
  mutate(across(where(is.character) & !c(StatusCat96NK, DemGender, DemHomeOwner),parse_number)) %>% 
  mutate(across(where(is.character) | c(TargetB, StatusCatStarAll, DemCluster), as.factor))


## data modification/correction ##
## Replace DemMedIncome ##
data <- data %>% mutate(DemMedIncome=replace(DemMedIncome,DemMedIncome==0, NA))

############################################
################ Week 4 ####################
############################################


## Partitioning ##
library(caTools)
set.seed(1234)
split = sample.split(data$TargetB, SplitRatio = 0.5) 

library(skimr) 
data %>% filter(split) %>% skim
data %>% filter(!split) %>% skim


# Challenge: How to split data into three sets
# say, training/validation/test with ratio 70:20:10,
# while maintaining the stratification of Target 1/0


split = sample.split(data$TargetB, SplitRatio = 0.7) 
split.valid<-!split
split.test<-!split
split2<-sample.split(data$TargetB[!split],SplitRatio = 2/3)
split.valid <- split.valid %>% replace(.,.==TRUE, split2)
split.test <- split.test %>% replace(.,.==TRUE, !split2)


summary(data$TargetB[split])
summary(data$TargetB[split.valid])
summary(data$TargetB[split.test])

####################################
######### Decision Tree ############
####################################

data <- as.data.frame(data)  # for building DM models
row.names(data) <- data$ID
data <- data %>% select(-(2:3)) 


# Model building
library(rpart)
tree <- rpart(TargetB ~ ., data=data[split,],control=rpart.control(cp=0.005))

summary(tree)
print(tree$cptable)
printcp(tree)

# Model visualization
library(partykit)
plot(as.party(tree))
print(tree)
#dev.off() # reset graphic options

# Model pruning
cp.seq=tree$cptable[,1]
misc<-numeric()
for (i in 1:length(cp.seq)) {
tree.predict <- predict(prune(tree, cp=cp.seq[i]),data[!split,],type="class")
  cm<-table(data$TargetB[!split], tree.predict)   # confusion matrix 
  misc[i]=(cm[1,2]+cm[2,1])/sum(cm) # misclassification rate
}

plot(tree$cptable[,'nsplit']+1,misc,
     type="o", xlab="Number of Leaves", ylab="Misclassification Rate")




# Final model
tree.final=prune(tree,cp=cp.seq[misc==min(misc)])
plot(as.party(tree.final))

tree.final$variable.importance 
names(tree.final$variable.importance) # extract useful variable names





## Model Evaluation: Lift Graph ##

evaluate.prob <- predict(tree.final, data[!split,], type = "prob")[,2]
train.prob <- predict(tree.final, data[split,], type = "prob")[,2]


library(ROCR)
pred.eva <- prediction(evaluate.prob, data$TargetB[!split])
pred<-prediction(train.prob, data$TargetB[split])

perf.eva <- performance(pred.eva,"lift","rpp")
perf <- performance(pred,"lift","rpp")

plot(perf, col='blue', type="b", main="Lift Curve")
plot(perf.eva, col= 'red', type="b",add = TRUE,main="Lift Curve")
legend('topright', legend=c('train', 'validation'), col=c("blue","red"),lty=c(1,1))


##  Ordinal Input Variables ##
data %>% select(where(is.factor)) %>% str

data <- data %>% mutate(DemCluster=factor(DemCluster, order = TRUE))
str(data$DemCluster)

tree.ordinal<- rpart(TargetB ~ DemCluster, data=data[split,])
plot(as.party(tree.ordinal))
print(tree.ordinal)

  
data <- data %>% mutate(StatusCat96NK=factor(StatusCat96NK, order = TRUE,
                                             levels = c("S", "N", "L","F","E","A")))
str(data$StatusCat96NK)
tree.ordinal<- rpart(TargetB ~ StatusCat96NK, data=data[split,])
plot(as.party(tree.ordinal))
print(tree.ordinal)



