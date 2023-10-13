########################################################
######## Predicted Modeling (Class Imbalance) ##########
########################################################

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
row.names(data) <- data$ID
data <- data %>% select(-c(ID,TargetD)) # use var names for consistency


## Partitioning ##
library(caTools)
set.seed(1234)
split = sample.split(data$TargetB, SplitRatio = 0.5) 

library(skimr) 
data %>% filter(split) %>% skim
data %>% filter(!split) %>% skim



####################################
######### Decision Tree ############
####################################

# Model building

library(rpart)
tree <- rpart(TargetB ~ ., data=data[split,],control=rpart.control(cp=0))
summary(tree)


# Model pruning on MISC
# cp.seq=tree$cptable[,1]
# misc<-numeric()
# for (i in 1:length(cp.seq)) {
#   tree.predict = predict(prune(tree, cp=cp.seq[i]), data[!split,],type="class")
#   cm=table(data$TargetB[!split], tree.predict) # confusion matrix
#   misc[i]=(cm[1,2]+cm[2,1])/sum(cm) # misclassification rate
# }
# 
# plot(tree$cptable[,'nsplit']+1,misc,
#      type="o", xlab="Number of Leaves", ylab="Misclassification Rate")
# 


# Model pruning on F-score USING alternative cutoff
library(pROC)
library(caret)
cp.seq=tree$cptable[,1]
fscore<-numeric()
fscore[1]<-0  # Set root node F-score zero
for (i in 2:length(cp.seq)) {
  tree.prob = predict(prune(tree, cp=cp.seq[i]), data[!split,],type="prob")[,2] 
  rocCurve.tree <- roc(data$TargetB[!split], tree.prob, quiet=TRUE)
  treeThresh <-  coords(rocCurve.tree, x = "best", best.method = "closest.topleft")
  tree.class <- as.factor(ifelse(tree.prob >= treeThresh$threshold, 1,0))
  fscore[i]<-confusionMatrix(tree.class,data$TargetB[!split],positive = "1")$byClass["F1"]
}

plot(tree$cptable[,'nsplit']+1,fscore,
     type="o", xlab="Number of Leaves", ylab="F-score")


# Final model
tree.final=prune(tree,cp=cp.seq[fscore==max(fscore)])  # max F-score=0.1165179
library(partykit)
plot(as.party(tree.final))



######################
### Transformation ###
######################

data.xf <- data  # pass on data

# numeric input xform
library(caret)
TransformParams <- data.xf %>% 
  filter(split) %>% 
  preProcess(method="YeoJohnson")
TransformParams$yj


# numeric input xform
data.xf <- data.xf %>% 
  mutate(across(starts_with(c("GiftCnt","GiftAvg")), ~ log(.+1)))

# nominal input xform
levels(data.xf$StatusCat96NK)<-c("A", "L", "N", "L", "N", "A")


######################
##### Imputation #####
######################

data.imp<-data.xf # pass on data


# check missing data
vars.na <- data.imp %>% select_if(colSums(is.na(.))>0) %>% names
vars.na

# numeric impute: By mean #
data.imp<-data.imp %>%
  mutate(across(where(is.numeric), ~replace_na(.,mean(.[split],na.rm = TRUE))))


# create missing value flag #
data.imp[paste(vars.na, "NA", sep=".")] <- ifelse(is.na(data.xf[vars.na]), 1, 0)  
data.imp <- data.imp %>% mutate(across(ends_with(".NA"), as.factor))



########################################
######### Logistic Regression ##########
########################################

data.mdl<-data.imp # pass on data

# summary(data.mdl)
# summary(data.mdl[split,])

# Build full model
full = glm(TargetB ~., family=binomial, 
           data=data.mdl[split,] %>% select(!GiftTimeFirst.NA)) # exclude GiftTimeFirst.NA
summary(full)
n<-sum(split)


# Backward
reg.bwd <- step(full, direction="backward",k=log(n), trace = FALSE)
summary(reg.bwd)
reg.bwd.prob<-predict(reg.bwd,data.mdl[!split,], type = "response")

# Use alternative cutoff
rocCurve.reg <- roc(data$TargetB[!split], reg.bwd.prob, quiet = TRUE)
regThresh <-  coords(rocCurve.reg, x = "best", best.method = "closest.topleft")
reg.class <- as.factor(ifelse(reg.bwd.prob >= regThresh$threshold, 1,0))
reg.fscore<-confusionMatrix(reg.class,data$TargetB[!split],
                           positive = "1")$byClass["F1"]

reg.fscore  # f-score=0.1303823

confusionMatrix(reg.class,data$TargetB[!split],
                positive = "1", mode= "everything")


########################################
####### Artificial Neural Network ######
########################################


#####################
## ANN Preparation ##
#####################

data.ann<-data.imp


vars.ann<-attr(terms(reg.bwd), "term.labels") # extract variable names from bwd model
vars.ann<-c(vars.ann,"StatusCat96NK")


## Standardization: numeric inputs ## 
library(caret)
ScaleParams <- preProcess(data.ann[split, vars.ann], method=c("center", "scale"))
data.ann <- data.ann %>% predict(ScaleParams, .)


## Dummy Encoding: nominal inputs ##
dummy <- dummyVars( ~ . , data = data.ann[split, c(vars.ann, "TargetB")], fullRank = TRUE)
data.ann.encode <- data.ann %>% predict(dummy, .)


## Prepare train/validation sets as matrices ##
x.train <- data.ann.encode[split, -ncol(data.ann.encode)] 
y.train <- data.ann.encode[split,"TargetB.1"]
x.valid <- data.ann.encode[!split, -ncol(data.ann.encode)] 
y.valid <- data.ann.encode[!split,"TargetB.1"]


####################
### ANN Building ###
####################
library(tensorflow)
library(keras)


set_random_seed(27)
ann <- keras_model_sequential() %>% 
  layer_dense(units = 4, activation = "tanh", input_shape = c(9)) %>%  # update input shape
  layer_dense(units = 1, activation = "sigmoid")

ann %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

callbacks.list = list(
  callback_early_stopping(
    monitor = "val_loss", # change
    patience = 5
  ),
  callback_model_checkpoint(
    filepath="my_ann_raw.h5",
    monitor = "val_loss",  # change
    save_best_only = TRUE
  )
)

history <- ann %>% fit(
  x= x.train,
  y= y.train,
  epochs = 40,
  validation_data = list(x.valid,y.valid),
  verbose = 1,
  callbacks = callbacks.list)



ann.select <-load_model_hdf5("my_ann_raw.h5") 


## Prediction ##
ann.prob<-ann.select %>% predict(x.valid) 

# Use alternative cutoff
rocCurve.ann <- roc(data$TargetB[!split], as.vector(ann.prob), quiet=TRUE)
annThresh <-  coords(rocCurve.ann, x = "best", best.method = "closest.topleft")
ann.class <- as.factor(ifelse(ann.prob >= annThresh$threshold, 1,0))
ann.fscore<-confusionMatrix(ann.class,data$TargetB[!split],
                            positive = "1")$byClass["F1"]

ann.fscore  # f-score=0.132415

confusionMatrix(ann.class,data$TargetB[!split],
                positive = "1", mode= "everything")


##################################
######### Profit Tree ############
##################################

# Model building

prop<-1/prop.table(table(data$TargetB)) # Obtain inverse prior probs

# Define cost matrix
costMatrix <-matrix(c(0,prop[2],prop[1],0), nrow=2) 
costMatrix

library(rpart)
tree <- rpart(formula = TargetB ~ .,data = data[split,],
              parms=list(loss=costMatrix),
              control=rpart.control(cp=0.001))



# Model pruning on F-score
library(pROC)
library(caret)
cp.seq=tree$cptable[,1]
fscore<-numeric(0)
fscore[1]<-0
for (i in 2:length(cp.seq)) {
  tree.prob = predict(prune(tree, cp=cp.seq[i]), data[!split,],type="prob")[,2] 
  rocCurve.tree <- roc(data$TargetB[!split], tree.prob, quiet=TRUE)
  treeThresh <-  coords(rocCurve.tree, x = "best", best.method = "closest.topleft")
  tree.class <- as.factor(ifelse(tree.prob >= treeThresh$threshold, 1,0))
  fscore[i]<-confusionMatrix(tree.class,data$TargetB[!split],
                             positive = "1")$byClass["F1"]
}


plot(tree$cptable[,'nsplit']+1,fscore,
     type="o", xlab="Number of Leaves", ylab="F-score")


# Final model
tree.final=prune(tree,cp=cp.seq[fscore==max(fscore)])  # max F-score=0.121434
library(partykit)
plot(as.party(tree.final))






########################
##### For Scoring ######
########################

Mean <-data.xf %>%
  filter(split) %>% 
  summarise(across(where(is.numeric), ~ mean(.,na.rm = TRUE))) 

save(Mean, reg.bwd, regThresh, file="myscore_raw.RData")










