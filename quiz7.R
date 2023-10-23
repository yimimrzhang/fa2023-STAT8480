############################
####### Quiz Seven #########
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



###############################
####### Decision Tree #########
###############################

## part 1 ##

prop<-1/prop.table(table(organics$TargetBuy))
costMatrix <-matrix(c(0,prop[2],prop[1],0), nrow=2) 
costMatrix

library(rpart)
DT.001<- rpart(formula = TargetBuy ~ .,data = organics[split,],
               parms=list(loss=costMatrix),
               control=rpart.control(cp=0.001))

library(caret)
library(pROC)
cp.seq=DT.001$cptable[,1]
fscore<-numeric(0)
fscore[1]<-0
for (i in 2:length(cp.seq)) {
  tree.prob = predict(prune(DT.001, cp=cp.seq[i]), organics[!split,],type="prob")[,2] 
  rocCurve.tree <- roc(organics$TargetBuy[!split], tree.prob, quiet=TRUE)
  treeThresh <-  coords(rocCurve.tree, x = "best", best.method = "closest.topleft", transpose = FALSE)
  tree.class <- as.factor(ifelse(tree.prob >= treeThresh$threshold, 1,0))
  fscore[i]<-confusionMatrix(tree.class,organics$TargetBuy[!split], positive = "1")$byClass["F1"]
}


plot(DT.001$cptable[,'nsplit']+1,fscore,
     type="o", xlab="Number of Leaves", ylab="F-score")


DT.001$cptable[which(fscore==max(fscore)),] # number of splits in the optimal tree
max(fscore) # max F-score=0.577435


# Final model
tree.final=prune(DT.001,cp=cp.seq[fscore==max(fscore)])  
library(partykit)
plot(as.party(tree.final))




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



#####################################
####### Logistic Regression #########
#####################################

organics.mdl<-organics.imp

full = glm(TargetBuy ~ ., family=binomial, data=organics.mdl[split,])
summary(full)

null<-glm(TargetBuy ~ 1, family=binomial, data=organics.mdl[split,])

# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", trace = FALSE)
summary(reg.step)


## part 2 ##

# Validation F-score
reg.step.prob<-predict(reg.step,organics.mdl[!split,], type = "response") 

rocCurve.reg <- roc(organics$TargetBuy[!split], reg.step.prob, quiet = TRUE)
regThresh <-  coords(rocCurve.reg, x = "best", best.method = "closest.topleft", transpose = FALSE)
regThresh
reg.class <- as.factor(ifelse(reg.step.prob >= regThresh$threshold, 1,0))
reg.fscore<-confusionMatrix(reg.class,organics$TargetBuy[!split],
                            positive = "1")$byClass["F1"]

reg.fscore  # best f-score=0.561067




#####################
####### ANN  ########
#####################

organics.ann<-organics.imp

vars.ann<-attr(terms(reg.step), "term.labels") # extract variable names from bwd model
vars.ann<-c(vars.ann, "PromSpend")


## Standardization ## 
library(caret)
ScaleParams <- preProcess(organics.ann[split, vars.ann], method=c("center", "scale"))
organics.ann <- organics.ann %>% predict(ScaleParams, .)

## Hot encoding ##
str(organics.ann)
dummy <- dummyVars( ~ ., data = organics.ann[split, c(vars.ann, "TargetBuy")], fullRank = TRUE)
organics.ann.encode<- organics.ann %>% predict(dummy, .)  

x.train <- organics.ann.encode[split, -ncol(organics.ann.encode)] 
y.train <- organics.ann.encode[split,"TargetBuy.1"]
x.valid <- organics.ann.encode[!split, -ncol(organics.ann.encode)] 
y.valid <- organics.ann.encode[!split,"TargetBuy.1"]


library(tensorflow)
library(keras)

set_random_seed(42)
ann <- keras_model_sequential() %>% 
  layer_dense(units = 4, activation = "tanh", input_shape = c(8)) %>% 
  layer_dense(units = 1, activation = "sigmoid")



ann %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)



callbacks.list = list(
  callback_model_checkpoint(filepath="my_ann_ex7.h5", 
                            monitor = "val_loss", 
                            save_best_only = TRUE, 
  ))


history <- ann %>% fit(
  x= x.train,
  y= y.train,
  epochs = 40,
  validation_data = list(x.valid,y.valid),
  verbose = 1,
  callbacks = callbacks.list
)


ann.select<- load_model_hdf5("my_ann_ex7.h5") 



## part 3 ##

## Prediction ##
ann.prob<-ann.select %>% predict(x.valid)

rocCurve.ann <- roc(organics$TargetBuy[!split], as.vector(ann.prob), quiet=TRUE)
annThresh <-  coords(rocCurve.ann, x = "best", best.method = "closest.topleft", transpose = FALSE)
ann.class <- as.factor(ifelse(ann.prob >= annThresh$threshold, 1,0))
ann.fscore<-confusionMatrix(ann.class,organics$TargetBuy[!split],
                            positive = "1")$byClass["F1"]

ann.fscore  # best f-score=0.5805485

confusionMatrix(ann.class,organics$TargetBuy[!split],
                positive = "1")


##################################
############ Scoring #############
##################################

## part 5 ##

Mean <-organics.xf %>%
  filter(split) %>% 
  summarise(across(where(is.numeric), ~ mean(.,na.rm = TRUE))) 

Mode <- organics.xf %>%
  filter(split) %>% 
  summarise(across(where(is.factor), mode))

save(Mean, Mode, reg.step, ScaleParams,annThresh, file="myscore_organics.RData")

load("myscore_organics.RData")

scoring<- function(organics,Mean, Mode, reg.step, ScaleParams,annThresh) {
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
  
  
  

  #####################
  ####### ANN  ########
  #####################
  
  organics.ann<-organics.imp
  
  vars.ann<-attr(terms(reg.step), "term.labels") # extract variable names from bwd model
  vars.ann<-c(vars.ann, "PromSpend")
  
  
  ## Standardization ## 
  library(caret)
  organics.ann <- organics.ann %>%  predict(ScaleParams, .)
  
  ## Hot encoding ##
  dummy <- dummyVars( ~ ., data = organics.ann[vars.ann], fullRank = TRUE)
  organics.ann.encode <- organics.ann %>% predict(dummy, .)

  
  x.valid<-organics.ann.encode
  
  
  ann.select<- load_model_hdf5("my_ann_ex7.h5") 
  
  ## Prediction ##
  ann.prob<-ann.select %>% predict(x.valid) 
  ann.class <- as.factor(ifelse(ann.prob >= annThresh$threshold, 1,0))
  
  raw %>% 
    mutate(pred.prob=ann.prob,
           pred.class=ann.class)
  }


organics.score <-read_csv("scoreorganics.csv", na=c(".", "NA", "", "?"))

# organics.score1 <- organics.score %>% distinct(ID, .keep_all = TRUE)
# write.csv(organics.score1, file = "scoreorganics1.csv",row.names=FALSE)

organics.score.pred<-scoring(organics.score, Mean, Mode, reg.step, ScaleParams,annThresh)

summary(organics.score.pred$pred.class)
prop.table(table(organics.score.pred$pred.class))






