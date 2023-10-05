##########################
####### Quiz Six #########
##########################

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

library(rpart)
DT.001<- rpart(formula = TargetBuy ~ .,data = organics[split,],
               control=rpart.control(cp=0.001))

cp.seq=DT.001$cptable[,1]
MISC<-numeric()
for (i in 1:length(cp.seq)) {
  DT.predict = predict(prune(DT.001, cp=cp.seq[i]), organics[!split,],type="class")
  cm=table(DT.predict, organics$TargetBuy[!split])
  MISC[i]=(cm[1,2]+cm[2,1])/sum(cm)}

  
tree.final=prune(DT.001,cp=cp.seq[MISC==min(MISC)])
tree.class<-predict(tree.final, organics[!split,],type="class")
tree.prob <-predict(tree.final, organics[!split,], type = "prob")[,2]


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

levels(organics.mdl$TargetBuy) 

full = glm(TargetBuy ~ ., family=binomial, data=organics.mdl[split,])
summary(full)

null<-glm(TargetBuy ~ 1, family=binomial, data=organics.mdl[split,])

# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", trace = FALSE)
summary(reg.step)


# Validation MISC
reg.step.prob<-predict(reg.step,organics.mdl[!split,], type = "response") 
reg.step.class <- ifelse(reg.step.prob > 0.5, 1, 0)
reg.step.misc<- 1-mean(reg.step.class == organics.mdl$TargetBuy[!split])
reg.step.misc



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
use_virtualenv("C:/Users/yzhang09/Documents/Rstudio/r-reticulate")
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
  callback_early_stopping(monitor = "val_accuracy", 
                          patience = 5),
  callback_model_checkpoint(filepath="my_ann_ex.h5", 
                            monitor = "val_accuracy", 
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


ann.select<- load_model_hdf5("my_ann_ex.h5") 

## Prediction ##
ann.prob<-ann.select %>% predict(x.valid) 
ann.class <-ifelse(ann.prob > 0.5, 1, 0)


############################### Continue from here #################################

# part b.1 #
ann.misc<-1-evaluate(ann.select, x.valid,y.valid)[2] 
tree.misc<-min(MISC)

c(tree.misc, reg.step.misc, ann.misc)


# part b.2 #
TargetBuy.valid <- organics$TargetBuy[!split]

library(caret)
models.class <- data.frame(tree.class, reg.step.class, ann.class) 
models.class %>% 
  mutate(across(everything(), as.factor)) %>% 
  summarise(across(everything(), ~ confusionMatrix(., TargetBuy.valid, positive = "1")$byClass["F1"]))



# part b.3 #
library(pROC)
rocCurve.tree <- roc(TargetBuy.valid, tree.prob)
rocCurve.reg<- roc(TargetBuy.valid, reg.step.prob)
rocCurve.ann<- roc(TargetBuy.valid, as.vector(ann.prob))  


## Plot ROC Curves
plot(rocCurve.tree, legacy.axes = TRUE, col= 'blue')
plot.roc(rocCurve.reg, add=TRUE, legacy.axes = TRUE, col= 'red')
plot.roc(rocCurve.ann, add=TRUE, legacy.axes = TRUE, col= 'green')
legend('topleft', legend=c('valid.ann', 'valid.reg', 'valid.tree'), 
       col=c("green","red","blue"),lty=c(1,1,1))

## Area under curve
auc(rocCurve.ann)


# part b.4 #

# Define profit matrix
profitMatrix<- matrix(c(4,0,0,4/3), nrow=2)
profitMatrix


# Function: calculate model average profit
avgprofit<- function(pmatrix, prob, target){ 
  # pmatrix: profit matrix
  #    prob: predicted prob.
  # target:  actual target, assuming 2nd level 
  #          is of primary interest
  
  N <- pmatrix[2,2]-pmatrix[2,1] 
  D <- N - (pmatrix[1,2]-pmatrix[1,1]) 
  threshold <- N/D
  pclass<-as.factor(ifelse(prob >= threshold, 1, 0))
  pclass <- fct_rev(pclass)
  target <- fct_rev(target)
  cm <- table(target,pclass)
  profit <- sum(pmatrix*cm)/sum(cm) 
  profit
}


models.prob <- data.frame(tree.prob, reg.step.prob, ann.prob) 
models.prob %>% 
  summarise(across(everything(), ~ avgprofit(profitMatrix,., TargetBuy.valid)))















