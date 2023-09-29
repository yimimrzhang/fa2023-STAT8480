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


data <- as.data.frame(data)  # for building DM models
row.names(data) <- data$ID
data <- data %>% select(-(2:3)) 


## Partitioning ##
library(caTools)
set.seed(1234)
split = sample.split(data$TargetB, SplitRatio = 0.5) 

####################################
######### Decision Tree ############
####################################

# Model building
library(rpart)
tree <- rpart(TargetB ~ ., data=data[split,],control=rpart.control(cp=0.005))

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
library(partykit)
plot(as.party(tree.final))
tree.misc<-min(misc)


######################
### Transformation ###
######################

data.xf <- data  # pass on data

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

# Build full model
full = glm(TargetB ~., family=binomial, data=data.mdl[split,])
summary(full)
n<-sum(split)


# Backward
reg.bwd <- step(full, direction="backward",k=log(n), trace = FALSE)
summary(reg.bwd)
reg.bwd.prob<-predict(reg.bwd,data.mdl[!split, ], type = "response")
reg.bwd.class <- ifelse(reg.bwd.prob > 0.5, 1, 0)
reg.bwd.misc<- 1-mean(reg.bwd.class == data.mdl$TargetB[!split])
reg.bwd.misc


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
  layer_dense(units = 4, activation = "tanh", input_shape = c(7)) %>% 
  layer_dense(units = 1, activation = "sigmoid")

ann %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = "accuracy"
)

callbacks.list = list(
  callback_early_stopping(
    monitor = "val_accuracy",
    patience = 5
  ),
  callback_model_checkpoint(
    filepath="my_ann.h5",
    monitor = "val_accuracy",
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


ann.select <-load_model_hdf5("my_ann.h5") 
results.valid <- evaluate(ann.select, x.valid,y.valid)
ann.misc<-1-results.valid[2]


## Prediction ##
ann.prob<-ann.select %>% predict(x.valid) 
ann.class <-ifelse(ann.prob > 0.5, 1, 0)


############################################
################ Week 7 ####################
############################################

### Misclassification Rate ###
c(tree.misc,  reg.bwd.misc, ann.misc)



### Confusion Matrix ###
table(ann.class, y.valid)


library(gmodels)
CrossTable(ann.class, y.valid)



### Evaluations related to Confusion Matrix ###
library(caret)
ann.class <- as.factor(ann.class)
confusionMatrix(data = ann.class, 
                reference = data$TargetB[!split],
                positive = "1",      # Default is 1st level
                mode= "everything")



### ROC curves ###
library(pROC)
rocCurve.ann<- roc(response =  data$TargetB[!split], 
                   predictor = as.vector(ann.prob),
                   levels = levels(data$TargetB[!split]))  # This function assumes that 2nd
                                                           # level is the event of interest, 
                                                           # and 1st level is control.
                   

# ANN: area under curve
auc(rocCurve.ann)

# DT
tree.prob <- predict(tree.final, data[!split,], type = "prob")[,2]
rocCurve.tree <- roc(data$TargetB[!split], tree.prob)

# Reg
rocCurve.reg<- roc(data$TargetB[!split], reg.bwd.prob)

## Compare Area Under Curve
c(auc(rocCurve.tree), auc(rocCurve.reg), auc(rocCurve.ann))



## Plot ROC Curves
plot(rocCurve.tree, legacy.axes = TRUE, col= 'blue')
plot.roc(rocCurve.reg, add=TRUE, legacy.axes = TRUE, col= 'red')
plot.roc(rocCurve.ann, add=TRUE, legacy.axes = TRUE, col= 'green')
legend('topleft', legend=c('valid.ann', 'valid.reg', 'valid.tree'), 
       col=c("green","red","blue"),lty=c(1,1,1))




### Profit ###
TargetB.p <- fct_rev(data$TargetB)


# Define profit matrix
profitMatrix<- matrix(c(0.55,-0.45,0,0), nrow=2)
# profitMatrix<- matrix(c(1,0,0,1),nrow=2)
rownames(profitMatrix) <- levels(TargetB.p)
colnames(profitMatrix) <- levels(TargetB.p)
profitMatrix


# Calculate decision threshold
N <- profitMatrix[2,2]-profitMatrix[2,1] 
D <- N - (profitMatrix[1,2]-profitMatrix[1,1]) 
threshold <- N/D
threshold


# ANN
ann.pclass <- as.factor(ifelse(ann.prob >= threshold, 1, 0)) # decision based on profit
ann.pclass <- fct_rev(ann.pclass)
ann.cm <- table(TargetB.p[!split], ann.pclass) # confusion matrix 
ann.profit=sum(profitMatrix*ann.cm)/sum(ann.cm) 


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



# Reg
reg.profit <- avgprofit(profitMatrix, reg.bwd.prob, data$TargetB[!split])


# DT
tree.profit=avgprofit(profitMatrix, tree.prob, data$TargetB[!split])


# Compare average profits
c(tree.profit, reg.profit, ann.profit)

## misclassification rate
## 1-c(tree.profit, reg.profit, ann.profit)






























 


















