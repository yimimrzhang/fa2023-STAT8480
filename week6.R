
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

### FIX: set missing flags as nominal inputs ###
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



############################################
################ Week 6 ####################
############################################


########################################
####### Artificial Neural Network ######
########################################

### FIX: set missing flags as nominal inputs ###

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
  layer_dense(units = 6, activation = "tanh", input_shape = c(7)) %>% 
  layer_dense(units = 6, activation = "tanh") %>%
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
#summary(ann.select)
results.valid <- evaluate(ann.select, x.valid,y.valid)
results.valid 


## Prediction ##
evaluate.prob<-ann.select %>% predict(x.valid) 
evaluate.class <-ifelse(evaluate.prob > 0.5, 1, 0)
train.prob <-ann.select %>% predict(x.train) 




##############
# Lift Graph #
##############

## Model Evaluation: Lift Graph ##
evaluate.prob1 <- predict(reg.bwd, data.mdl[!split,], type = "response")


library(ROCR)
pred.eva1 <- prediction(evaluate.prob1, data.mdl$TargetB[!split])
pred.eva <- prediction(evaluate.prob, data.mdl$TargetB[!split])
pred<-prediction(train.prob, data.mdl$TargetB[split])

perf.eva1 <- performance(pred.eva1,"lift","rpp")
perf.eva <- performance(pred.eva,"lift","rpp")
perf <- performance(pred,"lift","rpp")

plot(perf, col='blue',  main="Lift Curve")
plot(perf.eva, col= 'red', add = TRUE,main="Lift Curve")
plot(perf.eva1, col= 'green', add = TRUE,main="Lift Curve")
legend('topright', legend=c('train.ann', 'valid.ann', 'valid.bwd'), col=c("blue","red","green"),lty=c(1,1))
