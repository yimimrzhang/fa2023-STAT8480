######### From Quiz Four ###########

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


# Transformation #

organics.xf<-organics

organics.xf <- organics.xf %>% 
  mutate(across(c(PromTime, PromSpend), ~ log(.+1)))


#### Imputation #####

organics.imp<-organics.xf

vars.na <- organics.imp %>% filter(split) %>% select_if(colSums(is.na(.))>0) %>% names

mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

organics.imp <-organics.imp %>% 
  mutate(across(where(is.factor) & !TargetBuy, ~replace_na(.,mode(.[split])))) %>%  # Nominal Input: By Mode #
  mutate(across(where(is.numeric), ~replace_na(.,mean(.[split],na.rm = TRUE))))     # Numeric Input: By Mean #

# Create Missing Value Flag #
organics.imp[paste(vars.na, "NA", sep=".")] <- ifelse(is.na(organics.xf[vars.na]), 1, 0) 

organics.imp <- organics.imp %>% mutate(across(ends_with(".NA"), as.factor))


#### Modeling #####

organics.mdl<-organics.imp

levels(organics.mdl$TargetBuy) 

full = glm(TargetBuy ~ ., family=binomial, data=organics.mdl[split,])
summary(full)

null<-glm(TargetBuy ~ 1, family=binomial, data=organics.mdl[split,])


# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", trace = FALSE)
summary(reg.step)



##########################
####### Quiz Five ########
##########################

########## ANN ###########

## part b: Set missing flags as nominal inputs ##
organics.imp <- organics.imp %>% mutate(across(ends_with(".NA"), as.factor))


# part c # 
vars.ann<-attr(terms(reg.step), "term.labels") # extract variable names from bwd model
vars.ann<-c(vars.ann, "PromSpend")


# part d #

## Standardization ## 
organics.ann<-organics.imp

library(caret)
ScaleParams <- preProcess(organics.ann[split, vars.ann], method=c("center", "scale"))
organics.ann <- organics.ann %>% predict(ScaleParams, .)

## Hot encoding ##
str(organics.ann)
dummy <- dummyVars( ~ ., data = organics.ann[split, c(vars.ann, "TargetBuy")], fullRank = TRUE)
organics.ann.encode<- organics.ann %>% predict(dummy, .) 


## ANN Preparation ##

x.train <- organics.ann.encode[split, -ncol(organics.ann.encode)] 
y.train <- organics.ann.encode[split,"TargetBuy.1"]
x.valid <- organics.ann.encode[!split, -ncol(organics.ann.encode)] 
y.valid <- organics.ann.encode[!split,"TargetBuy.1"]


# part e-f #

library(tensorflow)
library(keras)


set_random_seed(42)

ann <- keras_model_sequential() %>% 
  layer_dense(units = 4, activation = "tanh", input_shape = c(8)) %>% 
  layer_dense(units = 4, activation = "tanh") %>% 
  layer_dense(units = 1, activation = "sigmoid")


ann %>% compile(
  loss = "binary_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)

callbacks.list = list(
  callback_early_stopping(monitor = "val_accuracy", 
                          patience = 5),
  callback_model_checkpoint(filepath="my_ann.h5", 
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



ann.select<- load_model_hdf5("my_ann.h5") 
evaluate(ann.select, x.valid,y.valid)








