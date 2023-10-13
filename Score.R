###########################################
######## Scoring (Balanced Data) ##########
###########################################


# Save essential parameters and model(s) into .RData
## Include at end of training script ##

Mean <-data.xf %>%
  filter(split) %>% 
  summarise(across(where(is.numeric), ~ mean(.,na.rm = TRUE))) 

save(Mean, reg.bwd, ScaleParams, file="myscore.RData")  



load("myscore.RData")


# Create scoring function
scoring<- function(data, Mean, reg.bwd, ScaleParams) { # data: data for scoring
  raw<-data # save an original copy
  
  ### Now, follow in training process' footsteps ###
  
  ###################
  ### Preparation ###
  ###################
  library(tidyverse)
  data<- data %>% 
    mutate(across(where(is.character) & !c(StatusCat96NK, DemGender, DemHomeOwner),parse_number)) %>% 
    mutate(across(where(is.character) | c(StatusCatStarAll, DemCluster), as.factor))
  
  data <- data %>% mutate(DemMedIncome=replace(DemMedIncome,DemMedIncome==0, NA))
  
  data <- as.data.frame(data)  # for building DM models
  row.names(data) <- data$ID
  data <- data %>% select(-ID) # use var name for consistency
  
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
 

  
  ###########################
  ######## UPDATES! #########
  ###########################
  
  # numeric impute: By mean #
  vars_imp <- data.imp %>% select_if(is.numeric) %>% replace_na(as.list(Mean))
  data.imp[names(vars_imp)] <- vars_imp
  

  
  # create missing value flag #
  data.imp[paste(vars.na, "NA", sep=".")] <- ifelse(is.na(data.xf[vars.na]), 1, 0)  
  data.imp <- data.imp %>% mutate(across(ends_with(".NA"), as.factor))
  
  ##################################
  ###### Logistic Regression #######
  ##################################
  
  data.mdl<-data.imp # pass on data

  reg.bwd.prob<-predict(reg.bwd, data.mdl, type = "response")
  reg.bwd.class <- as.factor(ifelse(reg.bwd.prob > 0.5, 1, 0))

  raw %>% 
    mutate(pred.prob=reg.bwd.prob,
           pred.class=reg.bwd.class)
 
  
  ##################
  ###### ANN #######
  ##################
  
  data.ann<-data.imp

  vars.ann<-attr(terms(reg.bwd), "term.labels") # extract variable names from bwd model
  vars.ann<-c(vars.ann,"StatusCat96NK")

  ## Standardization: numeric inputs ##
  library(caret)
  data.ann <- data.ann %>% predict(ScaleParams, .)

  ## Dummy Encoding: nominal inputs ##
  dummy <- dummyVars( ~ . , data = data.ann[vars.ann], fullRank = TRUE)
  data.ann.encode <- data.ann %>% predict(dummy, .)
  
  ## Prepare score set as matrix ##
  x.valid<-data.ann.encode

  library(tensorflow)
  library(keras)
  ann.select <-load_model_hdf5("my_ann.h5")
  ann.prob<-ann.select %>% predict(x.valid) 
  ann.class<-as.factor(ifelse(ann.prob > 0.5, 1, 0))

  raw %>% 
    mutate(pred.prob=ann.prob,
           pred.class=ann.class)
}



############### Model Implementation ##############
library(tidyverse)
data.score<-read_csv("scorepva97nk.csv", na=c(".", "NA", "", "?"))
print(data.score, width = Inf)

data.score.pred<- scoring(data.score, Mean, reg.bwd, ScaleParams)
summary(data.score.pred)

write.csv(data.score.pred, file = "pva97nk_scored.csv",row.names=FALSE)









