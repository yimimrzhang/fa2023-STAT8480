#############################################
######## Scoring (class imbalance) ##########
#############################################

load("myscore_raw.RData")

# Create scoring function
scoring <- function(data, Mean, reg.bwd, regThresh){
  raw <- data
  
  ###################
  ### Preparation ###
  ###################
  
  data<- data %>% 
    mutate(across(where(is.character) & !c(StatusCat96NK, DemGender, DemHomeOwner),parse_number)) %>% 
    mutate(across(where(is.character) | c(StatusCatStarAll, DemCluster), as.factor))
  
  data <- data %>% mutate(DemMedIncome=replace(DemMedIncome,DemMedIncome==0, NA))
  
  data <- as.data.frame(data)  # for building DM models
  row.names(data) <- data$ID
  data <- data %>% select(-c(ID,TargetB,TargetD)) # use var names for consistency
  
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
  vars_imp <- data.imp %>% select_if(is.numeric) %>% replace_na(as.list(Mean))
  data.imp[names(vars_imp)] <- vars_imp
  
  
  # create missing value flag #
  data.imp[paste(vars.na, "NA", sep=".")] <- ifelse(is.na(data.xf[vars.na]), 1, 0)  
  data.imp <- data.imp %>% mutate(across(ends_with(".NA"), as.factor))
  
  ##################################
  ###### Logistic Regression #######
  ##################################
  
  data.mdl<-data.imp # pass on data
  
  reg.bwd.prob<-predict(reg.bwd,data.mdl, type = "response")
  reg.bwd.class <- as.factor(ifelse(reg.bwd.prob >= regThresh$threshold, "1","0"))
  
  raw %>% 
    mutate(pred.prob=reg.bwd.prob,
           pred.class=reg.bwd.class)
}


############### Model Implementation ##############


data.score<-read_csv("pva97nk_raw.csv", na=c(".", "NA", "", "?"))

data.score.pred <- scoring(data.score, Mean, reg.bwd, regThresh)
summary(data.score.pred)











