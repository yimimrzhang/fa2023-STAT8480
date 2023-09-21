##########################
####### Quiz Four #######
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


# part a: Checking missing values #

vars.na <- organics %>% filter(split) %>% select_if(colSums(is.na(.))>0) %>% names
vars.na 


# part c: Imputation #

organics.imp<-organics

mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

organics.imp <-organics.imp %>% 
  mutate(across(where(is.factor) & !TargetBuy, ~replace_na(.,mode(.[split])))) %>%  # Nominal Input: By Mode #
  mutate(across(where(is.numeric), ~replace_na(.,mean(.[split],na.rm = TRUE))))     # Numeric Input: By Mean #

# Create Missing Value Flag #
organics.imp[paste(vars.na, "NA", sep=".")] <- ifelse(is.na(organics[vars.na]), 1, 0)  


# part d #

organics.mdl<-organics.imp

levels(organics.mdl$TargetBuy) 

full = glm(TargetBuy ~ ., family=binomial, data=organics.mdl[split,])
summary(full)

null<-glm(TargetBuy ~ 1, family=binomial, data=organics.mdl[split,])


# Stepwise selection
reg.step <- step(null, scope=formula(full), direction="both", trace = FALSE)
summary(reg.step)

# Variable importance
library(caret)
varImp(reg.step) 
varImp(reg.step) %>% arrange((desc(Overall)) )

# Validation MISC
reg.step.prob<-predict(reg.step,organics.mdl[!split,], type = "response") 
reg.step.class <- ifelse(reg.step.prob > 0.5, 1, 0)
reg.step.misc<- 1-mean(reg.step.class == organics.mdl$TargetBuy[!split])
reg.step.misc


# part e-f: Transformation #

organics.xf<-organics

organics.xf <- organics.xf %>% 
  mutate(across(c(PromTime, PromSpend), ~ log(.+1)))

par(mfrow=c(2,2))
hist(organics$PromSpend[split])
hist(organics.xf$PromSpend[split])
hist(organics$PromTime[split])
hist(organics.xf$PromTime[split])
par(mfrow=c(1,1))

library(fBasics)
organics %>% 
  filter(split) %>% 
  select(c(PromTime, PromSpend)) %>% 
  summarise(across(everything(), 
                   list(skewness= ~ skewness(.,na.rm = TRUE),
                        kurtosis= ~ kurtosis(.,na.rm = TRUE))))

organics.xf %>% 
  filter(split) %>% 
  select(c(PromTime, PromSpend)) %>% 
  summarise(across(everything(), 
                   list(skewness= ~ skewness(.,na.rm = TRUE),
                        kurtosis= ~ kurtosis(.,na.rm = TRUE))))           
            


# part g #

#### Imputation #####

organics.imp<-organics.xf

organics.imp <-organics.imp %>% 
  mutate(across(where(is.factor) & !TargetBuy, ~replace_na(.,mode(.[split])))) %>%  # Nominal Input: By Mode #
  mutate(across(where(is.numeric), ~replace_na(.,mean(.[split],na.rm = TRUE))))     # Numeric Input: By Mean #

# Create Missing Value Flag #
organics.imp[paste(vars.na, "NA", sep=".")] <- ifelse(is.na(organics.xf[vars.na]), 1, 0)  


#### Modeling #####

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


