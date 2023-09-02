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
################ Week 3 ####################
############################################

################################################
## Data Exploration II: Bivariate Exploration ##
################################################

## Use Graphics ##

# Side-by-side Boxplot of DemAge by TargetB
ggplot(data, aes(x=TargetB, y=DemAge, fill=TargetB)) +
  geom_boxplot(notch=TRUE)



# Grouped barplot of StatusCat96NK by TargetB

# Order factor levels by freq
StatusCat96NK_order<-reorder(data$StatusCat96NK, data$StatusCat96NK, function(x) -length(x))

ggplot(data, aes(x=StatusCat96NK_order, fill=TargetB)) + 
  geom_bar(position="dodge") 

# ggplot(data, aes(x=DemGender, fill=TargetB)) + 
#   geom_bar(position="dodge") 



# Mosaic plot
#install.packages("ggmosaic")
library(ggmosaic)

# Order factor levels by freq
# StatusCat96NK_order<-reorder(data$StatusCat96NK, data$StatusCat96NK, function(x) -length(x))

ggplot(data) +
  geom_mosaic(aes(x=product(StatusCat96NK_order), fill=TargetB))



## Summary statistics by Target variable ##
library(skimr) 
data %>% group_by(TargetB) %>%  skim


#################################
## Variable Importance Measure ##
#################################

## Binary Target ##

# nominal input - chi-square
data %>% 
  summarise(across(where(is.factor), ~ chisq.test(.,TargetB)$p.value)) %>% 
  unlist %>% 
  sort


# numeric input - t stat
data %>% 
  summarise(across(where(is.numeric) & !c(ID, TargetD), ~ t.test(.~TargetB)$p.value)) %>%
  unlist %>% 
  sort


# numeric input - area under ROC curve for predicting target
# install.packages("caret")
library(caret)
data %>% 
  select(TargetB, where(is.numeric)) %>%  
  filterVarImp(.$TargetB) %>%  
  slice(-(1:3)) %>% 
  arrange(desc(X1)) 


## Numeric Target ##

# numeric input - Pearson correlation
data %>% 
  summarise(across(where(is.numeric), ~ abs(cor(.,TargetD, use = "complete.obs")))) %>% 
  unlist %>% 
  sort(decreasing = TRUE)


# numeric input - Lowess R^2
library(caret)
data %>% 
  filter(!is.na(TargetD)) %>% 
  select(where(is.numeric)) %>% 
  filterVarImp(.$TargetD, nonpara = TRUE) %>% 
  slice(-(1:2)) %>% 
  arrange(desc(Overall)) 
  

# categorical input - ANOVA F-stat
data %>% 
  summarise(across(where(is.factor) & !TargetB, ~ summary(aov(TargetD ~ .))[[1]][["Pr(>F)"]][1])) %>% 
  unlist %>% 
  sort




################################
#### Variable Transformation ###
################################

# Box-Cox Transformation on numeric inputs

library(caret)
TransformParams <- data %>% 
                     as.data.frame %>%   # preProcess works better with dataframe than tibble
                     select(-(1:3)) %>% 
                     preProcess(method=c("BoxCox"))
TransformParams$bc

# TransformParams1 <- data %>% 
#                      as.data.frame %>%   
#                      select(-(1:3)) %>% 
#                      preProcess(method=c("YeoJohnson"))
# TransformParams1$yj

TransformParams <- data %>% 
                     as.data.frame %>%   
                     select(GiftAvgAll,GiftAvgCard36) %>% 
                     preProcess(method=c("BoxCox"))
data.xf <- data %>%  
  as.data.frame %>%  
  predict(TransformParams,.) %>% 
  as_tibble


# Histograms before/after transformation
par(mfrow=c(2,2))
hist(data$GiftAvgAll)
hist(data.xf$GiftAvgAll)
hist(data$GiftAvgCard36)
hist(data.xf$GiftAvgCard36)
par(mfrow=c(1,1))


# Stats before/after transformation
library(fBasics)
data %>% select(GiftAvgAll,GiftAvgCard36) %>% basicStats
data.xf %>% select(GiftAvgAll,GiftAvgCard36) %>% basicStats

# Transformation on nominal inputs

# Default Factor Order: Alphabetical #
levels(data.xf$StatusCat96NK)    
levels(data.xf$StatusCat96NK)<-c("A", "E", "F", "A", "E", "F")


## Filter Obs ##

data.fltr<-data %>% filter(DemGender!="U")
data.fltr<-data %>% filter(DemAge>=18)
data.fltr<-data %>% filter(DemAge>=18 & DemGender!="U")



#####################
##### Imputation ####
#####################

library(Amelia)
missmap(data, main="Missing Map")

# Nominal Input: By Mode #
mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

data.imp<-data %>% 
  mutate(across(where(is.factor) & !(1:3), ~replace_na(.,mode(.))))



# Numeric Input: By Mean #
data.imp<-data %>% 
  mutate(across(where(is.numeric) & !(1:3), ~replace_na(.,mean(.,na.rm = TRUE))))



# Numeric Input: By Median #
library(caret)

ImputeParams <- data %>% 
  select(-(1:3)) %>% 
  preProcess(method=c("medianImpute"))

data.imp <- data %>% predict(ImputeParams,.)
  

# Create Missing Value Flag #

data.imp <- data %>%
  mutate(GiftAvgCard36.NA=ifelse(is.na(GiftAvgCard36), 1, 0),
         DemAge.NA=ifelse(is.na(DemAge), 1, 0),
         DemMedIncome.NA=ifelse(is.na(DemMedIncome), 1, 0))

