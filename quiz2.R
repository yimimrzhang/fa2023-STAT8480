########################
####### Quiz Two #######
########################

# import organics data #
library(tidyverse)
organics<-read_csv("organics.csv",na=c(".", "NA", "", "?"))

# fix measurement levels
organics <- organics %>% 
  mutate(across(where(is.character) | TargetBuy, as.factor)) 

# part 1a #
# generate mosaic or bar plot 
library(ggmosaic)
ggplot(organics) +
  geom_mosaic(aes(x=product(DemGender), fill=TargetBuy))

# part 1b #
ggplot(organics, aes(x=TargetBuy, y=PromTime, fill=TargetBuy)) +
  geom_boxplot(notch=TRUE)

library(skimr) 
organics %>% 
  group_by(TargetBuy) %>% 
  select(PromTime) %>%
  skim

# part 1c #
organics %>% 
  group_by(TargetBuy) %>% 
  select(PromClass) %>%
  skim



# part 2a #
organics %>% 
  summarise(across(where(is.factor), ~ chisq.test(.,TargetBuy)$p.value)) %>% 
  unlist %>% 
  sort
chisq.test(organics$DemGender, organics$TargetBuy)$statistic


# part 2b #
organics %>% 
  summarise(across(where(is.numeric) & !TargetAmt, ~ t.test(.~TargetBuy, var.equal=FALSE)$p.value)) %>%
  unlist %>% 
  sort

library(caret)
organics %>% 
  select(TargetBuy, where(is.numeric)) %>%  
  filterVarImp(.$TargetBuy) %>%  
  arrange(desc(X1)) %>% 
  slice(-(1:2)) 

# part 2c #
organics %>% 
  summarise(across(where(is.numeric) & !TargetAmt, ~ abs(cor(.,TargetAmt, use = "complete.obs")))) %>% 
  unlist %>% 
  sort(decreasing = TRUE)

organics %>% 
  summarise(across(where(is.numeric) & !TargetAmt, ~ cor(.,TargetAmt, use = "complete.obs"))) 


# part 3a #
library(caret)
TransformParams <- organics %>% 
  as.data.frame %>%   
  select(PromSpend) %>% 
  preProcess(method=c("BoxCox"))
TransformParams$bc

organics.xf<-organics %>%  
  as.data.frame %>%  
  predict(TransformParams,.) %>% 
  as_tibble


# part 3b #
par(mfrow=c(1,2))
hist(organics$PromSpend)
hist(organics.xf$PromSpend)
par(mfrow=c(1,1))


# part 3c #
library(fBasics)
basicStats(organics$PromSpend)
basicStats(organics.xf$PromSpend)







