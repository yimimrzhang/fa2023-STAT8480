########################
####### Quiz One #######
########################

# import organics data #
library(tidyverse)
organics<-read_csv("organics.csv",na=c(".", "NA", "", "?"))

# check measurement levels
str(organics)

# fix measurement levels
organics <- organics %>% 
  mutate(across(where(is.character) | TargetBuy, as.factor)) 



# part c #
organics %>%                   
  slice(1:26) %>%                # select the first 26 rows
  select_if(~any(is.na(.))) %>%  # check ?any for description
  names                          # return column names
# or,
print(organics, n=26, width = Inf)  

# part d #
# 1)
ggplot(organics, aes(reorder(TargetBuy, TargetBuy, function(x) -length(x)))) + 
  geom_bar() +
  xlab("TargetBuy") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white") 


# 2)
ggplot(organics, aes(reorder(DemGender, DemGender, function(x) -length(x)))) + 
  geom_bar() +
  xlab("DemGender") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1.5, colour = "white") 

# 3)
ggplot(organics, aes(DemAge)) + geom_histogram()
summary(organics$DemAge) # check missing values


# 4)
ggplot(organics, aes(PromSpend)) + geom_histogram(bins=50)


# 5)
organics %>% 
  summarise(across(everything(), ~ sum(is.na(.))/length(.))) %>% 
  unlist() %>%             # convert tibble to a vector
  sort(decreasing = TRUE)  # sort missing percent 


# part e #
library(fBasics)
basicStats(organics$PromSpend) 





