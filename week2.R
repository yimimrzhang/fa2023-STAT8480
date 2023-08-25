## import data ##
library(tidyverse)
data<-read_csv("pva97nko.csv", na=c(".", "NA", "", "?"))

print(data, n=6, width = Inf)  # display top six observations

## data cleaning and define measure levels ##
data<-data %>% 
  mutate(across(where(is.character) & !c(StatusCat96NK,DemGender,DemHomeOwner), parse_number))  %>%
  mutate(across(where(is.character) | c(TargetB,StatusCatStarAll,DemCluster), as.factor))


# logical operators
# “|” is the logical operation OR
# “&” is the logical operation AND
# “!” is the logical operation NOT

str(data)

### Data Exploration ###

## summary statistics ##

# basic summary statistics
summary(data)

# detailed summary statistics
#install.packages("fBasics")
library(fBasics)
data %>% select_if(is.numeric) %>%  basicStats  # only for numeric var.

data %>% select_if(is.numeric) %>%  skewness(na.rm=TRUE)
data %>% select_if(is.numeric) %>%  kurtosis(na.rm=TRUE)

## distribution plots ##

# histogram
ggplot(data, aes(DemMedIncome)) + geom_histogram(bins=100)
ggplot(data, aes(DemAge)) + geom_histogram(bins=100)

# bar chart
ggplot(data, aes(reorder(StatusCat96NK, StatusCat96NK, function(x) -length(x)))) + 
  geom_bar() +
  xlab("StatusCat96NK") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "green")



## missing values exploration ##
missprop<-data %>% summarise(across(everything(), ~ sum(is.na(.))/length(.)))
print(missprop, width = Inf)


#install.packages("Amelia")
library(Amelia)
missmap(data, main="Missing Map")


### data modification/correction ###

## Replace DemMedIncome ##
data <- data %>% mutate(DemMedIncome=replace(DemMedIncome,DemMedIncome==0, NA))



### Optional: save selective objects ###
save(data,missprop, file="mysession.RData")
load("mysession.RData")
