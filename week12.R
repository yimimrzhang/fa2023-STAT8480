# install.packages('arules')
library(arules)

# Import the data
library(tidyverse)
bank <- read_csv("bank.csv")

bank <- bank %>% mutate(across(1:2, as.factor))

# Convert into transaction data
Transaction<-as(split(bank$SERVICE,bank$ACCOUNT), "transactions")
summary(Transaction)
itemFrequencyPlot(Transaction, topN = 10)

# Train Apriori on the data
rules <- apriori(data = Transaction, parameter = list(support = 0.05, confidence = 0.1))
rules

# Visualize the results
inspect(sort(rules, by = 'lift')[1:10])

library(arulesViz)
plot(rules,  measure=c("confidence", "support"), shading="lift", engine = "interactive")



