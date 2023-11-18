library(arules)

# part a
library(tidyverse)
store <- read_csv("transactions.csv")

# part b
store <- store %>% mutate(across(2:4, as.factor))

# part c
Transaction<-as(split(store$Product,store$Transaction), "transactions")
rules = apriori(data = Transaction, parameter = list(support = 0.01, confidence = 0.1))
rules

# part d
inspect(sort(rules, by = 'support'))

inspect(sort(rules, by = 'lift'))

inspect(sort(rules, by = 'confidence')[1:10])






