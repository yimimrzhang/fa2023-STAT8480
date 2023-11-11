############################
####### Quiz Nine  #########
############################

# parts a & b #
library(tidyverse)
dungaree <- read_csv('dungaree.csv', na=c(".", "NA", "", "?"))
print(dungaree)


# part c #
library(Amelia)
missmap(dungaree)
dev.off()

library(dlookr)
plot_hist_numeric(dungaree)


# part e #
dungaree.scale<-dungaree
dungaree.scale<-dungaree.scale %>% select(-c(STOREID, SALESTOT)) %>% scale



# part f #
set.seed(4321)
km <- kmeans(x = dungaree.scale, centers = 6, nstart=25)
table(km$cluster)  


# part g #
dungaree$cluster <-as.factor(km$cluster)


# Density of Original jeans by cluster
dungaree%>%
  ggplot(aes(x=ORIGINAL)) +  
  geom_density(lty=3) +      
  geom_density(aes(fill=cluster, colour=cluster), alpha=0.5) 

# Density of Fashion jeans by cluster
dungaree%>%
  ggplot(aes(x=FASHION)) +  
  geom_density(lty=3) +      
  geom_density(aes(fill=cluster, colour=cluster), alpha=0.5) 

# Density of Leisure jeans by cluster
dungaree%>%
  ggplot(aes(x=LEISURE)) +  
  geom_density(lty=3) +      
  geom_density(aes(fill=cluster, colour=cluster), alpha=0.5) 

# Density of Stretch jeans by cluster
dungaree%>%
  ggplot(aes(x=STRETCH)) +  
  geom_density(lty=3) +      
  geom_density(aes(fill=cluster, colour=cluster), alpha=0.5) 








