marine <- read.table("572_part1.tdf", header = TRUE) #import data, specifying the first row as headers
attach(marine)
str(marine) #check the number of variables and observations...5 variables and 20 observations.
hist(diversity) #histogram of diversity to check the initial data distribution
#the data appears normally distributed n first glance, so able to meet the requirements of parametric statistical tests.
par(mfrow=c(1,2)) #set graph area
install.packages("ggplot2")
library(ggplot2)
ggplot(marine, aes(x = lattitude, y = diversity)) + geom_boxplot(fill="steelblue") + scale_y_continuous(name = "Diversity") +scale_x_discrete(name = "Latitude")+ ggtitle("Microbial Diversity Vs Latitiude")
#boxplots depicting interaction between microbial diversity and latitude OR season respectively.
median(diversity[lattitude=="equatorial"]) #calculate median
median(diversity[lattitude=="temperate"]) #calculate median
IQR(diversity[lattitude=="equatorial"]) #calculate intequartile range
IQR(diversity[lattitude=="temperate"]) #calculate interquartile range
#the diversity median value is much higher in equatorial latitude and lower in temperate latitude, however the spread of data is equal. 
ggplot(marine, aes(x = season, y = diversity)) + geom_boxplot(fill="lightgreen") + scale_y_continuous(name = "Diversity") +scale_x_discrete(name = "Season")+ ggtitle("Microbial Diversity Vs Season")
boxplot(diversity~season, main="Microbial Diversity Vs Season", ylab="Diversity")
#the median values for diversity are very similar for both the aug and jan seasons. 
#Measurements of microbial diversity are more varied in the aug season. 
#create a model to analyse any interaction between season and latitude with regards to measures in microbial diversity.
model1 <- lm(diversity~season*lattitude)
#set graph space
par(mfrow=c(2,2))
#analyse distribution of residuals for model. Datalooks normally distributed and QQ plot is acceptable. 
plot(model1)
#analysis of variance on the model to measure the significance of the interaction between season and latitude
#the null hypothesis is that there is no association between season and latitude
model1.aov <- anova(model1)
model1.aov