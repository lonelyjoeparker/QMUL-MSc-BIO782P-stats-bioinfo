setwd("~/Desktop") #set working directory to where the data is saved
marine_microbes <- read.table("993_part1.tdf.txt", header = TRUE) #open file by reading data into table format
str(marine_microbes) #check structure of data is correct
attach(marine_microbes) #attach data so able to use variables directly w/o $
boxplot(diversity~season, col="orange", xlab= "Season", ylab= "Microbial diversity", main = "Effect of season on marine microbial diversity") #boxplot of diversity explained by season
boxplot(diversity~lattitude, col="brown", xlab= "Latitude", ylab= "Microbial diversity", main = "Effect of lattitude on marine microbial diversity") #boxplot of diversity explained by lattitude
marine_model <- lm(diversity~season*lattitude) #linear model of diversity(dependent variable) explained by season, lattitude and the interaction between both variables (both are independent variable)
anova(marine_model) #anova of the above lm, produces significant statistical values
interaction.plot(lattitude, season, diversity) # plot of interaction between both variables