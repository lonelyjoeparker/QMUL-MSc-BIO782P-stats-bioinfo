rm(list = ls())

marine <- read.table("897_part1.tdf.txt", header = TRUE)
attach(marine)

#checking variables and seeing top few lines of table
str(marine) 
head(marine)

#boxplots to see how diversity changes with season and lattitude respective
par(mfrow=c(1,2))

dvs <- boxplot(diversity~season, main="Diversity against Season",xlab="Season",ylab="Diversity")
dvl <- boxplot(diversity~lattitude, main="Diversity against Lattitude",xlab="Lattitude",ylab="Diversity")

#normality test
shapiro.test(diversity)

#barplot to examine 
barplot(diversity,names.arg = season,xlab = "Season",ylab = "Diversity",col = "blue",
        main = "Variation of Microbial Diversity with Time of the year", border = "red")

#t-test
test=t.test(diversity ~ season)
test

#linear regression model to test relation between season and location
model1 <- lm(diversity~season*lattitude)
summary(model1)
