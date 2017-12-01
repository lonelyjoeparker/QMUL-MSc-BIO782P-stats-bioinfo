setwd("~/Desktop") #set working directory to where the data is saved
pairwise_mutations <- read.table("993_part2.tdf.txt", header = TRUE) #open file by reading data into table format
str(pairwise_mutations) #check structure of data is correct
attach(pairwise_mutations) #attach data so able to use variables directly w/o $
mutations_model <- lm(expr_units~subst.)  #linear model of expression change explained by AA substitution
anova(mutations_model) #anova of the above lm, produces significant statistical values
plot(expr_units~subst., xlab="Genetic distance", ylab="Expression level", main="Effect of genetic distance on luciferase homologue expression levels", col = "orange") #scatterplot of expression change explained by AA substitution
abline(mutations_model, col = "red") #fit regression line
#abline(3.2,1.65, col = "black") #line of best fit, fitted by hand


#To check validity of model assumptions:
plot(expr_units~subst.) #independent and dependent varibles increase linearly
par(mfrow=c(2,2)) #scale margins to see 4 plots on same page
plot(marine_model) 
#normal Q-Q plot shows residuals points close to the line, bar deviation near ends(expected) 
#residuals vs fitted graph - no in/decreasing trend 
par(mfrow=c(1,1)) #scale margins back to normal settings
1/(1-(marine_model$residuals^2)) #multicollinearity - if >2 assumption not valid
library(ggplot2) #loads package for various graphs and tests
acf(marine_model$residuals) #plots estimates of autocorrelation, - no correlation - from lag1 there's a drop to significance level


#automatically checks validity of 5 assumptions
install.packages("gvlma") #install package that assesses lm assumptions
library(gvlma) #loads package into R
gvlma(mutations_model) #carries out assessment of linear model assumptions on lm of expression and substitution