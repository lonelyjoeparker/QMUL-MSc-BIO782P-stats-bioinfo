install.packages("gvlma") # Checks assumptions on a given linear model
install.packages("rcompanion") # So I can use the plotNormalHistogram function
library(gvlma)
library(rcompanion)
### Dataset 1: Marine microbial diversity ###
# setwd() to the directory you wish to work from and start
Dataset1 <- read.table("190_part1.tdf", header = T)
str(Dataset1)
attach(Dataset1) # makes life easier
plotNormalHistogram(diversity) # # easier to visualise histograms and immediately identify any skew
# distribution of diversity is slightly negatively skewed, but nothing problematic

# Question 1:
boxplot(diversity~latitude, xlab="Latitude", ylab="Diversity", col="steelblue") # steeblue is the best colour
Model1 <- lm(diversity~latitude)
gvlma::gvlma(Model1) # All assumptions are satisfied 
par(mfrow=c(2,2))
plot(Model1) # A better visual of model fit, which is acceptable
par(mfrow=c(1,1))
anova(Model1)

# Question 2:
boxplot(diversity~season, xlab="Season", ylab="Diversity", col="steelblue")
Model2 <- lm(diversity~season)
gvlma::gvlma(Model2) # All assumptions are satisfied 
par(mfrow=c(2,2))
plot(Model2) # Model2 has an acceptable fit
par(mfrow=c(1,1))
anova(Model2)

# Question 3:
Model3 <- lm(diversity~latitude*season) 
anova(Model3)
detach(Dataset1) # makes life easier
