#q1
#read txt file
read.table("930_part1.tdf", header=TRUE)
#import file and set name
rawinput=read.table("930_part1.tdf", header=TRUE)
attach(rawinput)
#set parameters for graph
par(mfrow=c(1,1))
#plot diversity in terms of equatorial and temperate locations(non numerical/categorical data must be plotted on x asis)
plot(lattitude,rawinput$diversity, main="Microbial Diversity by Latitude", xlab="Latitude", ylab="Microbial Diversity") 
#plot diversity vs time of year
plot(season,rawinput$diversity, main="Microbial Diversity vs Time of Year", xlab="Time of Year", ylab="Microbial Diversity")
#check any interaction between season and location
relation <- lm(diversity ~ season*lattitude)
#check residuals
hist(relation$residuals)
#see summary of relation
summary(relation)
#plot graphs to check physical interaction
par(mfrow=c(1,2))
plot (diversity~season, col="red")
plot (diversity~latitude, col="blue")
title ("Diversity by Season vs Latitude")
#stripplot to see individual values
stripchart(diversity~season*latitude)
#anova to see variance analysis
anova(relation)

#q2
rawinput2=read.table("930_part2.tdf", header=TRUE)
attach(rawinput2)#attach the data to make things simpler
#use the t test to get a feel for the data
t.test(expr_units,subst.)
#generate linear model to test
exprsubs = lm(expr_units~subst.)
summary(exprsubs) #to look at the details of the lm.
summexper = summary(exprsubs) #save summary as variable 
#plot lm
abline(summexper)
#next I wanted to check to see if the model assumptions were valid so did some tests
par(mfrow=c(2,2)
plot(exprsubs)

#q3
rawinput3=read.table("930_part3.tdf", header=TRUE)
attach(rawinput3)
#conduct a forward and backward stepwise to establish the best model for the HIV results
model <- lm(rawinput3)
fwd.model <- step(model, direction='forward')
bk.model <- step(model, direction='backward')
#found model, now want to see graphical results 
plot(viral_load ~ CD4 + Shannon_diversity + Tissue + Distance)
par(mfrow=c(2,2))
plot(viral_load ~ CD4 + Shannon_diversity + Tissue + Distance)



