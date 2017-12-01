#Open the Table:

luciferasa=read.table(file="403_part2.tdf",header = TRUE)

#Check the variables:
str(luciferasa)
attach(luciferasa)
mean(expr_units)
sd(expr_units)
mean(subst.)
sd(subst.)

#Histogram of the Response variable:
hist(expr_units, main="Histogram of Expression Levels of Luciferase")

ks.test(expr_units,"pnorm",mean(expr_units),sd(expr_units))
#Using the Kolmogorov-Smirnov test it was accepted the Ho: Expr_units follows a normal distribution
#With this test and the histogram it was accepted that diversity follows a normal distribution.

#Since the Response and Explanatory variables are continous, the model is going to be a
#Regression.

#MODEL 1:
model1=lm(expr_units~subst.)
par(mfrow=c(2,2))
plot(model1)
#The homoscedasticity and linearity look good.
anova(model1)
summary(model1)
#The results indicate that is a good model. 

#Plot of Expr_units as a function of subst. with the line obtained for the linear regression:
par(mfrow=c(1,1))
plot(expr_units~subst., main="Expression Levels vs Pairwise Distances",xlab="Pairwise Distances",ylab="Expression Levels",pch=16)
#With the information obtained in summary(model1) is possible to make the ecuation that
#explain the effect of the Explanatory variable over the Response variable.
abline(model1,col="black")

