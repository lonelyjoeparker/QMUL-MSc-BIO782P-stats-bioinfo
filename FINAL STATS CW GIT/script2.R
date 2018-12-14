#set wd to "source file location"

NUC=read.table("part_2_student_1725.tdf.txt", header=T)

#check data to ensure all values are of the correct type
str(NUC)
head(NUC)
names(NUC)

#name variables
distance=NUC$distance
expression_fold=NUC$expression_fold

hist(expression_fold) #does it need to be NORMAL?

#Scatter plot to visualise how genetic distance affects expression fold.
plot(expression_fold ~ distance, main="Scatter plot to show how expression changes with distance")
#does not appear to be any obvious correlation between the two variables. 


#Fit linear regression model
nuc_1=lm(expression_fold ~ distance)

#Check statistical significance of regression model.
anova(nuc_1)
#ANOVA: How much of the variance in the data is explained by the regression line comapred to error/residuals/ how well the model fits the data. 
#p > 0.05 - model not statistically significant

summary (nuc_1)
#Line equation:(expression=0.03472-0.08509*distance)

#Plot line for the regression model onto scatter plot
plot(expression_fold ~ distance, xlab = "Genetic distance from putative luciferase gene ", ylab="Expression fold change of luciferase homologue")
abline(nuc_1, col="red")

#test correlation
cor.test(distance,expression_fold)
#shows negative correlation but p value shows non-significance





#Diagnostics
par(mfrow=c(2,2))
plot(nuc_1)
par(mfrow=c(1,1))
#HOMOSCEDASTICITY:residuals bounce randomly around the 0 line (homoscedasticity) and  they roughly form a horizontal band around the 0 line 
#LINEAR RELATIONSHIP:slight hint of curvature (in 2 places, so possible third order polynomial) - will explore this later
#NORMAL DISTRIBUTION OF ERRORS: residuals are approximately normally distributed (mostly fall on the diagonal line) with some deviation from normality at tails only.

#Check normality with a histogram of residuals. 
hist(nuc_1$residuals)
#shows some positive skew

#2nd ORDER POLYNOMIAL
nuc_squared=lm(expression_fold ~ distance + I(distance^2))
anova(nuc_squared)
summary(nuc_squared)
#R sqaured=0.0385
#quadratic term not significant
par(mfrow=c(2,2))
plot(nuc_squared)
#residuals v fitted is still curved/ normal QQ plot looks worse/influencial outlier in last plot
par(mfrow=c(1,1))


#3rd Order Polynomial
nuc_cubed=lm(expression_fold ~ distance + I(distance^2)+I(distance^3))
par(mfrow=c(2,2))
summary(nuc_cubed)
#R sqaured=0.093
plot(nuc_cubed)
#slight improvement in QQ plot, residuals v fitted still slightly curved/influencial outlier seen in last plot  

#HISTOGRAM OF RESIDUALS FOR CUBED MODEL
hist(nuc_cubed$residuals)
#bell shaped-residuals normally distributed

#Curve equation / add curve to scatter plot 
plot(expression_fold ~ distance)
curve(-0.28131+0.76910*x-0.46522*x^2+0.06737*x^3,add=TRUE, col="red")
abline(nuc_1, col="blue") 
#visually, the curve fits better, and has a larger R squared, but, the quadratic terms are not significant. 

#2nd and 3rd order polynomial terms non justifiable
#model with 3rd order polynomial increases the R squared, but not by that much to justify the increase in complexity of the model, also Residuals v Leverage plot shows an influencial outlier
#stick to original linear model

#Other transformations 

#log+1 
nuc_log=lm(log(expression_fold+1) ~ distance)
summary(nuc_log)
plot(nuc_log) 
plot(expression_fold~distance) #No imporvement in diagnostic plots

#SQUARE ROOT
nuc_sqrt=lm(sqrt(expression_fold+1) ~ distance)
anova(nuc_sqrt)
par(mfrow=c(2,2))
plot(nuc_sqrt)
#improvement in Normal QQ plot/no influencial outliers 
#transformation improves model
par(mfrow=c(1,1))
summary(nuc_sqrt)

#Plot regression line on scatter plot 
plot(sqrt(expression_fold+1)~distance, pch=16, xlab = "Genetic distance from putative luciferase gene ", ylab="Expression fold change of luciferase homologue")
abline(nuc_sqrt, col="red")
