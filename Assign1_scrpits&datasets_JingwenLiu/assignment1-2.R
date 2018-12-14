###Week 1 Assessment###

#Read and attach the Dataset2 tdf file into R
tableExpr=read.table("part_2_student_1204.tdf.txt",header=T)
attach(tableExpr)

#Check whether the data have been read in properly and are they in proper format
head(tableExpr)
str(tableExpr)

#clean the data
Expression=as.numeric(tableExpr$expression_fold)

#First do some exploratory data analysis(EDA)
#Visualize the relationship between two variables by using plot
plot(distance, Expression, pch=16, col="steelblue",
     main= "Relationship between Luciferase's RNA Expression and their relative Genetic Distance within species of Brassicaceae", xlab="Genetic distances", ylab="RNA Expression Fold")

#fit a linear model and test,vasualize the goodness of fit
par(mfrow=c(1,1))
modelExprDis=lm(Expression~distance)
summary(modelExprDis)
abline(modelExprDis)

#check the diagnostic plots for the regression using plot()
par(mfrow=c(2,2))
plot(modelExprDis)
par(mfrow=c(1,1))

#trnasform the data and try another model
#modelExprDis1=lm(Expression~log(distance))
#summary(modelExprDis1)
#abline(modelExprDis1)
#par(mfrow=c(2,2))
#plot(modelExprDis1)
#par(mfrow=c(1,1))
# This model is worse than last one based on the residuals vs fitted plot
