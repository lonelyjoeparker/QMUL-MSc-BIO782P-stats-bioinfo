#importing the data
dataset_rna_expr<- read.table("75_part2.tdf", header=TRUE)

# useful to check whether the dataframe contains correct data types (factors/num)
str(dataset_rna_expr)
attach(dataset_rna_expr)

par(mfrow=c(1,1))
#Visualising the data
plot(subst.,expr_units,main="'luciferase' homologue expression with change in genetic distance", xlab="Amino Acid Substitutions", ylab="RNA expression Levels", col="blue",pch=16)

# linear regression model created and placed into a variable
model1 <- lm(expr_units~subst.) 
#summary table for the regression model, showing the coefficients and statistical probability that it is significant
summary(model1) 
#displays anova table
anova(model1) 

#fits a  line to the graph, can also use abline(model1)
abline(coef(lm(expr_units~subst.)),col="red")
#R-Squared value, calculated further down
legend(0,20,"R^2: 0.974")
#coef(lm(expr_units~subst.))

#Allows for configuration of graphics windows to show multiple graphs in 1 window
par(mfrow=c(2,2))
plot(model1)

#Check to see the attributes
attributes(model1)
#calculates the co-efficient of determination to see how well the Y values are described by X values
summary(model1)$r.squared 
#places standardised residuals into a variable, this is the same data from the normal Q-Q plot
standard_residuals<- rstandard(model1) 

par(mfrow=c(1,1))
#view the residuals as a histogram 
hist(standard_residuals)




# Log transformations were conducted as well as sqrt, 
# however results came out worse, failing more than 1 model asummption

#log_res<- log(dataset_rna_expr)
#plot(log_res$subst.,log_res$expr_units) # slight curve
#model1_log<- lm(log_res$expr_units~log_res$subst.)
#summary(model1_log)
#plot(model1_log)

#sqrt_res<- sqrt(dataset_rna_expr$expr_units)
#plot(subst.,log_res) # slight curve
#model1_sqrt<- lm(sqrt_res~subst.)
#summary(model1_sqrt)
#plot(model1_sqrt)
