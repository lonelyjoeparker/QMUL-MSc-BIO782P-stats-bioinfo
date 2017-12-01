luciferasedata <- read.table("909_part2.tdf", header = T) #open the file.
str(luciferasedata) #make sure it's all good.
plot(luciferasedata) #look at a plot.

luciferasemodel <- lm(luciferasedata$subst.~luciferasedata$expr_units) #make a model.

plot(luciferasemodel) #plot the model.
summary(luciferasemodel) #look at summary for the coefficients for a line.

luciferasemodel2 <- lm(luciferasedata$subst.~luciferasedata$expr_units+I(luciferasedata$expr_units^2)) #make a exponential model.
plot(luciferasemodel2) #look at the plot again, tiny bit better.
summary(luciferasemodel2) #get the coefficients for a curved line.

plot(luciferasedata, ylab = "Substitutions", xlab = "Expression Units") #plot the data.
abline(-1.00913, 0.54459, col = "red") #add a line.

plot(luciferasedata, ylab = "Substitutions", xlab = "Expression Units")
linefunction = function(x){-2.472018+0.796946*x+-0.009660*x^2} #function to make the curved line.
curve(linefunction, add = T, col = "blue") #add the curved line.