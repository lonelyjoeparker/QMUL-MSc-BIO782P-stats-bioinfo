luciferase <- read.table("572_part2.tdf", header = TRUE) ##import data, specifying the first row as headers
str(luciferase) #check the number of variables and observations...30 obs 2 variables
attach(luciferase)
par(mfrow=c(1,1)) #set graph space
#inital scatterplot of data to check if there is any visual liklehood of a linear regression
plot(expr_units~subst., xlab="No. of nucleotide substutions", ylab="Expression level", main="Luciferase RNA expression relative to number of pairwise nucleotide substititions")
cor.test(subst.,expr_units) #find the correlation coefficient for the data, to estimate a likely correlation?

lucimod1 <- lm(expr_units~subst.) #create a linear model
summary(lucimod1) #R sqaured value has high correlation and low significant p value
par(mfrow=c(2,3)) #set graph space
#re-plot scatter graph
plot(expr_units~subst., xlab="No. of nucleotide substutions", ylab="Expression level", main="Luciferase RNA expression relative to number of pairwise nucleotide substititions")
abline(lucimod1, col="blue") #add linear regression model line of best fit to the scattergraph
plot(lucimod1) #residual plots for the model
plotNormalHistogram(lucimod1$residuals, main = "Histogram of residuals") #plot a histogram of the residuals, to highlight a normal distribution

#take out the extra data point!
luciferase_update <- luciferase[-21,] #remove row 21 as outlier is high
attach(luciferase_update)
lucimod2 <- lm(expr_units~subst.) #re-create the linear model with the outlier removed
summary(lucimod2) #still good R squared and low p value
par(mfrow=c(2,3)) #set graph space
#re-plot scatter graph
plot(expr_units~subst., xlab="No. of nucleotide substutions", ylab="Expression level", main="Luciferase RNA expression relative to number of pairwise nucleotide substititions")
abline(lucimod2, col="blue") #add linear regression model line of best fit to the scattergraph
plot(lucimod2) #residual plots for the model
hist(lucimod2$residuals, main = "Histogram of residuals") #plot a histogram of the residuals, to highlight a normal distribution

#plot fancy graph!
install.packages("ggplot2")
library(ggplot2)
ggplot(luciferase_update, aes(x=subst., y=expr_units)) + geom_point()+ geom_abline(intercept=coef(lucimod2)[1],slope=coef(lucimod2)[2])+scale_y_continuous(name = "RNA expression") +scale_x_continuous(name = "No. of pairwise nucleotide substitutions")+ ggtitle("Relationship between RNA expression and Genetic Distance of a Luciferase Homologue")