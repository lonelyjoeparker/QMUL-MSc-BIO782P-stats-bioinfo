LUCI <- read.table("273_part2.tdf", header = T)
attach(LUCI)

#check the data to make sure the values are the right type
str(LUCI)

#take a look at the data
par(mfrow =c(1,1)) #set viewing parameters for one graph
plot(LUCI)
#looks linear, with no obivious causes for concern.

#fit a model
LUCImod <- lm(expr_units~subst.)

#look at diagnostic plots of model
par(mfrow = c(2,4))
plot(LUCImod)


#residuals vs. fitted looks like it might be a bit fan shaped, or maybe indicative or a true curved relationship
#we'll try fitting with log and cuve to see how they do

#check log and curve (logging subst. has bizarre results so log expr_units)
LUCIlog <- lm(log(expr_units)~subst.)
LUCIcurve <- lm(expr_units~subst.+I(subst.^2))

#look at diagnostic plots for the three models
par(mfrow = c(3,4))
plot(LUCImod)
plot(LUCIlog)
plot(LUCIcurve)
#not convinced that log or curve is better


#let's look at the anova of each model
anova(LUCIlog)
anova(LUCImod)
anova(LUCIcurve)
#the anova for the curve has the I(subst.^2) term as non-significant, so we'll ignore this model
#LUCIlog has lower residuals so we'll pick this model

#make a graph of the log values and the fitted line
par(mfrow =c(1,1)) #set viewing parameters for one graph
plot(log(expr_units)~subst., xlab= "Amino Acid Substitutions", ylab = "Expression Level", col = "steel blue")
abline(1.4166,0.1771, col = "dark blue")


#_____________________________________________________________________________________________________________________________

#Model Assumptions
#1. independance of data, no value depends on any other value (avoid simpson's paradox)

#2. normal errors (look at residuals data in summary) 
#3. homegeneity of variance (variance is the same for the two samples being compared)
#   (no clumping in certain points) (Homoscedasticity)
#These can both be looked at together in the first two plots of the model
par(mfrow = c(2,2))
plot(LUCIlog)


#4. Linear realtionship between response and explanatory variables
#5. The explanatory variable is measured without error
