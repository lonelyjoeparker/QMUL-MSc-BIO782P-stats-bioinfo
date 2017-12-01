# clear the environment
rm(list=ls())
# install ggplot2 if not already installed
install.packages(c("ggplot2"))

# reset the plot window
par(mfrow=c(1,1))

# load ggplot2
library(ggplot2)

# import the data
RNA_exp <- read.table("596_part2.tdf", header=TRUE)
# look at the first ten rows
head(RNA_exp)
# check the data types
str(RNA_exp)
attach(RNA_exp)

hist(expr_units)
hist(subst.)

# Take an inital look at a plot
ggplot(RNA_exp, aes(x=subst., y=expr_units))+
  geom_point()+
  xlab("Genetic distance (amino acid substitutions)")+
  ylab("Putative 'luciferase' Homologue Expression")

# correlation
cor(expr_units, subst.)
# 0.9734458
# strong positive correlation

# propose a linear model (expr_units~subst.)
# look at the coefficients of the linear model
lm(expr_units~subst.)

# check the diagnostic plots
par(mfrow=c(2,2))
plot(lm(expr_units~subst.))

# reset the plot window
par(mfrow=c(1,1))

# histogram of residuals also shows the positive skew shown on the QQ-plot
hist(lm(expr_units~subst.)$residuals)

# is our model statistically significant?
model_one <- lm(expr_units~subst.)
model_one
# Coefficients:
# (Intercept)       subst.  
#      2.939        1.598  
summary(model_one)
# F(1,28) = 506.3, p < 0.001
# it is statistically significant


# plot the graph with the linear model
ggplot(RNA_exp, aes(x=subst., y=expr_units))+
  geom_point()+
  xlab("Genetic distance (amino acid substitutions)")+
  ylab("Putative 'luciferase' Homologue Expression")+
  geom_abline(slope = 1.598, intercept = 2.939)+
  theme_bw()
 

# does log transforming the data make a difference?
ggplot(RNA_exp, aes(x=subst., y=log10(expr_units)))+
  geom_point()+
  xlab("Genetic distance (amino acid substitutions)")+
  ylab("Putative 'luciferase' Homologue Expression")

# check the diagnostic plots again
par(mfrow=c(2,2))
plot(lm(log10(expr_units)~subst.))
par(mfrow=c(1,1))

# propose a linear model for the logged data
log_model <- lm(log10(expr_units)~subst.)
log_model
# Coefficients:
#  (Intercept)       subst.  
#     0.65586      0.06904  
summary(log_model)
# (F(1,28)= 264.1, p<0.001)


# plot the graph with the linear model
ggplot(RNA_exp, aes(x=subst., y=log10(expr_units)))+
  geom_point()+
  xlab("Genetic distance (amino acid substitutions)")+
  ylab("Putative 'luciferase' homologue expression")+
  geom_abline(slope = 0.06904, intercept = 0.65586)+
  theme_bw()

# Is there a curved relationship? 
curve_log_model <- lm(log10(expr_units)~subst.+I(subst.^2))
curve_log_model
# Coefficients:
#  (Intercept)       subst.  I(subst.^2)  
#    0.588026     0.106206    -0.003691  
summary(curve_log_model)
# (F(2,27)= 153.6, p<0.001)

# plot the graph with the curve
ggplot(RNA_exp, aes(x=subst., y=log10(expr_units)))+
  geom_point()+
  xlab("Genetic distance (amino acid substitutions)")+
  ylab("log10 Putative 'luciferase' homologue expression")+
  stat_function(fun=function(x) 0.588026 + x*0.106206 -0.003691*x**2)+
  theme_bw()
# This looks like more of a fit, but does it make sense biologically?
# Would need more data points.

