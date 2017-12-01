library(gvlma) 
library(rcompanion)
### Dataset 2: Pairwise nucleotide substitutions and RNA expression levels ###
# setwd() to the directory you wish to work from and start
Dataset2 <- read.table("190_part2.tdf", header = T)
str(Dataset2)
attach(Dataset2)
plotNormalHistogram(expr_units) # looks good
plotNormalHistogram(subst.) # has a slight positive skew
# it all looks normally distributed
Model4 <- lm(expr_units~subst.)
Model5 <- lm(expr_units~subst.+I(subst.^2)) 
summary(Model4)$r.squared # value of 0.9215855
summary(Model5)$r.squared # value of 0.9216019
# minimal difference in R squared value
par(mfrow=c(2,2))
plot(Model4) # Model4 has an acceptable fit, two outliers skewing data in Residuals vs Leverage plot
plot(Model5) # Model5 has an acceptable fit, two outliers skewing data in Residuals vs Leverage plot
par(mfrow=c(1,1))
# for peace of mind
anova(Model4, Model5)
# curved model is not significant over the linear model (p = 0.9406), 
# so we accept that there is no evidence of curvature in these data
plot(expr_units~subst., xlab="Genetic distance (amino acid substitutions)", 
     ylab="Luciferase expression change", pch=16, col="blue" )
abline(Model4, col="red", lwd=2) # Model4 is the preferred model

# Question 1:
cor.test(expr_units, subst.)
# correlation coefficient of 0.96 and a p-value (2.2e-16).

# Question 2:
# Model assumptions are valid. Residuals are zero, homoscedasticity of residuals...etc.
gvlma::gvlma(Model4)
# All the assumptions have been satisfied.

# Question 3:
# Luciferase (LUC) catalyses the ATP-dependent oxidative decarboxylation of luciferin, 
# a reaction that produces light.
detach(Dataset2)
