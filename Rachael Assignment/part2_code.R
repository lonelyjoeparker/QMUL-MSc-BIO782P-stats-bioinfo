#read file and check variables
luciferase <- read.table("322_part2.tdf", header = T)
str(luciferase)

#easily accessible variables
attach(luciferase)

#plot
par(mfrow = c(1,1))
plot(subst., expr_units, pch = 16, col = "darkorange",
     xlab = "Average number of amino acid substitutions",
     ylab="Expression Levels")

#test correlation
cor.test(subst., expr_units)

#linear regression model and check diagnostic plots
regress.lm <- lm(expr_units~subst.)
par(mfrow=c(2,2))
plot(regress.lm)
summary(regress.lm)
#residual vs fitted indicates some curvature

#transformation
regress.lm2 <- lm(expr_units~subst. + I(subst.^2))
par(mfrow=c(1,2))
plot(regress.lm2)
summary(regress.lm2)
anova(regress.lm2)
#quadratic coefficient term shows significance and the plots look better 

#check out plots together
par(mfrow=c(1,2))
plot(regress.lm)
plot(regress.lm2)
# transformed data definitely looks better

#anova to confirm the better model
anova(regress.lm, regress.lm2)

#line function from summary
line_func = function(x){4.42100 + 0.911552*x +0.07229*x^2}

#plot line
par(mfrow=c(1,1))
plot(subst., expr_units, pch = 16, col = "darkorange",
     xlab = "Average Amino Acid Substitutions", xlim = c(0,10),
     ylab="Luciferase Expression Levels")
curve(line_func, add=T, col="blue")

# expression exponentially increases exponentially with genetic distance.
# assumptions:
# normal error
par(mfrow=c(2,1))
hist(regress.lm2$residuals)
hist(regress.lm$residuals)
# or QQ plot - line looks straight so all good
#homogeneity of var
#ressiduals vs fitted - line not perfect but better than non logged plot
#linearity
#definitely not!
# substitutions have additive effect? 