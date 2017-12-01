# clear the environment
rm(list=ls())
# install ggplot2 if not already installed
install.packages(c("ggplot2"))

# reset the plot window
par(mfrow=c(1,1))

# load ggplot2
library(ggplot2)

# import the data
HIV_data <- read.table("596_part3.tdf", header=TRUE)
# look at the first ten rows
head(HIV_data)
# check the data types
str(HIV_data)
attach(HIV_data)

# take an inital look for any obvious interactions
plot(HIV_data)

# plot(viral_load ~ Shannon_diversity)
ggplot(HIV_data, aes(x = Shannon_diversity, y = viral_load))+
  geom_point()+
  xlab("Shannon diversity")+
  ylab("log10 HIV viral load")
# There is no obvious pattern here.
 

# plot(viral_load ~ Tissue)
ggplot(HIV_data, aes(x = Tissue, y = viral_load))+
  geom_boxplot()+
  geom_point(size = 2, alpha = 0.5)+
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 5)+
  xlab("Tissue")+
  ylab("log10 HIV viral load")
# This is more promising - looks like there is a difference in the means
t.test(viral_load ~ Tissue)
# t = 2.0953, df = 32.392, p-value = 0.04404
# The means are significantly different, 
# this gives us a good starting choice for our model.

#plot(viral_load ~ CD4)
ggplot(HIV_data, aes(x = CD4, y = viral_load))+
  geom_boxplot()+
  geom_point(size = 2, alpha = 0.5)+
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 5)+
  xlab("CD4+ cell count")+
  ylab("log10 HIV viral load")
# not much to see here - means look very similar
t.test(viral_load ~ CD4)
# t = -0.5142, df = 37.084, p-value = 0.6102
# Means are not significantly different.

#plot(viral_load ~ Distance)
ggplot(HIV_data, aes(x = Distance, y = viral_load))+
  geom_point()+
  xlab("Mean pairwise genetic distance")+
  ylab("log10 HIV viral load")
# Doesn't look like there is any pattern here.

# Propose a first model using a range of variables:
model_one <- lm(viral_load ~ Tissue*Distance*CD4)
drop1(model_one, test="F")

# second model with Distance variable dropped:
model_two <- lm(viral_load ~ Tissue*CD4)
drop1(model_two, test="F")

# third model with Tissue:CD4 dropped:
model_three <- lm(viral_load ~ Tissue+CD4)
drop1(model_three, test="F")
# CD4 should be dropped.

# Leaves us with an initial model of viral_load~Tissue:
initial_model <- lm(viral_load ~ Tissue)
# can test beginning with a null model too:
null_model <- lm(viral_load~1)

# perform forward stepwise search to look for a suitable model:
forward_model_initial <- step(initial_model, scope=(~Shannon_diversity*Tissue*CD4*Distance), direction="forward")
summary(forward_model_initial)
anova(forward_model_initial)
# lm(formula = viral_load ~ Tissue)
# F(1,38) = 4.3905, p = 0.04286

# see if going forward from the null model gives a different result:
forward_model_null <- step(null_model, scope=(~Shannon_diversity*Tissue*CD4*Distance), direction="forward")
summary(forward_model_null)
anova(forward_model_null)
# lm(formula = viral_load ~ Tissue)
# F(1,38) = 4.3905, p = 0.04286
# Good, this is the same.

# perform backwards stepwise search to look for a suitable model:
backwards_model <- step(lm(viral_load ~ Shannon_diversity*Tissue*CD4*Distance), direction="backward")
summary(backwards_model)
anova(backwards_model)
# lm(formula = viral_load ~ Shannon_diversity + Tissue + Distance + Shannon_diversity:Distance)
# F(4,35) = 2.076, p = 0.105
# This isn't so good...

# perform convergent stepwise search in both directions, starting from the null model:
convergent_model <- step(null_model, scope=c(lower=~1, upper=~Shannon_diversity*Tissue*CD4*Distance), direction="both")
summary(convergent_model)
anova(convergent_model)
# lm(formula = viral_load ~ Tissue)
# F(1,38) = 4.3905, p = 0.04286
# viral_load ~ Tissue seems to be the best model we can find.

t.test(viral_load ~ Tissue)
tapply(viral_load,Tissue,mean)
# Tissue means:
#      brain spinalcord 
# 6.7139479  0.9061404 

tapply(viral_load,Tissue,sd)
# Tissue SDs:
#     brain spinalcord 
# 10.430441   6.69770

