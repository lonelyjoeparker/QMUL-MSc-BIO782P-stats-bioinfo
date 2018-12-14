###### Catherine Okuboyejo, 180279704
# data set 1608
# NOTE: script assumes all files in the current working directory
# I submitted a pull request on github

## Dataset 3####
hiv_data <- read.table("../Assessment_1/part_3_student_1608.tdf", header = TRUE)
str(hiv_data)

# since brain and spinalcord samples not randomised, add week column to account for possible effect of time
hiv_data$week <- 1:40

# explore variables
plot(hiv_data) # looks like there is an effect of time
par(mfrow=c(2,2))
plot(VLoad ~ CD4, data = hiv_data, main = "VLoad ~ CD4") # look similar, may not be such a good explanatory variable
plot(VLoad ~ tissue, data = hiv_data, main = "VLoad ~ tissue") # spinalCord looks to have higher viral load than brain
plot(VLoad ~ score_shannon, data = hiv_data, main = "VLoad ~ score_shannon") # maybe a tiny little positive correlation
plot(VLoad ~ score_distance, data = hiv_data, main = "VLoad ~ score_distance") # looks like a small positive correlation

# explore effect of time on CD4, shannon, vload
plot(VLoad ~ week, data = hiv_data, main = "VLoad ~ week") # looks like there is a positive correlation
plot(week ~ CD4, data = hiv_data, main = "week ~ CD4") # more low cd4 counts in later weeks
plot(score_shannon ~ week, data = hiv_data, main = "score_shannon ~ week") # looks like there is no relationship

## overall tissue and score_distance look to have the most exploratory power however
# there seems to be an influence of time 
# since the data were so hard to collect we will try to make the most of all of it

# fit maximal model - try to get best model using p-values
max_model <- lm(VLoad ~ CD4 * tissue * score_shannon * score_distance * week, data = hiv_data) 
anova(max_model) # no significant interactions especially with week

# refit model with no interaction terms as none were significant
hiv_model1 <- lm(VLoad ~ CD4 + tissue + score_shannon + score_distance + week, data = hiv_data) 
anova(hiv_model1)

# reorder vars with most significant first
hiv_model2 <- lm(VLoad ~ tissue + score_distance + score_shannon + CD4 + week, data = hiv_data) 
anova(hiv_model2)
plot(hiv_model2)

# remove week
hiv_model3 <- lm(VLoad ~ tissue + score_distance + score_shannon + CD4, data = hiv_data)
anova(hiv_model3)

# remove CD4
hiv_model4 <- lm(VLoad ~ tissue + score_distance + score_shannon, data = hiv_data)
anova(hiv_model4)

# remove shannon score 
hiv_model5 <- lm(VLoad ~ tissue + score_distance, data = hiv_data)
anova(hiv_model5) # the best one yet

# check if model can get any better (spoilers, it can't)
hiv_model6 <- lm(VLoad ~ tissue , data = hiv_data)
anova(hiv_model6)

hiv_model6 <- lm(VLoad ~ score_distance, data = hiv_data)
anova(hiv_model6)

# final model
hiv_modelfinal <- hiv_model5

#check assumptions
par(mfrow=c(2,2))
plot(hiv_modelfinal) #looks fine

summary(hiv_modelfinal)

# create plot
par(mfrow=c(1,1))
plot(VLoad ~ score_distance, data = hiv_data, 
     pch = 16, 
     xlab = "Mean pairwise genetic distance", ylab = "Viral load", cex.lab=1.2)
abline(-1.3560, 1.8495, col = "darkcyan", lwd = 2, lty = 2)
abline(-1.3560+1.9894, 1.8495, col = "indianred2", lwd = 2)
legend(0.1, 7.5, legend = c("Brain", "Spinal cord"), 
       col = c("indianred2", "darkcyan"), lwd=2, lty = 1:2, cex = 1.2)


