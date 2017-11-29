# read file and check data variables
microDiversity <- read.table("322_part1.tdf", header = T)
str(microDiversity)

#attach for easy access
attach(microDiversity)

#start model
diversity.lm <- lm(diversity ~ lattitude*season)
summary(diversity.lm)
anova(diversity.lm)
par(mfrow=c(2,2))
plot(diversity.lm)
#requires transformation

#transformed data model
diversity.lm2 <- lm(log(diversity+3) ~ lattitude*season)
summary(diversity.lm2)
anova(diversity.lm2)
par(mfrow=c(2,2))
plot(diversity.lm2)
# comments: intercept (aug/equatorial) is p-value significantly different from 0. mean for jan season significantly different from intercept. no significant interaction term

#check assumptions
#1 continuous data - yes
#2 random samples - yes
#3 normally dist - logged data to account or this as seen in QQ plot of model
#4 equal varaince
tapply(diversity, lattitude, var)
tapply(diversity, season, var)
#season shows difference in variance. ANOVA robust test - equal sample sizes so go ahead.

#anova
anova(diversity.lm)
# comments: significance only seen with season. No significance sen in the interactive term

#drop test, drop interaction term
drop1(diversity.lm, test="F")
# Comments: AIC values within 2 vlaues of each other and therefore don't show significance

#interaction plot
interaction.plot(season, lattitude, diversity)
# lines don't cross so no interaction

#data
mean(diversity[lattitude == "equatorial"])
sd(diversity[lattitude == "equatorial"])
mean(diversity[lattitude == "temperate"])
sd(diversity[lattitude == "temperate"])

mean(diversity[season == "Jan"])
sd(diversity[season == "Jan"])
mean(diversity[season == "Aug"])
sd(diversity[season == "Aug"])

#plotting data
par(mfrow = c(1,1))
#matix of means
diversity.mat <- tapply(log(diversity+3), list(season, lattitude), mean)
diversity.mat
#matrix of sd
diversity.sd <- tapply(log(diversity+3), list(season, lattitude), sd)
diversity.sd

#standard errors
diversity.se <- diversity.sd/sqrt(5)
diversity.se

#critical value (0.05, 0.025 because 2 tiered) at 4df (5-1), 
crit.val <- qt (0.025, 4)
#confidence intervals
diversity.95s <- diversity.se * crit.val
diversity.95s

locations <- barplot(diversity.mat, beside = T)

barplot(diversity.mat, beside=T, col = c("seagreen2", "seagreen4"), legend.text = c("Aug","Jan"), args.legend = list(x="topright", bty = "n", title="Season"), ylim = c(0,2.5), ylab="Log(Relative Diversity)", xlab = "Latitude", font.lab = 2, font.axis = 2, xaxt = "n")
axis(side = 1, at = c(2,5), labels = c("Equatorial","Temperate"), font = 2)

#addition of error bars
arrows(locations, diversity.mat, locations, diversity.mat+diversity.95s, angle = 90, len = 0.1)
arrows(locations, diversity.mat, locations, diversity.mat-diversity.95s, angle = 90, len = 0.1)
