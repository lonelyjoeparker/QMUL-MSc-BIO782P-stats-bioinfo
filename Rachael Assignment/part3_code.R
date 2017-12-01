#read file and check variables
HIV <- read.table("322_part3.tdf", header = T)
str(HIV)

attach(HIV)

#exploratory analysis
par(mfrow = c(1,1))
plot(viral_load)
plot(Shannon_diversity)
plot (Distance)

par(mfrow = c(1, 2))
plot(viral_load, Shannon_diversity, pch = as.numeric(Tissue))
legend("bottomleft", pch = 1:2, c("brain","spinalcord"), cex = 0.7, pt.cex = 0.7)
# Comments: no obvious correlation. might be a difference Tissue types
plot(Distance, viral_load, pch = as.numeric(Tissue), col=c("midnightblue", "indianred4"), ylab="Viral Load, log10(particles/ml)")
legend("bottomright", pch = 1:2, c("brain","spinalcord"), col=c("midnightblue", "indianred4"),cex = 0.7, pt.cex = 0.7)
# Comments: looks to have correlation, not significant with tissue
plot(viral_load~CD4)
tapply(viral_load, CD4, var)
fligner.test(viral_load~CD4)
t.test(viral_load~CD4)
#doesnt look important, slight difference in variance, tested and non-significant
plot(viral_load~Tissue)
tapply(viral_load, Tissue, var)
fligner.test(viral_load~Tissue)
t.test(viral_load~CD4)
#possibly important, difference in variance seen, tested and non-significant (just!)

# Analysis of comments
# Look at difference of diversity by tissue
par(mfrow = c(1, 1))
boxplot(Shannon_diversity~Tissue)
t.test(Shannon_diversity~Tissue)
# p-value > 0.05 - not significant

# Look at correlation between viral load and distance/diversity
cor.test(viral_load, Distance)
# p value < 0.05 so significant
cor.test(viral_load, Shannon_diversity)
#not significant

# effect of immune system penetration on HIV population, looks to have effect
boxplot(viral_load~CD4)
t.test(viral_load~CD4)
# p-value > 0.05 so not significant

#effect of tissue on viral load
boxplot(viral_load~Tissue)
t.test(viral_load~Tissue)
# p-value > 0.05 so not significant

par(mfrow = c(1, 1))
plot(viral_load ~ interaction(CD4, Tissue))
interaction.plot(Tissue, CD4, viral_load, ylab = "Mean Viral Load", legend = T)
vltcd <- lm(viral_load~CD4*Tissue)
anova(vltcd)
summary(vltcd)
#interaction, not significant

#means
mean(viral_load[CD4=="hi" & Tissue=="brain"])
sd(viral_load[CD4=="hi" & Tissue=="brain"])
mean(viral_load[CD4=="hi" & Tissue=="spinalcord"])
sd(viral_load[CD4=="hi" & Tissue=="spinalcord"])
mean(viral_load[CD4=="lo" & Tissue=="brain"])
sd(viral_load[CD4=="lo" & Tissue=="brain"])
mean(viral_load[CD4=="lo" & Tissue=="spinalcord"])
sd(viral_load[CD4=="lo" & Tissue=="spinalcord"])



#plot
HIV.mat <- tapply(viral_load, list(CD4, Tissue), mean)
HIV.sd <- tapply(viral_load, list(CD4, Tissue), sd)
HIV.se <- HIV.sd/sqrt(10)
crit.value <- qt(0.025, 4)
HIV.95s <- HIV.se*crit.value
locations <- barplot(HIV.mat, beside=T)
barplot(HIV.mat, beside=T, col=c("midnightblue", "indianred4"), legend.text = c("High", "Low"),
        args.legend = list(x="top", bty = "n", title = "CD4 count"), ylim=c(0,25), ylab="Viral Load",
        xlab = "Tissue Type", font.lab=2, font.axis=2, xaxt="n")
axis(side = 1, at = c(2,5), labels = c("Brain", "Spinal Cord"), font=2)
arrows(locations, HIV.mat, locations, HIV.mat + HIV.95s, angle = 90, length = 0.1)
arrows(locations, HIV.mat, locations, HIV.mat - HIV.95s, angle = 90, length = 0.1)


#term significance:
test <- lm(viral_load~Distance+Shannon_diversity+CD4+Tissue)
drop1(test, test = "F")
anova(test)

#maximum model with term in order of significance
maxmod <- lm(viral_load~Distance*Shannon_diversity*Tissue*CD4)
#minimum model
minmod <- lm(viral_load~1)

#find minamal adequate model - remove least significant terms of highest order first
anova(maxmod)
summary(maxmod)
drop1(maxmod, test="F")
#remove Distance:Tissue:CD4

#look at plots and see if transformation needed
par(mfrow = c(2,2))
plot(maxmod)
maxmod.log <- lm(log(viral_load+4)~Distance*Shannon_diversity*Tissue*CD4)
plot(maxmod.log)
#transformation doesn't help stick with normal data

mod1 <- update(maxmod, .~. -Distance:Tissue:CD4)
anova(maxmod, mod1)
#justified simplification
anova(mod1)
drop1(mod1, test = "F")
#term to remove: Distance:Shannon_diversity:Tissue:CD4

mod2 <- update(mod1, .~. -Distance:Shannon_diversity:Tissue:CD4)
anova(mod1, mod2)
#justified simplification
anova(mod2)
drop1(mod2, test = "F")
#term to remove: Shannon_diversity:Tissue:CD4

mod3 <- update(mod2, .~. -Shannon_diversity:Tissue:CD4)
anova(mod2, mod3)
#justified simplification
anova(mod3)
drop1(mod3, test="F")
#term to remove Distance:Shannon_diversity:CD4

mod4 <- update(mod3, .~. -Distance:Shannon_diversity:CD4)
anova(mod3, mod4)
#justified simplification
anova(mod4)
drop1(mod4, test="F")
#term to remove: Distance:CD4

mod5 <- update(mod4, .~. -Distance:CD4)
anova(mod4, mod5)
#justified simlpification
anova(mod5)
drop1(mod5, test="F")
#term to remove: Shannon_diversity:CD4

mod6 <- update(mod5, .~. -Shannon_diversity:CD4)
anova(mod5, mod6)
#justified
anova(mod6)
drop1(mod6, test="F")
#factor to remove: Tissue:CD4

mod7 <- update(mod6, .~. -Tissue:CD4)
anova(mod6, mod7)
#justified
anova(mod7)
drop1(mod7, test="F")
#term to remove: CD4

mod8 <- update(mod7, .~. -CD4)
anova(mod7, mod8)
#justified
anova(mod8)
drop1(mod8, test = "F")
#term to remove: Distance:Shannon_diversity

mod9 <- update(mod8, .~. -Distance:Shannon_diversity)
anova(mod8,mod9)
anova(mod9)
drop1(mod9, test = "F")
#term to remove: Shannon_diversity:Tissue

mod10 <- update(mod9, .~. -Shannon_diversity:Tissue)
anova(mod9, mod10)
#return to previous model

#check diagnostic plots
par(mfrow = c(2,2))
plot(mod9)
mod9.log <- lm(log(viral_load+4) ~ Distance + Shannon_diversity + Tissue + Distance:Tissue + 
                 Shannon_diversity:Tissue + Distance:Shannon_diversity:Tissue)
plot(mod9.log)

#continue with logged data
anova(mod9.log)
drop1(mod9.log, test="F")
#term to remove: Distance:Shannon_diversity:Tissue

mod10.log <- update(mod9.log, .~. -Distance:Shannon_diversity:Tissue)
anova(mod9.log, mod10.log)
#justified
anova(mod10.log)
drop1(mod10.log, test="F")
#term to remove: Shannon_diversity:Tissue

mod11.log <- update(mod10.log, .~. -Shannon_diversity:Tissue)
anova(mod10.log, mod11.log)
#justified
anova(mod11.log)
drop1(mod11.log, test="F")
#term to remove: Distance:Tissue

mod12.log <- update(mod11.log, .~. -Distance:Tissue)
anova(mod11.log, mod12.log)
#justified
anova(mod12.log)
drop1(mod12.log, test="F")
#term to remove: Tissue

mod13.log <- update(mod12.log, .~. -Tissue)
anova(mod12.log, mod13.log)
#justified
anova(mod13.log)
drop1(mod13.log, test="F")
#term to remove: Shannon_diversity

mod14.log <- lm(log(viral_load+4)~Distance)
anova(mod13.log, mod14.log)
#justified, check diagnostic plots
par(mfrow = c(2,2))
plot(mod14.log)
anova(mod14.log)
#data transformation

mod15 <- lm(viral_load~Distance)
par(mfrow = c(2,2))
plot(mod15)
#difficult call... use mod14.log

#Summary:
#overall: lm(log(viral_load+4)~Distance)

#compare to log of max and min model
minlog <- lm(log(viral_load+4)~1)
maxlog <- lm(log(viral_load+4)~Distance*Shannon_diversity*Tissue*CD4)
anova(mod14.log, minlog)
anova(mod14.log, maxlog)
#Comments: not significant

#plot
par(mfrow = c(1,1))
plot(Distance, log(viral_load+4), col = "red", ylab = "Log(Viral Load + 4)", xlab="Genetic Distance")
#add line
abline(mod14.log, col="darkmagenta")

#automated model: fitting model step wise
#forward model stepwise selection
forward_final=step(lm(viral_load ~ Distance), scope=(~Distance*Shannon_diversity*Tissue*CD4), direction = "forward")
summary(forward_final)
plot(forward_final)
# final model as distance + diversity
backwards_final=step(lm(viral_load ~ Distance*Shannon_diversity*Tissue*CD4),direction="backward")
summary(backwards_final)
plot(backwards_final)
#final model as Distance * Shannon_diversity * Tissue * CD4
both_final=step(lm(viral_load ~ Distance*Shannon_diversity), scope=c(lower=~Distance, upper=~Distance*Shannon_diversity*Tissue*CD4), direction="both")
summary(both_final)
plot(both_final)
# final model as Distance + Shannon_diversity

#best automatic, 
anova(backwards_final, forward_final)
#not significant so go for more simple model

#can't compare manual and automatic because not comparable

