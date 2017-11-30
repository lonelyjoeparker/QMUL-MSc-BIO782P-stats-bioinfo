#Name: Dionysios Grigoriadis
#ID: 170484367
#BIO782P Statistics and Bioinformatics Assignment 2017
#CWK 1

#Dataset3

#Reading of the data
hiv_data = read.table("137_part3.tdf", header = TRUE)
attach(hiv_data)

#Exploring possible relationships between the viral load values and the
#other variables.

test_cd4 = t.test(viral_load[CD4=="hi"], viral_load[CD4=="lo"]) #CD4+ count does not
#affect viral load
test_cd4

test_shannon = cor.test(viral_load, Shannon_diversity) #No significant correlation 
#between shannon_Diversity and viral load
test_shannon

test_tissue = t.test(viral_load[Tissue=="brain"], viral_load[Tissue=="spinalcord"])
#Tissue does not affect viral load
test_tissue

test_distance = cor.test(viral_load, Distance) #There is a positive corelation between
#distance and viral load
test_distance

#Plot of the relationships examined above
png(filename="viral_load_vs.everything_plot.png")
par(mfrow=c(2,2))
par(mar=c(5, 6, 4, 2) + 0.1)
plot(viral_load ~ Shannon_diversity+Tissue, ylab = "Viral Load\nlog10(viral particles/ml)")
plot(viral_load ~ Distance, ylab = "Viral Load\nlog10(viral particles/ml)", xlab = "Mean pairwise genetic distance\nfrom individual viruses")
plot(viral_load ~ CD4, ylab = "Viral Load\nlog10(viral particles/ml)", xlab = "CD4+ cell counts in general circulation")
dev.off()
par(mfrow = c(1,1))
#It is possible that viral load is only explained by the distance eplanatory variable.
#Stepwise regression to confirm this hypothesis:

#Forward
f_model1=step(lm(viral_load ~ 1),scope=(~Tissue*Distance*Shannon_diversity*CD4),direction="forward")
f_model2=step(glm(viral_load ~ 1),scope=(~Tissue*Distance*Shannon_diversity*CD4),direction="forward")

#Backward
b_model3=step(lm(viral_load ~ Tissue*Shannon_diversity*Distance), direction="backward")
b_model4=step(glm(viral_load ~ Tissue*Shannon_diversity*Distance), direction="backward")

#Both sides
both_model5=step(lm(viral_load ~ Tissue+Shannon_diversity+Distance), scope = c(lower = ~ 1, higer = ~Tissue*Distance*Shannon_diversity), direction = "both")
both_model6=step(glm(viral_load ~ Tissue+Shannon_diversity+Distance), scope = c(lower = ~ 1, higer = ~Tissue*Distance*Shannon_diversity), direction = "both")

#The best proposed model based on AIC is:
png(filename="viral_load_vs.distance_Regression_Assumptions.png")
par(mfrow=c(2,2))
best_model=lm(viral_load~Distance)
plot(best_model) #Assumptions seem valid
summary(best_model)
dev.off()

#Viral load vs. Distance plot with the regression line
png(filename="viral_load_vs.distance_Regression.png")
par(mfrow = c(1,1))
plot(viral_load ~ Distance, ylab = "Viral Load\nlog10(viral particles/ml)", xlab = "Mean pairwise genetic distance\nfrom individual viruses to a reference sequence")
abline(coef(best_model), col = "red")
dev.off()

