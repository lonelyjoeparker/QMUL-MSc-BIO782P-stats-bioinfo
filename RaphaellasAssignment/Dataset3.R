HIV <- read.table("273_part3.tdf", header = T)
attach(HIV)

#____________________________________________________________

#Let's start by trying to fit our own model
#We'll try fitting all the terms and then remove them one by one according to highest p-value
#(but only if their p-value is >0.05)

#We'll start with a model assuming all the terms interact

HIVmymodel <- lm(viral_load~Distance*CD4*Shannon_diversity*Tissue)
drop1(HIVmymodel, test = "F")
#a model with all terms interacting is non-significant so let's try and find the individual significant terms
#in code terms this means "*" -> "+"

HIVmymodel <- lm(viral_load~., data = HIV)
summary(HIVmymodel)
#R^2 is 0.4997
drop1(HIVmymodel, test = "F")
#Tissue has the largest p-value

HIVmymodel <- update(HIVmymodel, .~. -Tissue)
drop1(HIVmymodel, test = "F")
#Shannon_diversity has the largest p-value

HIVmymodel <- update(HIVmymodel, .~. -Shannon_diversity)
drop1(HIVmymodel, test = "F")
#CD4 still has a p-value over 0.05

HIVmymodel <- update(HIVmymodel, .~. -CD4)
drop1(HIVmymodel, test ="F")
#only have one term left but it's definetly significant
summary(HIVmymodel)
#R^2 is 0.4518


#__________________________________________________________________________________________________________________________________________

#Now let's try to use an automated model selection
#We'll use what we learned via our earlier experimentation to order the terms
#We'll order them according to when they were removed from our model
# "Distance - CD4 - Shannon_Diversity - Tissue"

#Model from steps backward
HIVautomodelback <- step(lm(viral_load~., data = HIV), direction = "backward")
drop1(HIVautomodelback, test = "F")
#Hmmm it leaves in CD4 but CD4 is non-significant in a drop1 test...

#Let's try see about from the other direction

#Model from steps forward
HIVautomodelforward <- lm(viral_load~Distance)
step(HIVautomodelforward, scope = (~Distance*CD4*Shannon_diversity*Tissue), direction = "forward")
drop1(HIVautomodelforward, test = "F")
#Gives the same model as what i found manually

#Let's try doing a step backward model from a model that assumes all the terms interact

HIVautomodelback <- step(lm(viral_load~Distance*CD4*Shannon_diversity*Tissue), direction = "backward")
drop1(HIVautomodelback, test="F")
#Hmmm it reports Pr(>F) values for Distance and CD4:Shannon Diversity but not CD4 and Shannon Diversity seperately.
#Additionaly CD4:Shannon_diversity is reported as non-significant
#Let's try removing the individual CD4, the individual Shannon_Diversity, and then both
#It may just not be significant because when the term is there twice it has to share the variance explains
#this can push significant effects into non-significant territory

#create the three models
HIVmodminusCD4 <- lm(viral_load~Distance+Shannon_diversity+CD4:Shannon_diversity)
HIVmodminusShannon <-lm(viral_load~Distance+CD4+CD4:Shannon_diversity)
HIVmodminusCDSh <- lm(viral_load~Distance+CD4:Shannon_diversity)

#check the three models
drop1(HIVmodminusCD4, test="F")
drop1(HIVmodminusShannon, test="F")
drop1(HIVmodminusCDSh, test="F")
#Distance and CD4:Shannon_diversity are significant in (D,CD4:Sh):
#ModminusCD4 (1.18e-05,0.0256) and ModminusCDSh (1.18e-05,0.0494)

#Now let's try a bidirectional search
HIVautobi <- step(lm(viral_load~Distance+Shannon_diversity+CD4:Shannon_diversity), scope = c(lower=~Distance,upper= ~Distance*CD4*Shannon_diversity*Tissue), direction = "both")
HIVautobi2 <- step(lm(viral_load~Distance+CD4:Shannon_diversity), scope = c(lower=~Distance,upper= ~Distance*CD4*Shannon_diversity*Tissue), direction = "both")
#they both give back the same model

#_____________________________________________________________________________________________________________________________________________________


#ok so now we have 3 potential models that show significance under the drop 1 test
HIVmodminusCD4
HIVmodminusCDSh
HIVmymodel

anova(HIVmymodel,HIVmodminusCDSh)
#Anova can't work on HIVmodminus CD4 because Shannon_Diversity is repeated twice that violates ANOVAs requirements
#However, it's still valuable
#HIVmodminusCDSh is significantly better than HIVmymodel

#Now it's down to HIVmodminusCDSh vs. HIVmodminusCD4
HIVmodminusCD4
HIVmodminusCDSh
#wait they're actually the same?
summary(HIVmodminusCD4)
summary(HIVmodminusCDSh)
#no not the same...
anova(HIVmodminusCD4)
anova(HIVmodminusCDSh)
#HIVmodminusCDSh is better (Shanon_diversity is not significant)

#check plot 
par(mfrow =c(2,2))
plot(HIVmodminusCDSh)
summary(HIVmodminusCDSh)


#Plot Diversity vs. Viral Load
par(mfrow =c(1,1))
plot(viral_load~Distance, xlab= "Mean Pairwise Genetic Distance", ylab = "Viral Load")
vldmodel <- lm(viral_load~Distance)
anova(vldmodel) #just check significance
abline(-3.375,1.371, col ="red", lty =2 )


# Plot 95% Confidence levels for Coefficients of CD4:Shannon Diversity
# These matrix values can be gotten from the summary(HIVModminusCDSh)
HIVmat.matrix <- matrix(data = c(0.640856, -0.002732), dimnames = list(c("CD4 High","CD4 Low"),c("Coefficient")))
HIVsd.matrix <- matrix(data = c(0.311602 , 0.299914), dimnames = list(c("CD4 High","CD4 Low"),c("Coefficient")))

par(mfrow=c(1,1))
locationsrev <- barplot(HIVmat.matrix, beside = T)
barplot(HIVmat.matrix, beside = T, col = c("goldenrod", "darkgoldenrod4"), ylim = c(-0.5,1), xaxt ="n", ylab = "Coefficient", xlab = "CD4 Count")
axis(side=1, at =c(1.5,2.5), labels = c("High","Low"),font = 2)
arrows(locationsrev, HIVmat.matrix, locationsrev, HIVmat.matrix + HIVsd.matrix, angle =90, length =0.1)
arrows(locationsrev, HIVmat.matrix, locationsrev, HIVmat.matrix - HIVsd.matrix, angle =90, length =0.1)

