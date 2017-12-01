HIV <- read.table("572_part3.tdf", header = TRUE) #import data, specifying first line as header.
#no major issues with data on first sight. One neg number, indicating v.small viral load?! One high pos number viral load at 25
str(HIV) #check variables and observations...obs 40 and variables 5
attach(HIV)
par(mfrow=c(1,1)) #set graph space
hist(viral_load) #intial histogram of data, looks normally distributed
plot(HIV) #plots of all the data
#relationship between viral_load and distance?
#this is the only immediately obvious visual relationship 
cor.test(Distance, viral_load) #60% correlation and low p value?
cor.test(Shannon_diversity, viral_load) #no coreelation really...
par(mfrow=c(2,2)) #set graph space
#plot all variables with regard to the response variable to identify any interactions
boxplot(viral_load~CD4, xlab="CD4 count")
plot(viral_load~Shannon_diversity, xlab="Shannon diversity")
boxplot(viral_load~Tissue, xlab="Tissue")
plot(viral_load~Distance, xlab="Genetic Distance") #only noticeable relationship again.

simplemodel_man <- lm(viral_load~CD4+Shannon_diversity+Tissue+Distance) #simplest model to identify any single correlations to viral load
summary(modelnoint) #summary of data shows only 1 signifcance with distance
simplemodel_man.aov <- anova(simplemodel_man) 
simplemodel_man.aov #only significance, low p value with distance
plot(simplemodel_man) #heavy tailed on the qq plot? 
#fan shaped residual distribution vs fitted?

manualmodel <- lm(viral_load~CD4*Shannon_diversity*Tissue*Distance) #most complicated model
plot(manualmodel) #bad model plot, appears to be one outlier in res vs fitted plot. qq plot is slightly better. 
manualmodel.aov <- anova(manualmodel)
manualmodel.aov 
drop1(manualmodel, test = "F") #a lot of non-significant interactions here, only distance or CD4:Tissue seem to have an effect in the model.
HIV_new <- HIV[-14,] #remove the outlier
attach(HIV_new) #attach new dataset
manualmodel2 <- update(manualmodel, . ~ . - CD4:Shannon_diversity:Tissue:Distance) #create new model and update by removing the least significant interaction in drop 1 test.
manualmodel2.aov <- anova(manualmodel2)
manualmodel2.aov
plot(manualmodel2) #removing outlier improved res vs fitted plot, variance appears to decrease as data set increases?
#qq plot normal dist but not great. One major small outlier?
drop1(manualmodel2, test="F")
#the model is too complicated and most of the interactions don't appear to be significant
#CD4:Tissue was significant before the last model update, so will create a simplified model with this interaction
manualmodel3 <- lm(viral_load~Distance+(CD4*Tissue))
plot(manualmodel3) #stronger res vs fitted plot,qq plot still heavy tailed.
manualmodel3.aov <- anova(manualmodel3)
manualmodel3.aov
drop1(manualmodel3, test = "F") #no improvements to AIC values suggested here, will attempt to remove the CD4:Tissue interaction to see if there is any improvement
manualmodel4 <- update(manualmodel3, . ~ . - CD4*Tissue)
plot(manualmodel4)
manualmodel4.aov <- anova(manualmodel4)
drop1(manualmodel4, test = "F")
manualmodel4.aov #not much difference to the model after removing the interaction.

#Distance appears to be the most relevant relationship.
#carry out forward stepwise models with Distance as the starting variable, shuffle the others.
forward_1 <- step(lm(viral_load ~ Distance),scope=(~Distance*CD4*Tissue*Shannon_diversity),direction="forward")
plot(forward_1)
forward_2 <- step(lm(viral_load ~ Distance),scope=(~Distance*Tissue*Shannon_diversity*CD4),direction="forward")
plot(forward_2)
forward_3 <- step(lm(viral_load ~ Distance),scope=(~Distance*Shannon_diversity*CD4*Tissue),direction="forward")
plot(forward_3)
drop1(forward_3, test = "F")
#nothing improves AIC in these steps...viral load appears to be best explained by the distance. This is the lowest AIC value as of now

#Aattempt backwards stepwise models
backward_1 <- step(lm(viral_load~Distance*CD4*Tissue*Shannon_diversity), direction = "backward")
plot(backward_1)
backward_2 <- update(backward_1, . ~ . -CD4:Tissue:Shannon_diversity)
plot(backward_2)
summary(backward_2)
drop1(backward_2)
#AIC values are worse for all the backwards models. Do not include these in the final model. 

#attempt bidirectional model
bidirect<-step(lm(viral_load~Distance*CD4),scope=c(lower=~CD4,upper=~ Distance* CD4 * Shannon_diversity * Tissue),direction="both")
#bad AIC values, not likely to improve by any suggested updates to the model.


final_model <- lm(viral_load~Distance) #final model will be simple relationship with distance
drop1(final_model, test = "F") #this AIC value matches that of the best forward stepwise regression model. 
final_model.aov <- anova(final_model)
final_model.aov #significant Pvalue for the model. 
plot(final_model)
summary(final_model)
par(mfrow=c(1,1))
plot(viral_load~Distance)
abline(final_model) #plot the line of best fit for the regresison model. 