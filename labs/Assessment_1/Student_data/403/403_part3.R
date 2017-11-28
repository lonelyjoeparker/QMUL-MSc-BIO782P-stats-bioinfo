#Open the Table:

hiv=read.table(file="403_part3.tdf",header = TRUE)

#Check the variables:
#Initially, it was add a variable "Week" with the information of the week when the sample was collected
#but in further analysis no effect on viral load was seen and the variable was discarded.
str(hiv)
attach(hiv)

#Plot of all the variables:
plot(hiv)

#Histogram of the Response variable:
hist(viral_load, main="Histogram of Viral_load")
mean(viral_load)
sd(viral_load)

ks.test(viral_load,"pnorm",mean(viral_load),sd(viral_load))
#Using the Kolmogorov-Smirnov test it was accepted the Ho: Expr_units follows a normal distribution
#With this test and the histogram it was accepted that diversity follows a normal distribution.

#Plot the Response variable vs each of the Explanatory variables:
par(mfrow=c(2,3))
plot(viral_load~CD4, main="Viral Load vs CD4")
plot(viral_load~Shannon_diversity, main="Viral Load vs Shannon diversity")
plot(viral_load~Tissue, main="Viral Load vs Tissue")
plot(viral_load~Distance, main="Viral Load vs Distance")

#Automated model search:
#Since thera are a large number of variables and automated  stepwise search in both ways was
#made. Because of the plots, Tissue was selected as the lower model, meanwhile all the variables and their 
#interaction were selected as the upper model:
model_Search=step(lm(viral_load~CD4+Tissue+Distance+Shannon_diversity),scope=c(lower=~Tissue,upper=~Tissue*Distance*Shannon_diversity*CD4),direction="both")

#With the stepwise result a model was created:
model1=lm(viral_load~CD4+Tissue+Distance+CD4:Tissue)
par(mfrow=c(2,2))
plot(model1)
anova(model1)
summary(model1)

#Because the order matters and the marginality of the model is important, a second model was proposed:
model2=lm(viral_load~Distance+CD4*Tissue)
par(mfrow=c(2,2))
plot(model2)
anova(model2)
summary(model2)
drop1(model2)

#Just to be sure not to lose an important variable:
model3=lm(viral_load~Distance+Shannon_diversity+CD4*Tissue)
par(mfrow=c(2,2))
plot(model3)
anova(model3)
summary(model3)
drop1(model3)

#See what happens when a possible significant variable is removed from the model:
model4=lm(viral_load~CD4*Tissue)
par(mfrow=c(2,2))
plot(model4)
anova(model4)
summary(model4)
drop1(model4)

#Comparison between the selected model and the simplest one: 
anova(model2,model4)

#Plots of model2:
par(mfrow=c(1,1))

#Interaction Viral_load with CD4*Tissue:
library(ggplot2)
ggplot(data=hiv, aes(x=factor(Tissue,labels = c("Brain","Spinalcord")), y=viral_load, fill=CD4)) + geom_boxplot() + ggtitle("Boxplot of HIV Viral Load by Tissue") + theme(plot.title = element_text(hjust=0.5))+ scale_x_discrete(name = "Tissue")+ scale_y_continuous(name="Viral Load")

#Interaction Viral_load with Distance:
plot(Distance,viral_load, main="HIV Viral Load vs Genetic Distance", xlab = "Genetic Distance", ylab="viral_load", col= "red")
abline(lm(viral_load~Distance),col="darkred")
summary(lm(viral_load~Distance))

#The 4 variables in a same graph:
plot(Distance, viral_load, main="Representative graph of Model 2", xlab = "Genetic Distance", ylab="Viral Load", type="n")
points(Distance[Tissue=="brain" & CD4=="lo"], viral_load[Tissue=="brain" & CD4=="lo"], col="red")
points(Distance[Tissue=="spinalcord" & CD4=="lo"], viral_load[Tissue=="spinalcord" & CD4=="lo"], col="orange")
points(Distance[Tissue=="brain"& CD4=="hi"], viral_load[Tissue=="brain" & CD4=="hi"], col="blue")
points(Distance[Tissue=="spinalcord"& CD4=="hi"], viral_load[Tissue=="spinalcord"& CD4=="hi"], col="violet")
abline(lm(viral_load[Tissue=="brain" & CD4=="lo"]~Distance[Tissue=="brain" & CD4=="lo"]), col="red")
abline(lm(viral_load[Tissue=="spinalcord" & CD4=="lo"]~Distance[Tissue=="spinalcord" & CD4=="lo"]), col="orange")
abline(lm(viral_load[Tissue=="brain" & CD4=="hi"]~Distance[Tissue=="brain"& CD4=="hi"]), col="blue")
abline(lm(viral_load[Tissue=="spinalcord"& CD4=="hi"]~Distance[Tissue=="spinalcord"& CD4=="hi"]), col="violet")
legend("bottomright", legend=c("Brain:Low", "Spinalcord:Low", "brain:High","Spinalcord:High"),pch=1, col=c("red","orange","blue","violet"))
