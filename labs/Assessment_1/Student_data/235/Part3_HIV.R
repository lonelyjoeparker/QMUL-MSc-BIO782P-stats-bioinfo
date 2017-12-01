#Part 3: HIV viral load and within-patient population dynamics

#set the working directory to source file location under Session
#open the file and include headers:
HIV <- read.table("235_part3.tdf",header = TRUE)
#check that the data is in the right format:
str(HIV)
#change variables to factors and numbers if appropriate
#attach the variables to the data = do not have to constantly specify which data (HIV$): 
attach(HIV)
#look at all variables
par(mfrow=c(1,1))
plot(HIV,pch=21,col="black",bg="skyblue") 

#look at each variable against viral_load seperatly and perform and ANOVA:
par(mfrow=c(2,2))
plot(viral_load~CD4,xlab="CD4 Cell Count",ylab="Viral Load", main="A",col=c("skyblue1","skyblue4")) 
anova(lm(viral_load~CD4)) # pvalue (0.2242) above 0.05 = non-significant

plot(viral_load~Shannon_diversity,xlab="Shannon Diversity",ylab="Viral Load",main="B", pch=21,col="black",bg="skyblue1") 
abline(lm(viral_load~Shannon_diversity),col="red")
anova(lm(viral_load~Shannon_diversity)) # pvalue (0.003091**) below 0.05 = significant

plot(viral_load~Tissue,xlab= "Tissue Sample",ylab="Viral Load", main="C",col=c("skyblue","skyblue4")) 
anova(lm(viral_load~Tissue)) # pvalue (0.2631) above 0.05 = non-significant

plot(viral_load~Distance, xlab="Genetic Distance",ylab="Viral Load",main="D",pch=21,col="black",bg="skyblue1") 
abline(lm(viral_load~Distance),col="red")
anova(lm(viral_load~Distance)) # pvalue (0.001327**) below 0.05 = significant


#From these an potentially ideal model can be created:
HIV_ideal_model<-lm(viral_load~Distance+Shannon_diversity) 
plot(HIV_ideal_model) #check assumptions
anova(HIV_ideal_model) # pvalue (0.0005754*** and 0.0079073**) below 0.05 = significant

HIV_ideal_interaction_model<-lm(viral_load~Distance*Shannon_diversity)
plot(HIV_ideal_interaction_model) #check assumptions
anova(HIV_ideal_interaction_model) #with interaction: pvalue (0.8041628) above 0.05 = non-significant

#check for the best model for all these variables automatically and if they have interactions:
#first check with the backwards stepwise AIC:
HIV_backwards_model<-step(lm(viral_load~CD4*Shannon_diversity*Tissue*Distance),direction = "backward")
#check the forwards:
HIV_forwards_model<-step(lm(viral_load~Shannon_diversity),scope=(~Shannon_diversity*Tissue*Distance*CD4),direction ="forward")
#check using both together:
HIV_bidirectional_model<-step(lm(viral_load~Shannon_diversity),scope=c(lower=~Shannon_diversity,upper=~ Shannon_diversity*Tissue*Distance*CD4),direction="both")

anova(HIV_backwards_model) # pvalues = Shannon_diversity=0.002481**,Distance=0.003728** and CD4:Shannon_diversity:Tissue=0.027772*
plot(HIV_backwards_model) #check assumptions = curve in Residual vs Fitted
anova(HIV_forwards_model) #Shannon_diversity=0.001210** and Distance=0.003427**
plot(HIV_forwards_model) #check assumptions
anova(HIV_bidirectional_model) #same as previous
plot(HIV_bidirectional_model)#check assumptions
#low AIC = good - choose model with lowest AIC = forwards/bidirectional

#compare the suggested models:
anova(HIV_backwards_model,HIV_forwards_model,HIV_bidirectional_model, test="LRT") #using likelihood ratio test
anova(HIV_backwards_model,HIV_forwards_model,HIV_bidirectional_model, test="F") #using F test

#The final model is therefore:
Final_HIV_model <- lm(viral_load~Distance+Shannon_diversity)