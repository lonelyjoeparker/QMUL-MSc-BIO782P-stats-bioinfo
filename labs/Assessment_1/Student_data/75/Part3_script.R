#Importing the data into a table
hiv_data <- read.table("75_part3.tdf", header=TRUE)
# Check the data
str(hiv_data)
attach(hiv_data)
plot(hiv_data)


# plotting the data that is in chronological order
# No pattern visible, constant fluctuation
plot(viral_load,main = "Viral Load over time" ,xlab= "Weeks") 

# A linear model is created and placed into a variable
model1_fit <- lm(viral_load~Shannon_diversity+Distance+CD4+Tissue) #can also do model1_fit <- lm(viral_load ~ . ,data=hiv_data)
drop1(model1_fit, test = "F") 
#we can see that dropping Distance has a significant effect on AIC, thus we need to keep distance
summary(model1_fit)

#BACKWARD ELIMINATION
backward_step1<-step(model1_fit, direction="backward") # Virald load ~ Distance had the lowest AIC, thus distance was the best explanatory variable

#FORWARD STEPWISE
model2<- lm(viral_load~1)
forward_step<-step((model2), scope =(~ Shannon_diversity*Distance*CD4*Tissue), direction="forward")

#Both Direction stepwise
bidirec_step=step(lm(viral_load~1),scope=c(lower=~1,upper=~ CD4 + Tissue + Shannon_diversity + Distance),direction="both")
anova(bidirec_step)



#Split data based on the Tissue type, 2 data frames created (tables), 1 with data assocaited with spine, the other for brain

hiv_brain <- data.frame(viral_load=hiv_data$viral_load[Tissue=="brain"],CD4=hiv_data$CD4[Tissue=="brain"],Shannon_diversity=hiv_data$Shannon_diversity[Tissue=="brain"],Tissue=hiv_data$Tissue[Tissue=="brain"],Distance=hiv_data$Distance[Tissue=="brain"])
brain_step <- step(lm(hiv_brain$viral_load~hiv_brain$Shannon_diversity+hiv_brain$Distance+hiv_brain$CD4),direction="backward")

hiv_spine <- data.frame(viral_load=hiv_data$viral_load[Tissue=="spinalcord"],CD4=hiv_data$CD4[Tissue=="spinalcord"],Shannon_diversity=hiv_data$Shannon_diversity[Tissue=="spinalcord"],Tissue=hiv_data$Tissue[Tissue=="spinalcord"],Distance=hiv_data$Distance[Tissue=="spinalcord"])
spine_step <- step(lm(hiv_spine$viral_load~hiv_spine$Shannon_diversity+hiv_spine$Distance+hiv_spine$CD4),direction="backward")

# Distance had the best fit (lower AIC) for viral_load in the brain and spine
# Distance AIC for viral_load was lower for brain than spine.




#split data based on CD4+ count "lo", or "hi". 2 data frames created.
hiv_lo <- data.frame(viral_load=hiv_data$viral_load[CD4=="lo"],CD4=hiv_data$CD4[CD4=="lo"],Shannon_diversity=hiv_data$Shannon_diversity[CD4=="lo"],Tissue=hiv_data$Tissue[CD4=="lo"],Distance=hiv_data$Distance[CD4=="lo"])
lo_step <-  step(lm(hiv_lo$viral_load~hiv_lo$Shannon_diversity+hiv_lo$Distance+hiv_lo$Tissue),direction="backward") 

hiv_hi <- data.frame(viral_load=hiv_data$viral_load[CD4=="hi"],CD4=hiv_data$CD4[CD4=="hi"],Shannon_diversity=hiv_data$Shannon_diversity[CD4=="hi"],Tissue=hiv_data$Tissue[CD4=="hi"],Distance=hiv_data$Distance[CD4=="hi"])
hi_step <-  step(lm(hiv_hi$viral_load~hiv_hi$Shannon_diversity+hiv_hi$Distance+hiv_hi$Tissue),direction="backward") 

# Distance had the best fit (lower AIC) for viral_load in both CD4 levels.
# Distance AIC for viral_load was lower when CD4+ count was high.


# Plot the data found from the stepwise regression
par(mfrow=c(1,1))
plot(Distance,viral_load, main = "Viral Load over mean pairwise genetic distance", ylab="Viral load log10(particles per ml)", xlab ="Genetic Distance")
abline(coef(lm(viral_load~Distance)))
coef(lm(viral_load ~ Distance))
legend(14,0,"R^2: 0.504")


# Checking model assumptions
model1 <- lm(viral_load ~ Distance)
summary(model1)$r.squared # co-efficient of determination
par(mfrow=c(2,2))
plot(model1)

