HIVdata <- read.table("909_part3.tdf", header = T)

#manual attempts at modelling.
model1 <- lm(HIVdata$viral_load ~ HIVdata$Shannon_diversity * HIVdata$CD4 * HIVdata$Tissue * HIVdata$Distance)
model2 <- lm(HIVdata$viral_load ~ HIVdata$Shannon_diversity * HIVdata$CD4 * HIVdata$Tissue)
model3 <- lm(HIVdata$viral_load ~ HIVdata$Shannon_diversity * HIVdata$CD4 * HIVdata$Distance)
model4 <- lm(HIVdata$viral_load ~ HIVdata$Shannon_diversity * HIVdata$Tissue * HIVdata$Distance)
model5 <- lm(HIVdata$viral_load ~ HIVdata$CD4 * HIVdata$Tissue * HIVdata$Distance)
model6 <- lm(HIVdata$viral_load ~ HIVdata$Shannon_diversity * HIVdata$CD4)
model7 <- lm(HIVdata$viral_load ~ HIVdata$Shannon_diversity * HIVdata$Distance)
model8 <- lm(HIVdata$viral_load ~ HIVdata$Tissue * HIVdata$Distance)

anova(model1, model2, model3, model4, model5, model6, model7, model8) #test which model is best, pretty useless data.

#automated modelling (backwards and forwards)
backwards_final <- step(lm(HIVdata$viral_load ~ HIVdata$Shannon_diversity * HIVdata$CD4 * HIVdata$Tissue * HIVdata$Distance), direction = "backward")
forwards_final <- step(lm(HIVdata$viral_load ~ HIVdata$Shannon_diversity), scope = (~HIVdata$Shannon_diversity*HIVdata$CD4*HIVdata$Tissue*HIVdata$Distance), direction = "forward")

anova(forwards_final, backwards_final) #see if there is a difference between the two.

#plot CD4 data. 
plot(HIVdata$viral_load~HIVdata$CD4, col=c("red", "steelblue1"), ylab = "Viral Load", xlab = "CD4 Content")

