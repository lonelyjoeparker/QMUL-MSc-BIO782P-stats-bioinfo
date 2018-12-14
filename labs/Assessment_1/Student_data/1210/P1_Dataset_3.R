######## "Dataset 3: HIV viral load and within-patient population dynamics"
######## Author: HAJAR SAIHI
######## Due: 14 December
######## Examiner: Dr Parker
################################################################################################################################################

#### Dataset 3: HIV viral load and within-patient population dynamics
HIV_data <- read.table('part_3_student_1210.tdf', header = TRUE, sep='')
str(HIV_data)

par(mfrow=c(2,2))

### (1) Check for significance of interaction and main effects: Population diversity explained by all variables
model_0 <- aov(VLoad~score_shannon*score_distance*tissue*CD4, data = HIV_data) # First check for interaction across all variables to explain VLoad
anova(model_0) # CD4 and score_shannon appear significant.

### (2) Model Selection using stepwise AIC 
step(lm(VLoad~score_shannon+tissue+CD4+score_distance, data = HIV_data), direction = 'forward') # Carry out a forward step model search
step(lm(VLoad~score_shannon+tissue+CD4+score_distance, data = HIV_data), direction = 'backward') # Carry out a backward step model search
step(lm(VLoad~1, data = HIV_data),scope=(~score_shannon+tissue+CD4+score_distance), direction = 'both') # Use a scope search to carry out both forward and backward model searches

### (3) Extra Checks: Check if the week of sample collection had any interaction or main effects on viral load.
weekID <- c(1:40) # Set the week IDs
HIV_data["Week"] <- weekID # Add a new column called Week and use the IDs as the dataset

mod_with_week<-lm(VLoad~Week*score_shannon*CD4*score_distance*tissue, data=HIV_data) # Check if Week has any interacting/main effects on viral load
anova(mod_with_week) # Assess results using anova function

### (4) Final Model! 
par(mfrow=c(2,2))

final_model <- lm(VLoad~score_shannon+CD4, data = HIV_data) # The final best model using stepwise AIC
anova (final_model)
summary(final_model)
plot(final_model)

par(mfrow=c(1,1))
plot (VLoad~score_shannon, data = HIV_data, # Plot changes in viral load explained by shannon scores
      main='Changes in Viral Load concentration against \nShannon-Weiner Scores for high and low CD4',
      cex.main = 0.9,
      xlab = 'Shanon-Weiner Diversity Index',
      ylab = 'Viral Load (log10(number of viral particles per ml))',
      cex.lab=0.8)

abline(-1.0546,-1.0666, col = 'blue', lwd=1.5) # add the first line of regression for high CD4
abline(1.3421+-1.0546,-1.0666, col = 'purple',lwd=1.5) # add the second line of regression for low CD4

legend(3.95,2,legend=c('CD4 High', 'CD4 Low'), col=c('blue','purple'), lwd=1.5, lty=1, cex=0.7)

