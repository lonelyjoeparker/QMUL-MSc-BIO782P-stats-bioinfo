######## "Dataset 2: Pairwise nucleotide substitutions and RNA expression levels"
######## Author: HAJAR SAIHI
######## Due: 14 December
######## Examiner: Dr Parker
################################################################################################################################################

#### Dataset 2: Pairwise nucleotide substitutions and RNA expression levels ####
pairwise_sub <- read.table('part_2_student_1210.tdf', header = TRUE, sep='')

par(mfrow=c(1,1))
#str(pairwise_sub)
intial_plot <- plot(expression_fold~distance, data = pairwise_sub, # Create a plot to see how expression fold changes with distance
                    xlab = 'Genetic Distance (Amino Acid Substitutions)',
                    ylab = 'Luciferase Homologue Expression ', 
                    main = 'Changes in Luciferase homologue\nexpression against increasing genetic distances',
                    cex.main=0.9, cex.lab = 0.8)

initial_model <- lm(expression_fold~distance, data = pairwise_sub) # Define a model where genetic pairwise distances explain expression fold of the luciferase
#plot (initial_model) 
abline(initial_model, col='purple') # Use abline to plot the regression line of the initial model
#shapiro.test(residuals(initial_model)) # Check for normality violations of the assumptions made on the residuals

best_model <- lm(log(expression_fold+5)~distance, data = pairwise_sub) # Use log() to transform the data
anova(best_model) # Carry out anova to check if the model significantly explains the data
summary(best_model) # The summary() function will give more information on the intercept and slope values
#plot (best_model)
shapiro.test(residuals(best_model)) #Check for normality violations of the assumptions made on the residuals

plot(log(expression_fold+5)~distance, data = pairwise_sub, # plot the final best model
     xlab = 'Genetic Distance (Amino Acid Substitutions)', 
     ylab = 'Luciferase Homologue Expression (log(expression_fold+5)) ', 
     main = 'Changes in Luciferase homologue\nexpression against increasing genetic distances',
     cex.main=0.9, cex.lab = 0.8,
     ylim = c(0.8,2.2))

abline(best_model, col = 'blue') # Use abline to plot the regression line of the best model

