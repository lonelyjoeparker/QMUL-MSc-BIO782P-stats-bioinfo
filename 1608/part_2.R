###### Catherine Okuboyejo, 180279704
# data set 1608
# NOTE: script assumes all files in the current working directory
# I submitted a pull request on github

#### Dataset 2: Pairwise nucleotide substitutions and RNA expression levels ####
# read in data
luciferase_data <- read.table("part_2_student_1608.tdf", header = TRUE)

# explore data
str(luciferase_data)
par(mfrow=c(1,1))
plot(expression_fold ~ distance, data = luciferase_data) # looks like there is a strong relationship between the two variables

## How does the putative 'luciferase' homologue expression change with genetic distance
# linear regression
luciferase_model <- lm(expression_fold ~ distance, data = luciferase_data)
anova(luciferase_model)
summary(luciferase_model)

# plot regression line on scatter plot
plot(expression_fold ~ distance, data = luciferase_data, 
     xlab="Pairwise genetic distance", 
     ylab = "Luciferase expression fold change", 
     pch = 16)
abline(luciferase_model, col="indianred2", lty=2, lwd=2)

# Is the model statistically valid? - heterosced ok but residuals not normally distributed
par(mfrow=c(2,2))
plot(luciferase_model) # residuals look like they are not normally distributed

# plot and test residuals
par(mfrow=c(1,2))
hist(residuals(luciferase_model), breaks = 15) # distribution looks bimodal
plot(density(residuals(luciferase_model)))

# shapiro test H0=distribution not normal
shapiro.test(residuals(luciferase_model)) # definitely not normally distributed

# transform data to satisfy assumptions
plot(expression_fold ~ distance, data = luciferase_data, 
     xlab="Genetic distance", ylab = "Expression fold change")
# try log transformation  
luciferase_model2 <- lm(log(expression_fold+10) ~ distance, data = luciferase_data)
# had to add 5 to log to avoid heteroscedasticity
# plot diagnostics and residuals
par(mfrow=c(3,2)) 
plot(luciferase_model2)
anova(luciferase_model2)
hist(residuals(luciferase_model2))
plot(density(residuals(luciferase_model2))) # looks better

