##########################################################
                    #Dataset 1
##########################################################

# import data and save to object called "microbiota"
microbiota <- read.table("part_1_student_1812.tdf", header = T)
# Check data
str(microbiota)
# Attach the dataframe
attach(microbiota)
# Set up new variable which is absolute value
unifrac <- abs(UniFracInd)

#### For Q1
# Draw boxplot
par(mfrow = c(1,2))
plot(latitude, unifrac, xlab = "Latitude", ylab = "distance", col = "steelblue")
# Calculate variances 
mean(unifrac[latitude == "temperate"])
mean(unifrac[latitude == "tropical"])

#### For Q2
# Draw boxplot
plot(season, unifrac, xlab = "Season", ylab = "distance", col = "steelblue")
# Calculate variances in different group
mean(unifrac[season == "Jan"])
mean(unifrac[season == "Aug"])

#### For Q3
# Fit the model
model <- lm(unifrac ~ latitude * season)
# Test for significant interaction
summary(model)
drop1(model, test = "F")
# diagnostic plots
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))
interaction.plot(latitude, season, unifrac, main = "The interaction between the season and the latitude")

##########################################################
                    #Dataset 2
##########################################################

# import data and save to object called "data2"
data2 <- read.table("part_2_student_1812.tdf", header = T)
# Check data
str(data2)
attach(data2)

# Visualise data
plot(distance, expression_fold, col = "steelblue", pch = 16, xlab = "genetic distance", ylab = "gene expression level", main = "The relationship between genetic distance and homologous gene expression")

# Fit the model
model2 <- lm(expression_fold ~ distance)
summary(model2)
par(mfrow=c(2,2))
plot(model2) # according to "Residuals vs Fitted" and Q-Q plot, this model looks not so good

model3 <- lm(log(expression_fold) ~ distance)
summary(model3)
plot(model3) # it looks better

# plot with fitted line
par(mfrow=c(1,1))
plot(distance, log(expression_fold), col = "steelblue", pch = 16, xlab = "genetic distance", ylab = "gene expression level", main = "The relationship between genetic distance and homologous gene expression")
abline(a = 0.47, b = -0.22, col = "red")

##########################################################
                 #Dataset 3
##########################################################

# import data and save to object called "hiv"
hiv <- read.table("part_3_student_1812.tdf", header = T)
# Check data
str(hiv)
attach(hiv)
boxplot(VLoad) # Vload looks like normal distributed

# Fit a model
my_model <- lm(VLoad ~ CD4 * tissue * score_shannon * score_distance)
anova(my_model)
summary(my_model)
drop1(my_model, test = "F")

my_model2 <- lm(VLoad ~ CD4 + tissue)
summary(my_model2)
anova(my_model, my_model2)
par(mfrow=c(2,2))
plot(my_model2) # according to the Q-Q plot, residuals are normal distributed


#### Model selection by stepwise AIC
# backwards
backward_final <- step(lm(VLoad ~ CD4 * tissue * score_shannon * score_distance), direction="backward")
# forwards
forward_final <- step(lm(VLoad ~ tissue),scope=(~ CD4 * tissue * score_shannon * score_distance),direction="forward")

anova(my_model, my_model2, backward_final, forward_final)
anova(my_model2, backward_final, test = "F")
anova(my_model2, forward_final, test = "F")

# both direction
final_final <- step(lm(VLoad ~ tissue),scope=c(lower = ~ tissue, upper = ~ CD4 * tissue * score_shannon * score_distance))
summary(final_final)
anova(my_model2, final_final)
