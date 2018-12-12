# Name: Justyna Gredecka
# Student ID: 180991611
# Assessment 1: Dataset 1

diversity_data <- read.table("~/BIO782P_JG/part_1_student_1280.tdf", header=TRUE)
attach(diversity_data)

# Check the data frame.
head(diversity_data)
str(diversity_data)
summary(diversity_data)

# Produce a boxplot to show how diversity relates to latitude.
jpeg(filename="~/BIO782P_JG/diversity_latitude_boxplot.jpeg")
boxplot(UniFracInd ~ latitude, names=c("Temperate Seawater", "Tropical Seawater"), xlab="Location", ylab="Relative Microbial Diversity (UniFranc)")
dev.off()

# Produce a boxplot to show how diversity relates to time of year.
jpeg(filename="~/BIO782P_JG/diversity_seasons_boxplot.jpeg")
boxplot(UniFracInd ~ season, names=c("August", "January"), xlab="Season", ylab="Relative Microbial Diversity (UniFranc)")
dev.off()

# Prior to the t-tests, check that each sample is approximately normally distributed.
# Note: the default Welch's t-test does not require the samples to have similar variances.
trop_samples <- UniFracInd[latitude=="tropical"] 
temp_samples <- UniFracInd[latitude=="temperate"]
jan_samples <- UniFracInd[season=="Jan"]
aug_samples <- UniFracInd[season=="Aug"]
jpeg(filename="~/BIO782P_JG/Dataset_1_histagrams.jpeg")
par(mfrow=c(2,2))
hist(trop_samples)
hist(temp_samples)
hist(jan_samples)
hist(aug_samples)
dev.off()

#QUESTION 1
# Carry out a t-test between temperate and tropical data.
div_latitude <- t.test(UniFracInd ~ latitude)
div_latitude

#QUESTION 2
# Carry out a t-test between January and August data.
div_seasons <- t.test(UniFracInd ~ season)
div_seasons

# QUESTION 3
# Fit a model with an interaction between season and latitude.
interactions <- lm(UniFracInd ~ season * latitude)

# Check diagnostic plots for the interaction model.
jpeg(filename="~/BIO782P_JG/Dataset_1_diagnostics.jpeg")
par(mfrow=c(2,2))
plot(interactions)
dev.off()

# Test the interaction for statistical significance and produce interaction plots. 
drop1(interactions, test = "F")
jpeg(filename="~/BIO782P_JG/Dataset_1_interactions.jpeg")
par(mfrow=c(2,1))
interaction.plot(season, latitude, UniFracInd)
interaction.plot(latitude, season, UniFracInd)
dev.off()