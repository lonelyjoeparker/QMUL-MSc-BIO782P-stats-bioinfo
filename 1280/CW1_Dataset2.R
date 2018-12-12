# Name: Justyna Gredecka
# Student ID: 180991611
# Dataset 2

expression_data <- read.table("~/BIO782P_JG/part_2_student_1280.tdf", header=TRUE)
attach(expression_data)

#Check the data frame.
str(expression_data)
head(expression_data)
summary(expression_data)

# Produce an intitial plot to visualise the relationship between the two variables.
plot(expression_fold ~ distance, xlab="Relative Genetic Distance", ylab="RNA Expression Level")

# Fit a simple linear regression model to the data.
expression_model <- lm(expression_fold ~ distance)
summary(expression_model)
abline(expression_model)

# Check if diagnostic plots meet all assumptions.
jpeg(filename="~/BIO782P_JG/CW1_Dataset2_diagnostics.jpeg")
par(mfrow=c(2,2))
plot(expression_model)
dev.off()