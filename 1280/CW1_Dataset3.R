# Name: Justyna Gredecka
# Student ID: 180991611
# Dataset 3

hiv_data <- read.table("~/BIO782P_JG/part_3_student_1280.tdf", header=TRUE)
attach(hiv_data)

# Check the data frame.
str(hiv_data)
head(hiv_data)
summary(hiv_data)

# Explore the effect of each explanatory variable on the HIV viral load.
# Tissue and CD4 look like potential predictors but we can test this further with stepwise regression.
jpeg(filename="~/BIO782P_JG/CW1_Dataset3_exploratory_plots.jpeg")
par(mfrow=c(2,2))
plot(VLoad ~ tissue, xlab="Tissue", ylab="Viral Load log10 (particles/ml)", names=c("Brain", "Spinal Cord"))
plot(VLoad ~ CD4, xlab="CD4+ Cell Count", ylab="Viral Load log10 (particles/ml)", names=c("High", "Low"))
plot(VLoad ~ score_distance, xlab="Relative Genetic Distance", ylab="Viral Load log10 (particles/ml)")
plot(VLoad ~ score_shannon, xlab="Average Population Diversity (Shannon)", ylab="Viral Load log10 (particles/ml)")
dev.off()

# Create full and null models to be used in stepwise regression below.
full.model=lm(VLoad ~ tissue+CD4+score_distance+score_shannon, data=hiv_data)
null.model=lm(VLoad ~ 1, data=hiv_data)

# Bidirectional selection by AIC.
bi.model <- step(null.model, scope = list(upper=full.model), data=hiv_data, direction="both")

# Forward selection by AIC.
forward.model <- step(null.model, scope=list(lower=null.model, upper=full.model), direction="forward")

# Backward elimination by AIC.
backward.model <- step(full.model, data=hiv_data, direction="backward")

# All models have given identical results. Check the statistics for one of the models.
# Since the predictors are categorical, a word equation will be used to represent the final model instead of the intercept and coefficients.
summary(bi.model)

# Check the diagnostic plots for one of the final models.
jpeg(filename="~/BIO782P_JG/CW1_Dataset3_diagnostic_plots.jpeg")
par(mfrow=c(2,2))
plot(bi.model)
dev.off()
