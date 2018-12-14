
#title: "BIO782P Statistics and Bioinformatics Assignment 1- Dataset 3"
#Name: "Ishack M Mougamadou"

  
#Student ID: 150346599
  


#This analysis was run on  R Studio Version 1.1.456
#Mac OS El Capitan 
#Intel i5
#4GB RAM

#Dataset 3: HIV viral load and within-patient population dynamics

#Imported the HIV dataset into Rstudio


HIV_data=read.table("Datasets/part_3_student_1887.tdf", header = TRUE) # imported the dataset into Rstudio
HIV_data


#Exploratory Data Analyses


str(HIV_data) # Explored the number and the type of variables (Numeric and Factor) of the dataset



#Added the Week column to the HIV dataset.

HIV_data_new=cbind(Week=(1:40),HIV_data)# add week 1 to 40



#Assigned the six variables to appropriate variable names.  

Week=HIV_data_new$Week # Week
Viral=HIV_data_new$VLoad # Viral Load
Shannon=HIV_data_new$score_shannon # Score Shannon
Distance=HIV_data_new$score_distance # Score Distance
Tissue=HIV_data_new$tissue # Tissue
CD4=HIV_data_new$CD4 #CD4



#Summarised the HIV dataset.


summary(HIV_data_new) # Summary of the HIV dataset


#Created a frequency distribution graph of the Viral Load residuals.


hist(Viral, col= "lightgreen", main="The Frequency Distribution of the Viral Load Residuals",xlab="Viral Load Residuals") # A frequency distribution graph of the Viral Load residuals


#Boxplots

#Boxplots of the variables that seem to have explanatory power in relation to the viral load.


boxplot(Viral~CD4, col=c("red","powderblue"),  # Boxplot of VLoad vs CD4+
main="VLoad vs CD4+ Count",  # Title
xlab= "CD4+ Count",ylab="VLoad") # X and Y label

aggregate(.~HIV_data$tissue, HIV_data[1], summary) # Summarised UniFrac based on tissue

#Diagnostic Plots

#Diagnostic plots to assess the assumptions of the model.

diagnostic_model=lm(Viral~CD4*Shannon*Tissue*Distance) # Diagnostic model
plot(diagnostic_model) # Plot the model


##Choosing a Model of Best Fit

#To find the best model to explain the viral population size in relation to the other variables, the following models were run and then compared with each other to determine the best model:
  
#1. Manual ANOVA model
#2. Automatic Stepwise model (Forward) 
#3. Automatic Stepwise model (Backward)  
#4. Automatic Stepwise model (Bidirectional)  

# Model Selection by ANOVA (Manual)

#Fitting a maximum model manually. 

manual_all_model=lm(Viral~Tissue*Shannon*CD4*Distance) # Fitting a manual maximum model
anova(manual_all_model) # ANOVA of the model


#Fitting a model with the most significant interactions. 

manual_model_significant=lm(Viral~Tissue*Shannon) # Most significant model
anova(manual_model_significant) # ANOVA of the model

# Model Selection by stepwise AIC

#Forward model - start with a model containing simple interactions and end with a model containing complex interatcions.


forward_final=step(lm(Viral~Tissue*Shannon),scope =c(~Viral*CD4*Tissue*Shannon*Distance), direction ="forward") # Forward model



#Backward model - start with a model containing complex interactions and end with a model containing simple interactions.

backward_final=step(lm(Viral~Shannon*Tissue*Distance*CD4), direction ="backward") # Backward model


#Model with a bidirectional search, going both directions. 

both_final=step(lm(Viral~Tissue*CD4),scope =c(~Viral*CD4*Tissue*Shannon*Distance), direction ="both")


anova(backward_final,forward_final,both_final, manual_model_significant) # ANOVA of all four models


#The relationship between the Viral Load and the Shannon score

plot(Viral~Shannon,main=" The Viral Load Vs The Shannon Score ",ylab = "Viral Population Size (Log10)", xlab="Average Shannon Population Diversity", pch=20) # the scatterplot
abline(lm(Viral~Shannon), col="red",) # adding a linear regression line


#A Pearson's Correlation between the Viral Load and the Shannon Score

cor.test(Viral,Shannon, method="pearson") # Pearson's Correlation between the Viral Load and Shannon Score

#Created a boxplot to see the distribution of the viral population size based on the tissue.



boxplot(Viral~Tissue, col=c("blue","gold"),  # Boxplot of VLoad vs Tissue
        main="The Viral Load vs The Tissue",  # Title
        xlab= "Type of Tissue Specimen",ylab="Viral Population Size (log10) ") # X and Y label


