
#title: "BIO782P Statistics and Bioinformatics Assignment 1- Dataset 2"
#Name: "Ishack M Mougamadou"

  
#Student ID: 150346599
  
#This analysis was run on  R Studio Version 1.1.456
#Mac OS El Capitan 
#Intel i5
#4GB RAM


#Dataset 2- Pairwise nucleotide substitutions and RNA expression levels:

#Imported RNA expression dataset into Rstudio.

RNA_expression_data=read.table("Datasets/part_2_student_1887.tdf", header = TRUE) # Imported the RNA dataset
RNA_expression_data # view the dataset


#Exploratory Data Analyses
#Explored the number and the type of variables (Numeric and Factor) of the RNA dataset.

str(RNA_expression_data) # Explored the number and the type of variables (Numeric and Factor) of the dataset


#Assigned the two variables to appropriate variable names.  

Homologue_expression=RNA_expression_data$expression_fold #The homologue expression 
Genetic_distance=RNA_expression_data$distance # The genetic distance


#Summarised the RNA expression dataset 

summary(RNA_expression_data) # Summary of dataset


#Created a frequency distribution graph of the homologue expression residuals.


hist(Homologue_expression, col= "lightblue", main="The Frequency Distribution of the Homologue Expression Residuals",xlab="Homologue Expression Residuals") # A frequency distribution graph of the homologue expression residuals


#Created a frequency distribution graph of the genetic distance residuals.


hist(Genetic_distance, col= "lightpink", main="The Frequency Distribution of the Genetic Distance Residuals",xlab="Genetic Distance Residuals") # A frequency distribution graph of the genetic distance residuals


#Part 1: How does the putative 'luciferase' homologue expression change with genetic distance (amino acid substitutions)?

#A correlation scatterplot was used to indicate any relationship between the Genetic Distance and the Homologue Expression Change.

RNA_expression_model=(Homologue_expression~Genetic_distance) # Model to identify correlation 
plot(RNA_expression_model,main="Genetic Distance vs Homologous Expression Change",ylab = "Homologue Expression Change", xlab="Genetic Distance", pch=20) # the scatterplot
abline(lm(RNA_expression_model), col="red",) # adding a linear regression line



#Part 2: Comment on whether the model assumptions are valid.


#Tested for the assumption of normally distributed homolgue expression and genetic distance residuals.

#The diagnostic plots for the RNA expression dataset.

diagnostic_plot=lm(Homologue_expression~Genetic_distance) # Model used for the diagnostic plots
plot(diagnostic_plot) # Plotting the diagnostic plots of the model



summary(diagnostic_plot) # to find the degrees of freedom (DF)



#Conducted a Pearson Correlation between the homologue expression and the genetic distance because the residuals were normally distributed and followed a linear pattern in the scatterplot.

cor.test(Homologue_expression,Genetic_distance, method = "pearson") # Pearson correlation between the homologue expression and the genetic distance


#Part 3: Assuming your model is statistically valid, can you guess what effect is responsible for the relationship you've found?


#Please see the report for the answer to this question. Thank you.






