
#title: "BIO782P Statistics and Bioinformatics Assignment 1- Dataset 1"
#Name: "Ishack M Mougamadou
  
#Student ID: 150346599

#This analysis was run on  R Studio Version 1.1.456
#Mac OS El Capitan 
#Intel i5
#4GB RAM

#Please Note: A pull request has been submitted on GitHub

## Dataset 1: Marine microbial diversity

#Imported the microbial dataset into Rstudio.

marine_data = read.table("Datasets/part_1_student_1887.tdf", header = TRUE) # Imported the microbial dataset
marine_data # view the data


#Exploratory Data Analyses

str(marine_data) # Explored the number and the type of variables (Numeric and Factor) of the dataset


#Assigned the three variables to appropriate variable names.  


Microbial_Diversity = marine_data$UniFracInd # The microbial diversity is measured by UniFrac  
Latitude = marine_data$latitude # The latitude 
Season = marine_data$season # The Season


#Summarised the microbial diversity dataset.


aggregate(.~marine_data$latitude, marine_data[1], summary) # Summarised UniFrac based on the latitude
aggregate(.~marine_data$season, marine_data[1], summary) # Summarised UniFrac based on the season


#Created a frequency distribution graph of the UniFrac residuals.

hist(Microbial_Diversity, col= "yellow", main="The Frequency Distribution of the UniFrac Residuals",xlab="UniFrac Residuals") # A frequency distribution graph of the UniFrac residuals


## Part 1: How does microbial diversity change with latitude?

#Created a frequency distribution graph of the of UniFrac residuals (Microbial Diversity) based on the latitude.

boxplot(Microbial_Diversity~Latitude, col=c("mistyrose","powderblue"),  # Boxplot based on latitude
        main="The Distribution of Microbial Diversity based on the Latitude",  # Title
        ylab= "Microbial Diversity (UniFrac)",xlab="Latitude", # X and Y label
        pch=4) # Change the shape of the outliers to a cross to make it for distinct



#Tested for the assumption of normally distributed UniFrac residuals.

#The diagnostic plots for the Microbial Diversity dataset.

diagnostic_plot=lm(Microbial_Diversity~Latitude) # Model used for the diagnostic plots
plot(diagnostic_plot) # Plotting the diagnostic plots of the model


#Subsequently, due to non-normally distributed UniFrac residuals, the Man-Whitney U test (non-parametric) was used to identify if the latitude affected the microbial diversity. Please note, the function for the Wilcox and Mann-Whitney U test is the same in R Studio.

wilcox.test(Microbial_Diversity~Latitude) # Mann-Whitney U test of the microbial diversity based on latitude

## Part 2: How does microbial diversity change with time of year?

#Created a boxplot to see the distribution of the microbial diversity based on the two seasons.


boxplot(Microbial_Diversity~Season,col=c("gold","royalblue"), # Boxplot based on seasons 
        main="The Distribution of the Microbial Diversity based on the Seasons", # Title
        ylab= "Microbial Diversity (UniFrac)",xlab="Season", pch=4) # X and Y label with a cross for outliers


#The Man-Whitney U test was used again to identify if the season affected microbial diversity. Please note, the function for the Wilcox and Mann-Whitney U test is the same in R Studio.

wilcox.test(Microbial_Diversity~Season) # # Mann-Whitney U test of the microbial diversity based on season


## Part 3: Is there an interaction between the season, and location?

#ANOVA was used to see if there was an interaction between the location and the time of year. The response variable was the Microbial Diversity (UniFrac).

model1=lm(Microbial_Diversity~Latitude*Season) # Model to see if there is any interaction between the season and location based on the UniFrac (Microbial Diversity)
anova(model1) # ANOVA was used to identify any significant differences in the means between the groups

