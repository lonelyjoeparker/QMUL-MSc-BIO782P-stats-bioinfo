setwd("~/Desktop") #set working directory to where the data is saved
HIV_population <- read.table("993_part3.tdf.txt", header=TRUE) #open file by reading data into table format
str(HIV_population) #check structure of data is correct
attach(HIV_population) #attach data so able to use variables directly w/o $
null=lm(viral_load~1,data=HIV_population) #starting point of linear model - viral load explained by 1st variable
full=lm(viral_load~.,data=HIV_population) #range of lmodel - viral load explained by all variables
f_step <- step(null,scope =list(lower=null, upper=full), direction = "forward") #forward selection alogrithm, searches through models in range of null-full, starting with null - ends with the variables that should not be used
b_step <- step(full, data=HIV_population, direction = "backward") #for confirmation - backward elmination, starts at full and elimates variables till best are found
##these algorithms generate a best model using variables with the lowest AIC - these are the varibles that are most suitable and should be used to explain viral_load

plot(viral_load~CD4, xlab= "CD4+ T-cell count", ylab="Viral load (log10(number of viral particles per ml))", main="Effect of CD4+ cell counts on HIV viral load", col = c("orange", "brown")) #boxplot showing effect of level of CD4+ cells on HIV viral load
plot(viral_load~Distance, xlab= "Mean pairwise genetic distance", ylab="Viral load (log10(number of viral particles per ml))", main="Effect of evolutionary distance on HIV viral load", col ="orange") #scatterplot of viral load explained by evolutionary distance
best_model <- lm(viral_load~Distance) #linear regression model needed to fit regression line
abline(best_model, col = "red") #fit regression line