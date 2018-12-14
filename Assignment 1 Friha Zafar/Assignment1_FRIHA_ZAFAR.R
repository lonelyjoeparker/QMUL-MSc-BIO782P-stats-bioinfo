#############################Assignment 1######FRIHA ZAFAR##############################


#############DATASET1: part_1_student_1068######################################


setwd("~/apocrita/Statistics and Bioinformatics/Assignment 1") #PLEASE CHANGE THE WORKING DIRECTORY.
dataset_1<-read.table("part_1_student_1068.tdf", header = TRUE) #the dataset was called 'dataset_1'
str(dataset_1) #the structure of the dataset was checked to ensure the correct variables were listed
hist(dataset_1$UniFracInd) #to check if the data is normally distributed, which it was 


#to find How does microbial diversity change with latitude?

plot(dataset_1$latitude, dataset_1$UniFracInd, xlab="Latitude", ylab="Microbial Diversity (UniFrac Index)",main="Change in Microbial Diversity (UniFrac Index) with Latitude", col=c("magenta","darkgreen"))  #a box plot is made
latitude_Unifracind<-t.test(dataset_1$UniFracInd~dataset_1$latitude) #Student t-test was condcuted to find a relationship between the two variables 
latitude_Unifracind #t = 0.31046, df = 37.276, p-value = 0.7579 which is more than 0.05 therefore the H0 can be accepted and H1 is rejected


#How does microbial diversity change with time of year?

plot(dataset_1$season , dataset_1$UniFracInd, xlab="Season", ylab="Microbial Diversity (UniFrac Index)", main="Change in Microbial Diversity (UniFrac Index) with Season", col=c("orange","steelblue") ) 
season_Unifracind<-t.test(dataset_1$UniFracInd~dataset_1$season) #Student t-test was condcuted to find a relationship between the two variables 
season_Unifracind #t = 2.2522, df = 36.373, p-value = 0.03043 which is less than 0.05 therefore the H0 can be rejected and H1 is accepted

# Is there an interaction between the season, and location?

model1<-lm(dataset_1$UniFracInd~dataset_1$latitude*dataset_1$season) 
plot(model1) # shows normal distribution in Normal Q-Q plot as points are close to line, homoscedacity in R vs F. 
drop1(model1, test="F")#no interaction was seen between dataset_1$season:dataset_1$latitude
boxplot(dataset_1$UniFracInd~interaction(dataset_1$season, dataset_1$latitude), main="The Change in Microbial Diversity (UniFrac Index) with Season and Latitude" ,col=c("orange","steelblue"), ylab="Microbial Diversity (UniFrac Index)", las=1.0, cex.axis = 1.5, cex.lab=1.5,  cex.main=2.5)
interaction.plot(dataset_1$season, dataset_1$latitude, dataset_1$UniFracInd, main="The interaction between Season and Latitude", xlab="Season", ylab="Microbial Diversity (UniFrac Index)") #Interaction plot between the season and location when the diversity is the response #Interaction plot between the season and latitude when the diversity is the response
anova(model1) #dataset_1$season had a F-value of 0.03197 * which was statistically significant at 0.05


############DATASET2: part_2_student_1068######################################

dataset_2<-read.table("part_2_student_1068.tdf", header = TRUE)
str(dataset_2)



#How does the putative 'luciferase' homologue expression change with genetic distance (amino acid substitutions)?

plot(dataset_2$distance,dataset_2$expression_fold, xlab = "Genetic Distance (Amino Acid Substitutions) ", ylab = "Luciferase Homologue Expression Change", main="The Putative 'Luciferase' Homologue Expression Change with Genetic Distance",col="red",pch=19,  las=1.0, cex.axis = 1.5, cex.lab=1.5,  cex.main=2.5) #This plots the two values 
model3<-lm(dataset_2$expression_fold~dataset_2$distance) #linear model for finding the relationship between the two variables
summary(model3)
abline(model3) #fits the model onto the scatter graph, the scatter is large, the model has a slight neg correlation. 
anova(model3) #An anova was conducted to find a significant relationship between genetic distance and expression change.

CHECK_CORRELATION<-cor.test(dataset_2$distance,dataset_2$expression_fold, method=c("pearson"))  #I checked for a correlation between the two variables by conducting the Pearson's correlation test
CHECK_CORRELATION

#This gave: 
#Pearson's product-moment correlation data:  dataset_2$distance and dataset_2$expression_fold
#t = -0.57331, df = 28, p-value = 0.571  ###THE P-VALUE IS VERY HIGH >0.05
#alternative hypothesis: true correlation is not equal to 0     ###THIS MEANS THERE IS NO SIGNIFICANT DIFFERENCE BETWEEN THE TWO VARIABLES.
#95 percent confidence interval:
 # -0.4505016  0.2627509
#sample estimates:
 # cor 
#-0.1077147 ###THE PEARSON CORRELATION COEFFICIENT IS NEGATIVE MEANING THERE IS A SLIGHT NEGATIVE CORRELATION HOWEVER THE VALUE IS TOO SMALL, IT SHOULD BE CLOSE TO -1. 

 


#Comment on whether the model assumptions are valid.
par(mfrow=c(2,2))
plot(model3)  #The diagnostic plots were analysed and were found to show a normal distribution of the residuals as well as homoscedascity.

#The linear model's assumptions were found to be valid as the diagnostic plots indicate that the model assumptions were met. 
#The 'Residuals vs Fitted' graph gave a linear pattern with residuals that were spread equally. 
#The residuals were normally distributed, seen in the 'Normal Q-Q' plot. 
#The 'Scale-Location' plot shows that the residuals are equally spread on a horizontal line which verifies the homoscedasticity of the residuals.
#The 'Residuals vs Leverage' plot shows that there were no extreme residuals therefore no outliers influenced the regression line. 









############DATASET3: part_3_student_1068######################################

setwd("~/apocrita/Statistics and Bioinformatics/Assignment 1/DATASET3")
dataset_3<-read.table("part_3_student_1068.tdf", header = TRUE)
str(dataset_3)

#To find the effect of CD4+ cell counts on HIV viral load
plot(dataset_3$VLoad~dataset_3$CD4, xlab = " CD4+ Cell Counts", ylab = "HIV Viral Load (log10(number of viral particles per ml))", main="CD4+ Cell Counts and HIV Viral Load",col=c("pink","lightblue"),   las=1.0, cex.axis = 1.5, cex.lab=1.5,  cex.main=2.5)

#To find the effect of tissue type on HIV viral laod
plot(dataset_3$VLoad~dataset_3$tissue, xlab = " Tissue Type", ylab = "HIV Viral Load (log10(number of viral particles per ml))", main= "HIV Viral Load in Different Tissue Types", col=c("yellow", "lightgreen"),   las=1.0, cex.axis = 1.5, cex.lab=1.5,  cex.main=2.5)

#To  find the effect of Shannon population diversity on HIV viral load
plot(dataset_3$VLoad~dataset_3$score_shannon,xlab = "Average Shannon Population Diversity", ylab = "HIV Viral Load (log10(number of viral particles per ml))", main= "The Effect of Average Shannon Population Diversity on HIV Viral", col="blue",pch=19,   las=1.0, cex.axis = 1.5, cex.lab=1.5,  cex.main=2.5)
#No trend could be seen

#To  find the effect of the mean pairwuse genetic distance on HIV viral load
plot(dataset_3$VLoad~dataset_3$score_distance, xlab = "Mean Pairwise Genetic Distance", ylab = "HIV Viral Load (log10(number of viral particles per ml))", main= "The Effect of Mean Pairwise Genetic Distance on HIV Viral", col="green", pch=19,   las=1.0, cex.axis = 1.5, cex.lab=1.5,  cex.main=2.5)
#no trend can be seen

##Selecting a model:##
#A model was made manually to see the effect of each variable on the HIV Viral load
model_data3<-lm(dataset_3$VLoad~dataset_3$CD4*dataset_3$tissue*dataset_3$score_shannon*dataset_3$score_distance)
anova(model_data3)   #The ANOVA shows that the CD4, Tissue and dataset_3$tissue:dataset_3$score_shannon have an affect on Viral load
summary(model_data3)

#automated backwards model selection
backwards_dataset3=step(lm(dataset_3$VLoad~dataset_3$CD4*dataset_3$tissue*dataset_3$score_shannon*dataset_3$score_distance),direction="backward")

#automated forwards  model selection
forward_dataset3=step(lm(dataset_3$VLoad~dataset_3$CD4),scope=(~dataset_3$CD4*dataset_3$tissue*dataset_3$score_shannon*dataset_3$score_distance),direction="forward")

#automated model selection for both directions
both_dataset3=step(lm(dataset_3$VLoad~dataset_3$CD4*dataset_3$tissue*dataset_3$score_shannon*dataset_3$score_distance),direction="both")

anova(model_data3, backwards_dataset3, forward_dataset3, both_dataset3) #ANOVA was conducted to find the best model the third model appeared to be the best since the fourth model was too complex and didn't make sense.
summary(forward_dataset3)#the summary shows that there is a significant difference in CD4-low and Tissue-spinalcord. But score-distance was not significant therefore it was removed

#The final model
final_model_dataset3<-lm(formula = dataset_3$VLoad ~ dataset_3$CD4 + dataset_3$tissue)

#The model was plotted to check for validity using the diagnostic plots
plot(final_model_dataset3)

anova(final_model_dataset3) #The ANOVA shows that the CD4+ cell count and tissue type have a significant effect on viral load. 

#since the anova showed that the CD4 was significant a t-test was conducted
viralload_cd4<-t.test(dataset_3$VLoad~dataset_3$CD4) 
viralload_cd4#t = -3.4244, df = 29.68, p-value = 0.001821  the H0 was rejected and the H1 was accepted

#since the anova showed that the tissue was significant a t-test was conducted
viralload_tissue<-t.test(dataset_3$VLoad~dataset_3$tissue) 
viralload_tissue  #t = -2.7823, df = 37.948, p-value = 0.008363   the H0 was rejected and the H1 was accepted

#To find the effect of time on viral load, the week numbers were added to the dataset in order to plot the relationship:
setwd("~/apocrita/Statistics and Bioinformatics/Assignment 1/DATASET3")
dataset_3_edited<-read.table("part_3_student_1068edited.tdf", header = TRUE)
str(dataset_3_edited)

#Viral load against week number is plotted
plot(dataset_3_edited$VLoad~dataset_3_edited$Week, xlab = "Week Number", ylab = "HIV Viral Load", main= "The Effect of Time on HIV Viral", col="purple",pch=19,  las=1.0, cex.axis = 1.5, cex.lab=1.5,  cex.main=2.5)

#A linear model was made for finding the a relationship between viral load and time(week)
regression_model_dataset3<-lm(dataset_3_edited$VLoad~dataset_3_edited$Week) #A regression line is added to the plot

#The regression_model_dataset3's diagnostic plots show that the relationship is linear, residuals are normally distributed, residuals show homoscedasticity since they are equally spread across the horizontal line and the residuals are outside the Cook's distance and that there were no extreme values. 
plot(regression_model_dataset3)

#Viral load against week number is plotted 
plot(dataset_3_edited$VLoad~dataset_3_edited$Week, xlab = "Week Number", ylab = "HIV Viral Load (log10(number of viral particles per ml))", main= "The Effect of Time on HIV Viral", col="purple",pch=19,  las=1.0, cex.axis = 1.5, cex.lab=1.5,  cex.main=2.5)
abline(regression_model_dataset3)  #the regression line is plotted a positive correlation is see where an increase in week number leads to an increase HIV viral load.
CORRELATION<-cor.test(dataset_3_edited$VLoad,dataset_3_edited$Week, method=c("pearson"))  #I checked for a correlation between the two variables by conducting the Pearson's correlation test
CORRELATION

#Pearson's product-moment correlation

#data:  dataset_3_edited$VLoad and dataset_3_edited$Week
#t = 3.9923, df = 38, p-value = 0.0002889                   ###EVENTHOUGH THE P-VALUE IS LOW, THE CORRELATION R VALUE IS NOT SIGNIFICANT SINCE IT IS NOT CLOSE TO +1/-1 
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  0.2793960 0.7312734
#sample estimates:
#  cor 
#0.5435939  ###TOO LOW FOR IT TO BE SIGNIFICANT