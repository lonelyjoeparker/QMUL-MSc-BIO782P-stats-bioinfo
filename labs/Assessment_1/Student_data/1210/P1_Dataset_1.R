######## "Dataset 1: Marine microbial diversity"
######## Author: HAJAR SAIHI
######## Due: 14 December
######## Examiner: Dr Parker
################################################################################################################################################

#Dataset 1: Marine microbial diversity ####
marine_diversity <- read.table('part_1_student_1210.tdf', header = TRUE, sep='')
str(marine_diversity)

par(mfrow=c(1,1))
boxplot (UniFracInd~latitude, col= 'grey', # Create a box plot of Microbial diversity explained by latitude
         ylab = 'Microbial Diversity', 
         xlab = 'Latitude', 
         main = 'Micobial diversity for Temperate and Tropical waters', 
         cex.main = 0.8, 
         cex.lab = 0.8, 
         data = marine_diversity)

boxplot (UniFracInd~season, col= 'grey', # Create a box plot of Microbial diversity explained by season
         ylab = 'Microbial Diversity', 
         xlab = 'Season', 
         main = 'Micobial diversity for different seasons', 
         cex.main = 0.8, 
         cex.lab = 0.8, 
         data = marine_diversity)
#(1)
t.test(UniFracInd~latitude, data = marine_diversity) # Use the t.test function to work out if there is a significant difference between tropical and temperate waters

#(2)
t.test(UniFracInd~season, data = marine_diversity) # Use the t.test function to work out if there is a significant difference between january and august collection seasons

#(3)
par(mfrow=c(2,2))
marine_model <- lm(UniFracInd~latitude*season, data = marine_diversity) # Create a model to see if there is an interaction between latitude and season to explain microbial diversity
anova(marine_model)

plot(marine_model) # Check whether model assumptions have been met.
shapiro.test(residuals(marine_model)) # Check for normality in residuals

