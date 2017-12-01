#Part 1: Marine microbial diversity

#set the working directory to source file location under Session
#open the file and include headers:
Microbial_Diversity <- read.table("235_part1.tdf",header = TRUE)
#check that the data is in the right format:
str(Microbial_Diversity)
#ignore x and z variables in this dataset
#change variables to factors and numbers if appropriate
#change the factors of season to a specific order rather than alphabetically:
season <- factor(Microbial_Diversity$season,levels=c("Jan","Aug"),ordered = TRUE) 
#attach the variables to the data = do not have to constantly specify which data (Microbial_Diversity$): 
attach(Microbial_Diversity)

#compare the latitude with diversity:
par(mfrow=c(1,2))
plot(lattitude,diversity,xlab="Latitude",ylab="Microbial Diversity",main="A",col=c("skyblue1","skyblue4"))
#compare The seasons with diversity:
plot(season,diversity,xlab="Season",ylab="Microbial Diversity",main="B",col=c("skyblue1","skyblue4"))

#do a t test to look at the means
t.test(diversity~season) #pvalue (0.004559) below 0.05 = significant
t.test(diversity~lattitude) #pvalue (0.03779) below 0.05 = significant

#create a potential model:
Micro_model<-lm(diversity~season+lattitude)
#check the anova of the model:
anova(Micro_model) #season=0.0009128 and latitude=0.0072116 = significant as below 0.05
#plot the diagnostic plots for the model:
#change the parameters to show the 4 plots at the same time:
par(mfrow=c(2,2))
#now plot the model:
plot(Micro_model)
#does the response variable need adjusting? Think of the assumptions - normally distributed?

#create another model to check for the interactions of the variables:
Micro_interaction_model<-lm(diversity~season*lattitude)
#check the anova for this new model:
anova(Micro_interaction_model) 
#check the P value for the interactions = nonsignificant (0.613087) = no interaction

#The final model is therefore:
Final_Micro_model <- lm(diversity~season+lattitude)