#Statstics and Bioinformatics Coursework Part 1
#Bryan Mensah
#Student ID:180946806


#Question 1
#Imported dataset 1 and assigned shorter variable names to the variables
marine <-read.table("~/Datasets/part_1_student_1864.tdf.txt", header = T) #read the file. 
Diversity <- marine$UniFracInd #unifrac renamed as diversity
Season <- marine$season
Latitude <- marine$latitude

#Part 1 
hist(Diversity) # created histogram to check data was normally distributed before doing any analysis

boxplot(Diversity ~ Latitude, xlab ="Latitude", ylab ="Diversity (UniFrac)") #created boxplot of latitude and diversity

model1 <-lm(Diversity ~ Latitude) #fitted a linear model of  diversity and latitude
anova(model1) # anova analysis of the fitted model 1

#Part 2

boxplot(Diversity ~ Season , xlab ="Season",ylab ="Diversity (UniFrac)")#created boxplot of latitude and season


model2 <-lm(Diversity ~ Season) #fitted a linear model of diversity and season
anova(model2) #anova analysis of the fitted model 2

#Part 3
interaction.plot(Season,Latitude,Diversity , ylab="Mean of Diversity (UniFrac)", xlab="Season") # Interaction plot of the Latitude,season and Diversity


#converted the variables from factors to numeric
Season <-as.numeric(marine$season)
Latitude <-as.numeric(marine$latitude)

# now they are numeric need to check if the data is normally distributed before doing any analysis
hist(Season) # created histogram to check data was normally distributed

hist(Latitude) # created histogram to check data was normally distributed

model3 <-lm(Latitude ~ Season) #fitted a linear model of latitude and season
anova(model3) #anova analysis of the fitted model 3

#Question 2

#imported dataset 2 and assigned shorter variable names to the variables
protein <-read.table("~/Datasets/part_2_student_1864.tdf.txt", header = T)
Expression <- protein$expression_fold
Distance <- protein$distance

#Part 1

plot(Expression, Distance, xlab="Expression (Log 2 Fold)", ylab="Genetic Distance")#scatter plot of the Expression and Genetic Distance
abline(lm(Distance ~Expression))#created and applied linear regression line (line of best fit) to our scatter plot of expression and genetic distance


cor.test(Expression, Distance, method = "pearson") #Performed the Pearson correlation coefficient test on Expression and Distance

model4 <- lm(Expression ~ Distance) #fitted a linear model of Expression and Distance
anova(model4) #anova analysis of the fitted model 4


# Part 2 
par(mfrow=c(2,2)) #creates a 2x2 matrix
plot(model4) # Diagnostic plots of model 4 
par(mfrow=c(1,1)) #return everything to normal (makes 1x1 matrix)


#Part 3 in report

#Question 3
#imported the dataset and assigned shorter variable names to the variables
Hiv <- read.table("~/Datasets/part_3_student_1864.tdf.txt" , header = T)
Viral <- Hiv$VLoad
Shannon <-Hiv$score_shannon
HDistance <-Hiv$score_distance
Tissue <- Hiv$tissue
CD4 <- Hiv$CD4

Backward <- step(lm(Viral ~ Tissue*CD4*Shannon*HDistance), direction= "backward")#backward stepwise selection model


Forward <- step(lm(Viral ~ Tissue*CD4), scope=(~ CD4*Tissue*Shannon*HDistance), direction="forward")#forward stepwise selection model


Both<- step(lm(Viral ~ Tissue*CD4), scope=c(lower=~CD4 , upper=~Tissue*CD4*Shannon*HDistance), direction=c("both"))#bidirectional stepwise selection model


model5 <-anova(Backward,Forward,Both) #fitted a linear model of the forward,backward and both model
(model5) # anova analysis of fitted model 5

plot(Viral~Shannon,xlab="Viral load(log10(number of viral particles per ml))", ylab="Shannon Diversity(H)") # produces scatter plot of viral population size and Shannon Diversity
plot(Viral~HDistance,xlab="Viral load (log10(number of viral particles per ml))", ylab="Genetic Distance") # produces scatter plot of viral population size and Genetic Distance 

model6 <- lm(Viral ~ CD4 + Shannon + HDistance) #fitted a linear model of the selected model 
anova(model6) #anova analysis of fitted model 6

