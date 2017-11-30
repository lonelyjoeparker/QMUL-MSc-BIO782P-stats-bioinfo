#Reads the file into a variable
micro_diversity<- read.table("75_part1.tdf", header=TRUE)

#useful to check whether the dataframe contains correct data types (factors/num)
str(micro_diversity) 
attach(micro_diversity)

#Reset graphics device to show 1 plot
par(mfrow=c(1,1))
plot(lattitude, diversity, xlab = "Latitude", ylab="Microbial Diversity", main = "Change in Microbial Diversity with latitude")


#linear model function creates a linear regression by using the independent variable to predict the dependent variable
model1<- lm(diversity~lattitude) 
#Displays anova table, showing P-val and Df
anova(model1) 

par(mfrow=c(2,2))
#plotting 4 diagnostic graphs to check the regression for inconsistencies / issues.
plot(model1)


par(mfrow=c(1,1))
plot(season, diversity)
model2<- lm(diversity~season)
#Displays anova table, showing P-val and Df. No significance was found.
anova(model2)

#Alternative way to do t-test for season:
#jan<--diversity[season=="Jan"]
#aug<--diversity[season=="Aug"]
#t.test(jan, aug)


#This lm function has added variables, notable the * between season and lattitude indicates to check for interaction between those variables
model3<-lm(diversity~season*lattitude)

#No significant interaction between season and location, Pval:0.75
anova(model3)


