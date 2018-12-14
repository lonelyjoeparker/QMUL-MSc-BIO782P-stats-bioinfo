###Week 1 Assessment

#Read and attach the Dataset1 tdf file into R
tableDiv=read.table("part_1_student_1204.tdf.txt",header=T)
attach(tableDiv)

#Check whether the data have been read in properly and are they in proper format
head(tableDiv)
str(tableDiv)

#clean the data
Diversity=as.numeric(tableDiv$UniFracInd)

#First do some exploratory data analysis(EDA)
#Use boxplot to vasualize the relationships since we want to know how two factors(season and latitude) affact one numeric variable(diversity)
plot(latitude, Diversity, pch=16, col="steelblue",
     main= "Levels of Marine Microbial Diversity in different Latitudes", xlab="Latitude", ylab="Marine Microbial Diversity")
plot(season, Diversity, pch=16, col="orange",
     main= "Levels of Marine Microbial Diversity in different Seasons", xlab="Season", ylab="Marine Microbial Diversity")

summary(Diversity,latitude)

#1.check the normality of data
#note:It is very unlikely that a histogram of sample data will produce a perfectly smooth normal curve especially if the sample size is small. As long as the data is approximately normally distributed, with a peak in the middle and fairly symmetrical, a parametric test can be used. 
hist(Diversity)
CheckTrop=Diversity[latitude=="tropical"]
hist(CheckTrop)
# can be treat as normally distributed

#2.check the equality of variance of Diversity by season and by altitude
bartlett.test(Diversity~latitude)
bartlett.test(Diversity~season)
#results: both not equal， so we cannot use one-way ANOVA 

#3.Use Welch's t-test(Welch's ANOVA) to test the relationship between diversity and season, or between diversity and latitude
#note: Welch's t-test don't require data to have equal variance 
###Question 1.1 How does microbial diversity change with latitude?
modelDivSeas=t.test(Diversity~season)
modelDivSeas
###Question 1.2 How does microbial diversity change with time of year?
modelDivLat=t.test(Diversity~latitude)
modelDivLat

#Also, alternative way may be using Kruskal Wallis Test one-way Anova by Ranks for Question 1.1 and Question 1.2
#Note: it is a nonparametric used when diversity is numeric, season adn latitude are factors
#Note: Non-parametric tests are more conservative, which means it is less possible to find out the differences between variables using this method so not recommended
###Question 1.1 Alternative way
modelDivSeas=kruskal.test(Diversity~latitude)
modelDivSeas
###Question 1.2 Alternative way
modelDivLat=kruskal.test(Diversity~season)
modelDivLat


#Question 1.3 Is there an interaction between the season, and location?
#fit a model
modelInter=lm(Diversity~latitude*season)
#check the significance of the interaction terms
drop1(modelInter,test="F")
#check diagnostic plots
par(mfrow=c(2,2))
plot(modelInter)
#back to normal plotting
par(mfrow=c(1,1))

#discover the nature of interaction among the three variables
interaction.plot(season,latitude,Diversity)
#result shows there is no interaction between season and latitude, since we see two nearly parallel lines



