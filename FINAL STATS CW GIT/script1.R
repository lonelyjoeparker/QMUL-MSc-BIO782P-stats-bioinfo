
#set working directory to the same directory the data is in 

#"read.table" to make a data frame from dataset in table format and "header=T" so the first lines are read as variable names. Set name as "marine"
marine=read.table("part_1_student_1725.tdf.txt", header = T)

#see details about variables in data frame and check data has been read in properly
str(marine)
head(marine)

#name variables 
UniFracInd=marine$UniFracInd
season=marine$season
latitude=marine$latitude

#T-test assumption 1: data is normally distributed

#check for normality of data by first visualising histogram
hist(UniFracInd, main="Diversity Histogram")
#data looks normally distributed (bell shaped histogram)

#boxplot to check for normality 
boxplot(UniFracInd, main="Diversity Boxplot") 
#looks symmetrical

#Shapiro-Wilk test for normality 
shapiro.test(UniFracInd) #p=0.3152 (>0.05 so fail to reject null hypothesis that the sample comes from a population with a normal distribution.)

#we can conclude that data is normally distributed

#T-test assumption 2: unequal variance (unless explicitly stated), so check to see if season and latitude variances are unequal. 

tapply(UniFracInd, season,var)
tapply(UniFracInd,latitude,var)
#variances are diferent between season and latitude so no need for "var.equal=T"


#season means 
season_means=tapply(UniFracInd, season, mean)
season_means
#season sd
tapply(UniFracInd, season, sd)


#latitude means 
latitude_means=tapply(UniFracInd, latitude,mean)
latitude_means
#latitude sd
tapply(UniFracInd,latitude, sd)


#boxplot (DIVERSITY ~ SEASON) to visualise how diversity changes with season
seasonbp=plot(UniFracInd ~ season, xlab="Time of Year", ylab="Relative microbial Diversity", col="cadetblue3")
points(season_means,pch=16)
#plot on means to boxplots as a solid dot


#boxplot (DIVERSITY ~ LATITUDE) to visualise how diversity changes with latitude
plot(UniFracInd ~ latitude, xlab="Latitude", ylab="Relative microbial Diversity", col="deepskyblue4")
points(latitude_means,pch=16)


#t-test to see if diversity means between the different latitudes are statistically significantly different
t.test(UniFracInd ~ latitude) 
#0.2927 - latitude means not statistically different

#t-test to see if diversity means between the two months are statistically significantly different
t.test(UniFracInd ~ season) 
#0.9531 - season means not statistically different


# Interaction between season and latitude?

#COMPARE BOXPLOTS 
boxplot(UniFracInd~interaction(season,latitude), col=c("steelblue", "darkgreen"),ylab="Diversity")

#INTERACTION PLOT
interaction.plot(season, latitude, UniFracInd, ylab="Microbial diversity", xlab="Season")

#lines are not parallel,thus indicative of some interaciton, BUT is this interaction statistically significant?

#use multi-factor anova to see if there is any interaction between season and latitude

#build model
marine_model1=lm(UniFracInd~ season*latitude)
#include main effects of season and latitude in the model as well as their interaction

anova(marine_model1)
#p value(0.56) -interaction effect is not statistically significant

#SUMMARY OF MODEL
summary(marine_model1) # to see more information about the model 

#space for 4 graphs
par(mfrow=c(2,2))
#Diagnostic plots to check if model assumption are valid
plot(marine_model1)
#diagnostic plots all look ok 
#ends of Q-Q plot deviate from the straight line but not too much

#NON significant result for interaction between season and location
  