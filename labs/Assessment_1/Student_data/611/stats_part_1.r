#RCurl allows for data to be fetched from URIs
library(RCurl)
#Loading required package: bitops
install.packages("bitops", repos="https://cran.ma.imperial.ac.uk/")
#--- Please select a CRAN mirror for use in this session ---
#Selection: 50

#loading data
datafile1= getURL("https://raw.githubusercontent.com/lonelyjoeparker/QMUL-MSc-BIO782P-stats-bioinfo/master/labs/Assessment_1/Student_data/611/611_part1.tdf")
#reading data as a table
part1=read.table(text=datafile1, header=TRUE)
#checking the format of the read table is correct
str(part1)

#attaching
diversity=(part1$diversity)
season=as.factor(part1$season)
lattitude=as.factor(part1$lattitude)


#lattitude anova
boxplot(diversity~lattitude, xlab='lattitude', ylab='microbial diversity')
anovalattitude=aov(diversity~lattitude)
summary(anovalattitude)

#lattitude fitted model
modellattitude=lm(diversity~lattitude)
summary(modellattitude)
#formats view to allow all 4 diagnostic plots to be shown on one page
par(mfrow=c(2,2))
#diagnostic plots
plot(modellattitude)

boxplot(diversity~season, xlab="season", ylab="microbial diversity")
anovaseason=aov(diversity~season)
summary(anovaseason)

#season fitted model
modelseason=lm(diversity~season)
summary(modelseason)
par(mfrow=c(2,2))
plot(modelseason)

#interaction between season and lattitude
interaction.plot(season,lattitude, diversity)
#fitted model for interaction
model1=lm(diversity~season*lattitude)
summary(model1)
par(mfrow=c(2,2))
plot(model1)
