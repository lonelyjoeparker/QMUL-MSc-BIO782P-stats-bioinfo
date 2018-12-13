### dataset1 import file
setwd("~/Desktop/R/coursework")
dataset1 <- read.table("part_1_student_1115.tdf",header=TRUE)   # get data and ignore the first line 
str(dataset1) 
attach(dataset1)
# How does microbial diversity change with latitude?
plot(latitude,UniFracInd,xlab="Latitude",ylab="Microbial diversity index",main=" Microbial diversity change with latitude")
# How does microbial diversity change with time of year?
plot(season,UniFracInd,xlab="Season",ylab="Microbial diversity index",main="Microbial diversity change with season")
# Is there an interaction between the season, and location?
interaction.plot(latitude,season,UniFracInd)
relationship <- lm(UniFracInd~season*latitude)
summary(relationship)
anova(relationship)
drop1(relationship,test = 'F')


### dataset2  import file
setwd("~/Desktop/R/coursework")
dataset2 <- read.table("part_2_student_1115.tdf",header=TRUE)
str(dataset2)
attach(dataset2)
plot(distance,expression_fold,xlab="Genetic distance",ylab="Homologue expression",main="The putative 'luciferase' homologue expression change with genetic distance")
abline(0.25,1,col="red")      # try to find liner regression, intercept=0.25, slope=1,colour is red
abline(1,1,col="green")       # try to find liner regression, intercept=1, slope=1,colour is green
abline(1.25,1,col="blue")     # try to find liner regression, intercept=1.25, slope=1,colour is blue
abline(1.15,1,col="brown")    # try to find liner regression, intercept=1.15, slope=1,colour is brown
plot(distance,expression_fold,xlab="Genetic distance",ylab="Homologue expression",main="The putative 'luciferase' homologue expression change with genetic distance")
abline(1.15,1,col="brown") 
# make a liner model
model1 <- lm(expression_fold~distance)   
summary(model1)
anova(model1)
par(mfrow=c(2,2))
plot(model1)
# make a liner model, use log10 to xxxx
model2 <- lm(log10(expression_fold)~distance)
summary(model2)
anova(model2)
plot(model2)
# make a liner model, use sqrt function
model3 <- lm(sqrt(expression_fold)~distance)
summary(model3)
anova(model3)
plot(model3)


### dataset3  import file
setwd("~/Desktop/R/coursework")
dataset3 <- read.table("part_3_student_1115.tdf",header=TRUE)
str(dataset3)
attach(dataset3)
par(mfrow=c(1,1))
# to see each treatment
boxplot(VLoad~CD4,xlab="CD4 level",ylab="HIV viral load",main="HIV viral load change with CD4 level")
boxplot(VLoad~tissue,xlab="tissue type",ylab="HIV viral load",main="HIV viral load change with tissue type")
plot(score_shannon,VLoad,xlab="score_shannon",ylab="HIV viral load",main="HIV viral load change with score_shannon")
plot(score_distance,VLoad,xlab="score_distance",ylab="HIV viral load",main="HIV viral load change with score_distance")
# make a normal liner model
my_model <- lm(VLoad~CD4*tissue*score_shannon*score_distance)
anova(my_model)
# choose the group which has the lowest virance
my_model_final <- lm(VLoad~score_shannon)
# backwards
backward_final <- step(lm(VLoad~CD4*tissue*score_shannon*score_distance),direction="backward")
# forwards
forward_final <- step(lm(VLoad~CD4*tissue),scope=(~CD4*tissue*score_shannon*score_distance),direction="forward")
anova(my_model_final,forward_final)
final <- step(lm(VLoad~CD4*tissue),scope=c(lower=~tissue,upper=~CD4*tissue*score_shannon*score_distance,direction="forward"))
anova(forward_final,backward_final,final)
