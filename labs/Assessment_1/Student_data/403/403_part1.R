#Open the Table:
marine=read.table(file="403_part1.tdf",header = TRUE)
#Check the variables:
str(marine)
attach(marine)

#Plot of all the variables:
plot(marine)

#Histogram of the Response Variable (Diversity):
hist(diversity, main="Histogram of Diversity",xlab="Diversity")
mean(diversity)
sd(diversity)

ks.test(diversity,"pnorm",mean(diversity),sd(diversity))
#Using the Kolmogorov-Smirnov test it was accepted the Ho: Diversity follows a normal distribution
#With this test and the histogram it was accepted that diversity follows a normal distribution.

#Plot of the Response Variable against the Explanatory Variables (Season and Lattitude):
boxplot(diversity~season, main="Diversity vs Season",xlab="Season",ylab="Diversity")
boxplot(diversity~lattitude, main="Diversity vs Lattitude",xlab="Lattitude",ylab="Diversity")

#Building the Models:

#MODEL 1:
model1=lm(diversity~season*lattitude)
par(mfrow=c(2,2))
plot(model1)
anova(model1)
summary(model1)
drop1(model1,test = "F")
#The anova, summary and drop1 analysis show that the interaction between season:lattitude is not a good
#element to explain the Response variable. 

#MODEL 2:
model2=lm(diversity~season+lattitude)
par(mfrow=c(2,2))
plot(model2)
anova(model2)
summary(model2)
drop1(model2,test = "F")
#The second model is best than the first one.

#Using the Automated model selection of R indicates the same: Model 2 is better than 1.
backwards_final=step(lm(diversity ~ season*lattitude),direction="backward")

#MODEL 3: 
#Refit Model 2
#In an attemp to improve the homoscedasticity and linearity of Model 2, try to transform
#the Response variable.
logdiversity=log(diversity+4)
model3=lm(logdiversity~season+lattitude)
par(mfrow=c(2,2))
plot(model3)
anova(model3)
summary(model3)
#The model 3 is not good. In fact, seeing the anova analysis, the results are worse. 
#Decide to stay with the second model. 

#With an Anova Analysis compares the models:
anova(model1,model2)
#After making the anova comparison, the result indicates that the model with the interaction
#whitin season:lattitude is better. This contradicts the previous results. For that reason,
#make the model 4 and 5 to see if the position of the variables affects the model.

model4=lm(diversity~lattitude*season)
par(mfrow=c(2,2))
plot(model4)
anova(model4)
summary(model4)
anova(model1,model4)

#There is a sligthly improvement between model 1 and model 4. 
model5=lm(diversity~lattitude+season)
par(mfrow=c(2,2))
plot(model5)
anova(model5)
summary(model5)
anova(model2,model5)
#There is a sligthly improvement between model 2 and model 5. 

#Because previously it was observed that there is not an interaction bewteen season:lattitude,
#we know that the best model is the simplest one and the difference between Model 2
#and Model 5 are not significant, the model 2 is selected as the model that explains better
#the data. 

#Create the boxplot for Season and Latitude:
par(mfrow=c(1,2))

#Boxplot Diversity vs Season:
boxplot(diversity~season, main="Diversity vs Season",xlab="Season",ylab="Diversity", ylim=c(-3.5, 9))
#Draw lines:
lines(x[1:2],c(8.5, 8.5))
lines(x[c(1,1)],c(8.5, 8.3))
lines(x[c(2,2)],c(8.5,8.3))
#Draw asterisk:
text(x[1]+((x[2]-x[1])/2),8.9,"*")

#Boxplot Diversity vs Lattitude:
boxplot(diversity~lattitude, main="Diversity vs Latitude",xlab="Latitude",ylab="Diversity", ylim=c(-3.5, 9))
#Draw lines:
lines(x[1:2],c(8.5, 8.5))
lines(x[c(1,1)],c(8.5, 8.3))
lines(x[c(2,2)],c(8.5,8.3))
#Draw asterisk:
text(x[1]+((x[2]-x[1])/2),8.9,"***")

