#set wd to "source file location"
HIV=read.table("part_3_student_1725.tdf.txt", header=T) 
#HIV$week=1:40
#Initially, added variable "Week" to represent the week when the sample was collected,but no effect on viral load".

#check that the data has been imported correctly and all variables are of the correct type. 
str(HIV)
head(HIV)
tail(HIV)
names(HIV)



#assigning names for variables
VLoad=HIV$VLoad
CD4=HIV$CD4
tissue=HIV$tissue
score_shannon=HIV$score_shannon
score_distance=HIV$score_distance



hist(VLoad, main="Viral Load Histogram") #response variable looks normally distributed
#histogram shows the response variable follows a normal distribution
shapiro.test(VLoad)
#p>0.05 - fail to reject null hypothesis that the data is normally distributed 



#visualize pairwise plots for any obvious interactions
plot(HIV) 

#Plot response variable against each explanitory variable to visualize all relationships with viral load individually/ most relevant relationships.

par(mfrow=c(2,2))
plot(VLoad~CD4, main="Viral load v CD4") # no obvious relationship 
plot(VLoad~tissue, main="Viral load v tissue")# no obvious relationship 
plot(VLoad~score_shannon, main="Viral load v Shannon diversity")# no obvious relationship 
plot(VLoad~score_distance, main="Viral load v Distance")#hint of curvature. This will be explaored in the model. 
par(mfrow=c(1,1))


t.test(VLoad[CD4=="hi"], VLoad[CD4=="lo"])
sd(VLoad[CD4=="lo"])
sd(VLoad[CD4=="hi"])
#no significant difference in means between high and low CD4 levels 
#hi mean=-0.0352538 ,lo mean=0.3153000

t.test(VLoad[tissue=="brain"], VLoad[tissue=="spinalCord"])
sd(VLoad[tissue=="brain"])
sd(VLoad[tissue=="spinalCord"])
#no significant difference in means between tissue types 
#brain mean =0.19769489 spinalCord mean=0.08235135 

cor.test(score_shannon, VLoad)
cor.test(score_distance, VLoad) 

  
#MANUAL MODELLING

#How Vload is affected by all variables + their interaction
hivmod1=lm(VLoad~ score_distance*tissue*CD4*score_shannon) 
anova(hivmod1)#only two interactions significant and they're too complex.
summary(hivmod1)
drop1(hivmod1, test="F")


#How Vload is affected by all variables, no interaction between them
hivmod2=lm(VLoad~ tissue+score_distance+score_shannon+CD4)
anova(hivmod2)#no individual significant terms 
summary(hivmod2)
drop1(hivmod2, test="F")


#Try squaring distance? Looks like it had a curved relationship

#Linear model of distance and viral load
dist_mod1=lm(VLoad~score_distance)
par(mfrow=c(2,2))
plot(dist_mod1)#Residuals v Fitted model also shows some hint of curvature
par(mfrow=c(1,1))
anova(dist_mod1)# non significant


#SQUARED MODEL

squared_model=lm(VLoad~score_distance+I(score_distance^2))#fitting a curve with a polynomial term (still qualifies as a linear model as "x" is being squared)

summary(squared_model)#r sqaured shows the mod only makes up for 21.5% of variation,the rest by residuals

anova(squared_model) # polynomial term shows significance. 

par(mfrow=c(2,2))
plot(squared_model) # assumptions met
par(mfrow=c(1,1))

hist(residuals(squared_model)) #bell shaped (normally distributed residuals)

shapiro.test(residuals(squared_model)) # residuals are normal, p>0.05



#CURVE FOR SQUARED MODEL

plot(VLoad~score_distance, pch=16,col="red", ylab="log10 Number of Viral Particles per ml", xlab = "Evolutionary distance from reference sequence")
curve(-0.6110 + 3.4423* x -1.9671*x^2  , add=T, col="blue")



#AUTOMATED MODELLING (STEPWISE AIC)

#BACKWARD

#maximal model(all explanatory variables interacting)
backwards2=step(lm(VLoad~score_distance*score_shannon*tissue*CD4), direction = "backward")
#no interactions
backwards=step(lm(VLoad~score_distance+score_shannon+tissue+CD4), direction = "backward")
#Step:  AIC=35.53
#VLoad ~ score_distance

#FORWARD
forwards=step(lm(VLoad~ score_distance),scope=(~score_distance*score_shannon*tissue*CD4), direction = "forward")
#Start:  AIC=35.53
#VLoad ~ score_distance

#BIDIRECTIONAL
#VLoad~score_distance was chosen as the lower model and all variables and their interactions as the upper models
bothways=step(lm(VLoad~score_distance+tissue+score_distance+score_shannon+CD4), scope = c(lower = VLoad~score_distance, upper = VLoad~ tissue*score_distance*score_shannon*CD4), direction="both")
#Step:  AIC=35.53
#VLoad ~ score_distance

