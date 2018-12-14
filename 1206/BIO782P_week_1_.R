#Dataset 1
#read file
  microbial=read.table("part_1_student_1206.tdf.txt", header = TRUE)

#1. How does microbial diversity change with latitude?
#make a boxplot to .....
  boxplot(UniFracInd~latitude,data=microbial, main="Microbial diversity change with latitude", ylab="microbial diversity",xlab="latitude",col="pink")
#t-test
  t.test(UniFracInd~latitude,data=microbial)

#2. How does microbial diversity change with time of year?
#make a boxplot to .....
  boxplot(UniFracInd~season, data=microbial, main="Microbial diversity change with time of year", ylab="microbial diversity",xlab="time of year",col="cyan2")
#t-test
  t.test(UniFracInd~season,data=microbial)

#3. Is there an interaction between the season, and location?
#fit the data
  microbial_model=lm(UniFracInd~season*latitude,data=microbial)
  par(mfrow=c(2,2))
  plot(microbial_model)
  anova(microbial_model)
  par(mfrow=c(1,1))
  
#Dataset 2
#read data
luciferase=read.table("part_2_student_1206.tdf.txt", header = TRUE)
#check the data there 2 variables called "expression_fold" which is ?luciferase? homologue expression change, and "distance" which is the averaged numer of amino acid substitutions
str(luciferase)

#1. How does the putative ?luciferase? homologue expression change with genetic distance (amino acid substitutions)?
# plot a scatter plot to show how the 2 variables related each other
  plot(luciferase$distance,luciferase$expression_fold,xlab = "distance",ylab="expression fold",col="darkturquoise",pch=19)

#make a model:the expression is a dependent variable and genetic distance is an independent variable
  luciferase_model=lm(expression_fold~distance,data=luciferase)

#add the fitted line (model) to the scatter plot
  abline(luciferase_model,col="brown1")

#2. Comment on whether the model assumptions are valid.
#build the regression diagnostic plots
par(mfrow=c(2,2))
plot(luciferase_model)
par(mfrow=c(1,1))
  
#dataset3
#read input file
HIV=read.table("part_3_student_1206.tdf.txt", header = TRUE)

HIV_model1=lm(VLoad~CD4*tissue*score_shannon*score_distance,data=HIV)
anova(HIV_model1)

#delete score_shannon
HIV_model2=lm(VLoad~CD4:tissue*CD4:score_shannon:score_distance *score_distance,data=HIV)
anova(HIV_model2)

#delete tissue,CD4:score_shannon:score_distance because of no significance 
HIV_model3=lm(VLoad~score_distance,data=HIV)
anova(HIV_model3)

#crate a diagnotics plot
par(mfrow=c(2,2))
plot(HIV_model3)
par(mfrow=c(1,1))

#plot graph between the viral load and the genetic distance
plot(HIV$score_distance,HIV$VLoad,xlab = "score distance",ylab="Viral load",main="",col="brown4",pch=20)
#draw model line
abline(HIV_model3,col="cyan4")

#calculate the intercept
summary(HIV_model3)


