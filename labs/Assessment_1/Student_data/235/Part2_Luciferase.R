#Part 2: Pairwise nucleotide substitutions and RNA expression levels

#set the working directory to source file location under Session
#open the file and include headers:
Luciferase <- read.table("235_part2.tdf",header = TRUE)
#check that the data is in the right format:
str(Luciferase)
#change variables to factors and numbers if appropriate
#attach the variables to the data = do not have to constantly specify which data (Luciferase$): 
attach(Luciferase)
#plotting the Luciferase expression against Substitutions:
par(mfrow=c(1,1))
plot(expr_units,subst.,xlab="Luciferase Expression",ylab ="Genetic Distance",pch=21,col="black",bg="skyblue1")
#make a potential model:
Luciferase_model<-lm(subst.~expr_units)
#check the model for assumptions and significance:
t.test(expr_units,subst.) # pvalue (5.278e-06) below 0.05 = significant
anova(Luciferase_model) # pvalue (2.2e-16) below 0.05 = significant
summary(Luciferase_model)
#fit a linear regression:
abline(Luciferase_model,col="red")
#change parameters to view 4 diagnostic plots for the model:
par(mfrow=c(2,2))
#plot the model:
plot(Luciferase_model)
#Residuals vs Fitted plot shows slight curvature so the model needs to be changed

#change model to fit a second order polynomial:
Luciferase_nonlinear_model<-lm(subst.~expr_units+I(expr_units^2))
#now plot the new model again:
plot(Luciferase_nonlinear_model)
#Residual vs Fitted plot now looks better
#check summary to see if this decision is justified:
summary(Luciferase_nonlinear_model)
#if it is justified, a fitted curve can used in your plot:
#change the parameters to view the 1 plot:
par(mfrow=c(1,1))
#plot the original data once again:
plot(expr_units,subst.,xlab="Luciferase Expression",ylab ="Genetic Distance",pch=21,col="black",bg="skyblue1")
#add the curve to the plot based on the summary values(a^2x+bx+c):
curve(-3.059845+0.902431*x-0.012863*x^2,add=TRUE,col="red")

#is the curve better?check the anova of both models:
anova(Luciferase_model,Luciferase_nonlinear_model,test="LRT") # pvalue (0.0065257) below 0.05 = significant

#The final model is therefore:
Final_Luciferase_model <- lm(subst.~expr_units+I(expr_units^2))