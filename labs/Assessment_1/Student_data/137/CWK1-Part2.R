#Name: Dionysios Grigoriadis
#ID: 170484367
#BIO782P Statistics and Bioinformatics Assignment 2017
#CWK 1

#Dataset2
#Functions defined to perform the required analyses:
p_stars <- function(pval){
  #Function which takes a p-value as its argument and returns a string 
  #of the type "P < a" followed by the corresponding symbol of statistical
  #significance.
  if(pval<=0.001){p_star <- "P < 0.001 ***"}
  else if(pval<=0.01){p_star <- "P < 0.01 **"}
  else if(pval<=0.05){p_star <- "P < 0.05 *"}
  else if(pval>0.05){p_star <- "P > 0.05"}
  return(p_star)
}

#Reading of the data
Brass_data<-read.table("137_part2.tdf", header=TRUE)
str(Brass_data)
attach(Brass_data)  
  
#Question 1

#homologue expression vs. genetic distance plot to visualise
#their posible interaction   
plot(subst., expr_units)

#Linear model is fitted
exp_sub_model<-lm(expr_units~subst.)
#Diagnostic plots. The assumptions are not valid. 
png(filename = "expr_subst.model_assumptions.png")
par(mfrow=c(2,2))
plot(exp_sub_model)
dev.off()

#A new linear model is fitted. expr_units variable is square-root transformed.
exp_sub_model_sqrt<-lm(sqrt(expr_units)~subst.)

#Diagnostic plots for this new model. The assumptions are better.  
png(filename = "expr_sqrt(subst.)modelsubst.model_assumptions_improved.png")
par(mfrow=c(2,2))  
plot(exp_sub_model_sqrt)
dev.off()

# square-root transformed homologue expression vs. genetic distance plot with a fitted
#linear regression - linearity is better
png(filename = "expr_sqrt(subst.)model_regression.png")
par(mfrow=c(1,1)) 
par(mar=c(5, 6, 4, 2) + 0.1) #Extends the plot margins
plot(subst.,sqrt(expr_units), xlab = 'Genetic distance from the report gene\n(Averaged number of amino-acid substitutions)',
     ylab = 'Square-root transformed Luciferase\nhomologue relative expression level')
abline(coef(exp_sub_model_sqrt), col="red") #Regression line - exp_sub_model
dev.off()
  

#Question requires to Assume that the fisrt model is statistically valid.
#ANOVA to check significance for the interaction.
summary(exp_sub_model)
anova(exp_sub_model)
exp_sub_p<-anova(exp_sub_model)$"Pr(>F)" #stores p-value in a variable
exp_sub_p<-exp_sub_p[1]
  
#The new plot with the line regression: homologue expression vs. genetic distance.
png(filename = "expr_subst.model_regression.png")
par(mfrow=c(1,1)) 
par(mar=c(5, 6, 4, 2) + 0.1) #Extends the plot margins
plot(subst.,expr_units, xlab = 'Genetic distance from the report gene\n(Averaged number of amino-acid substitutions)',
     ylab = 'Luciferase homologue\n relative expression level')
abline(coef(exp_sub_model), col="red") #Regression line - exp_sub_model
text(8.7,4.2,p_stars(exp_sub_p),cex=1) #Position of the "P < a ***" on the plot
dev.off()

