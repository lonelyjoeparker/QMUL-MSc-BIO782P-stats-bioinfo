data2= read.table("/Users/vithusaaselva/Desktop/896_part2.tdf.txt", header = TRUE) #reads dataset 2
str(data2) #view one line structure in object is displayed
plot(data2$expr_units ~ data2$subst., xlab= "amino acid substituitions", ylab= "homologue expression", col= "blue") #scatterplot generated
mod1= lm(data2$expr_units~data2$subst.) #linear model for expression & substituion
summary(mod1) #summary table for model
abline(3.049, 1.631) #line of best fit (from intercept in summary) for scatterplot
plot(mod1) #interaction plots for model

