#RCurl allows for data to be fetched from URIs
library(RCurl)
#Loading required package: bitops
install.packages('bitops', repos='https://cran.ma.imperial.ac.uk/')
#for ggplots data visualisation
install.packages('ggpubr', repos='https://cran.ma.imperial.ac.uk/')
#--- Please select a CRAN mirror for use in this session ---
#Selection: 50

#loading data
datafile2=getURL('https://raw.githubusercontent.com/lonelyjoeparker/QMUL-MSc-BIO782P-stats-bioinfo/master/labs/Assessment_1/Student_data/611/611_part2.tdf')
#reading data as a table
part2=read.table(text=datafile2, header=TRUE)
#checking the format of the read table is correct
str(part2)

#attaching
expression=part2$expr_units
substitution=part2$subst.

#How does the putative 'luciferase' homologue expression change with genetic distance (amino acid substitutions)?
equation=lm(expression~substitution,data=part2)
#formats view to allow all 4 diagnostic plots to be shown on one page
par(mfrow=c(2,2))
#plot diagnostic plots
plot(equation)
#residuals vs fitted plot showed heteroscedascity, therefore fit another model with log transformation to expression


logexpr=log2(expression)
equation2=lm(logexpr~substitution)
par(mfrow=c(2,2))
plot(equation2)
#residual vs fitted plot showed less heteroscedascity

#coefficients equation
equ_coeff=coefficients(equation2)
equ_coeff
#equation of the line: y=0.23x +2.2
#plotting linear graph
plot(logexpr~substitution, xlab='number of aa substitutions', ylab='log expressionof luciferase', main='relationship between luciferase expression and genetic distance')
#adding the coefficient line
abline(equ_coeff, col='green')

#Comment on whether the model assumptions are valid.

#Shapiro-Wilk normality test

shapiro.test(substitution)
shapiro.test(logexpr)


#qqplots
library('ggpubr')
ggqqplot(substitution, ylab='substitution')
ggqqplot(logexpr, ylab='log expression')
#Correlation test between substitution and expression
#pearsons product moment coefficient- for parametric data

ppmc=cor.test(logexpr,substitution, method='pearson')
ppmc
#p value
ppmc$p.value
#correlation coefficient
ppmc$estimate
