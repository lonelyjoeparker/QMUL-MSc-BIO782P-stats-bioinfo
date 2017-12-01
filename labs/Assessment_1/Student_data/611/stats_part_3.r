#RCurl allows for data to be fetched from URIs
library(RCurl)
#Loading required package: bitops
install.packages("bitops", repos="https://cran.ma.imperial.ac.uk/")
#--- Please select a CRAN mirror for use in this session ---
#Selection: 50

#loading data

#loading data
datafile3= getURL("https://raw.githubusercontent.com/lonelyjoeparker/QMUL-MSc-BIO782P-stats-bioinfo/master/labs/Assessment_1/Student_data/611/611_part3.tdf")
#reading data as a table
part3=read.table(text=datafile3, header=TRUE)
#checking the format of the read table is correct
str(part3)

#attaching
load=part3$viral_load
cd4=as.factor(part3$CD4)
shannon=part3$Shannon_diversity
tissue=as.factor(part3$Tissue)
distance=part3$Distance

#matrix scatterplot
ndata=part3[,c(1:5)]
summary(ndata)
plot(ndata,pch=16,col='green',main='matrix scatterplot of HIV variables')


#viral load= response variable

#cd4
boxplot(load~cd4,xlab='cd4')

# cd4 model
cd4m=lm(load~cd4)
summary(cd4m)
#formats view to allow all 4 diagnostic plots to be shown on one page
par(mfrow=c(2,2))
#plots diagnostic plots
plot(cd4m)


#tissue
boxplot(load~tissue, xlab='tissue')

#tissue model
tissuem=lm(load~tissue)
summary(tissuem)
par(mfrow=c(2,2))
plot(tissuem)


#shannon model
shannonm=lm(load~shannon)
summary(shannonm)
par(mfrow=c(2,2))
plot(shannonm)
#residuals vs fitted shows heteroscedascity, therefore square root Shannon diversity

#square root shannon model. Abs= absolute to remove -ve values when square rooting
sqrt_shannon=sqrt (abs(shannon+1))
sqrtsh_model=lm(load~sqrt_shannon)
summary(sqrtsh_model)
par(mfrow=c(2,2))
plot(sqrtsh_model)

#distance model
distancem=lm(load~distance)
summary(distancem)
par(mfrow=c(2,2))
plot(distancem)
#q-q plot shows curve, therefore log transform distance

#log distance model. Abs= absolute to remove -ve values when log transforming
logdistance=sign(distance)*log((abs(distance))+1)
model_logdis=lm(load~logdistance)
summary(model_logdis)
par(mfrow=c(2,2))
plot(model_logdis)


#forwards stepwise regression model
forwards1=step(lm(load~logdistance),scope=(~logdistance*sqrt_shannon*cd4*tissue),direction='forward')
par(mfrow=c(2,2))
plot(forwards1)


#termplot for forwards1 model
par(mfrow=c(2,2))
termplot(forwards1, partial.resid=TRUE, col.res = 'purple', smooth=panel.smooth)
