rm(list = ls())

hiv <- read.table("897_part3.tdf.txt", header = TRUE)
attach(hiv)

#checking variables and seeing top few lines of table
str(hiv)
head(hiv)

#examine factors affecting viral load by estimating linear regression model
#and using stepwise regression method to obtain best fit for
#dependent variable
#load leaps
library(leaps)

#perform stepwise regression
stepreg <- regsubsets(viral_load~CD4+Shannon_diversity+Tissue+Distance, data = hiv, nbest = 10)

#plot stepwise regression to estimate ranked models
#plot according to R-squared criteria
plot(stepreg, scale = "adjr2")

#plot according to BIC
plot(stepreg, scale = "bic")
