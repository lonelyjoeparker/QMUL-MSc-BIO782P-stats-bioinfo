rm(list = ls())

lucifer <- read.table("897_part2.tdf.txt", header = TRUE)
attach(lucifer)

#checking variables and seeing top few lines of table
str(lucifer) 
head(lucifer)

plot(lucifer)

#correlation test
cor.test(lucifer$expr_units,lucifer$subst.)

#linear regression model
model2 <- lm(expr_units~subst.)
summary(model2)
