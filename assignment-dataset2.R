part2<-read.table("/home/bt17186/Desktop/61_part2.tdf.txt", header=TRUE)
str(part2)
plot(part2$expr_units~part2$subst. , col="brown", xlab="substitution", ylab="expression", main="Pairwise nucleotide substitutions and RNA expression levels") #scatterplot- two continuous data
abline(3.40,1.65,col="purple")#line of best fit
lm(part2$expr_units~part2$subst.) #model
summary(lm(part2$expr_units~part2$subst.))
plot(lm(part2$expr_units~part2$subst.)) # assumptions for the model
#install.packages("gvlma")
#library(gvlma)
gvlma(lm(part2$expr_units~part2$subst.)) # assumptions shown quantitatively