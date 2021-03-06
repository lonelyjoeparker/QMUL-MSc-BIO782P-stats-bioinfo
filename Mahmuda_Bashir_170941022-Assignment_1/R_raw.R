
Question-1
marine_table=read.table("part_1_student_1311.tdf", header=TRUE)
View(marine_table)
hist(UniFracInd)
boxplot(UniFracInd~latitude, col=c("skyblue", "lightgreen"), main="Microbial diversity based on Latitude", xlab="Latitude", ylab="UniFracInd Frequency")
boxplot(UniFracInd~season, col=c("lightpink", "orange"), main="Microbial diversity based on Season", xlab="Time of the Year", ylab="UniFracInd Frequency")
t.test(UniFracInd~latitude)
t.test(UniFracInd~season)
interaction=lm(UniFracInd~latitude*season)
anova(interaction)
luciferase=read.table("part_2_student_1311.tdf", header=TRUE)
View(luciferase)
summary(luciferase)
attach(luciferase)
plot(expression_fold~distance, main="Homologus expression change with Distance", pch=20)
abline(lm(expression_fold~distance), col="red")
par(mfrow=c(2,2))
plot(lm(expression_fold~distance))
virus=read.table("part_3_student_1311.tdf", header = TRUE)
View(virus)
viraldata=cbind(week=(1:40),  virus ) # to add the week 1 to 40.
View(viraldata)
