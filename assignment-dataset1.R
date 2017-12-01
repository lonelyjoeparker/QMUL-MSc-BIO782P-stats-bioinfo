part1<-read.table("/home/bt17186/Desktop/61_part1.tdf.txt", header=TRUE)
str(part1)
plot(part1$diversity~part1$lattitude, col="red", xlab="lattitude", ylab="diversity", main="Diversity vs lattitude") 
plot(part1$diversity~part1$season, col="pink", xlab="season", ylab="diversity", main="Diversity vs Time of year")
x= part1$lattitude
y=part1$diversity
t.test(y~x) #t.test to find the difference between the means lattitude and diversity
x1= part1$season #independent=x1=season changes irrespective of the diversity
y2=part1$diversity #dependent=y2=diversity is dependent on season
t.test(y2~x1) #t.test to find the difference between the means of seasons and diversity
lm(part1$diversity~part1$season+part1$lattitude + part1$season*part1$lattitude) #interaction
plot(lm(part1$diversity~part1$season+part1$lattitude + part1$season*part1$lattitude))