data1= read.table("/Users/vithusaaselva/Desktop/896_part1.tdf.txt", header = TRUE) #reads file data
str(data1) # view one line structure in object is displayed
hist(data1$diversity, col = "dark green", xlab= "Diversity") #check distribution for diversity
boxplot(data1$diversity~data1$lattitude, col= "pink", ylab= "Diversity") #boxplot for diversity & latitude
boxplot(data1$diversity~data1$season, col= "dark green", ylab= "Diversity") #boxplot for diversity & season
qf(0.95,9,9) #tabulated F value
t.test(data1$diversity~ data1$lattitude, var.equal=TRUE) #T-test to see difference in means for div and lat
qf(0.95,9,9) #tabulated F value
t.test(data1$diversity~data1$season, var.equal  = TRUE) #T-test to see difference in means for div and season
lslm <- lm(data1$diversity ~ data1$lattitude+data1$season+(data1$lattitude:data1$season)) #model fitted for interaction
lslm
summary(lslm) #summary results for interaction model between latitude and season


