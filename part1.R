marinediversity <- read.table("909_part1.tdf", header = T)
str(marinediversity) #make sure it's all good.

shapiro.test(marinediversity$diversity[marinediversity$lattitude == "temperate"]) #check if the diversity for "temperate" is normally distributed.
shapiro.test(marinediversity$diversity[marinediversity$lattitude == "equatorial"]) #check if the diversity for "equatorial" is normally distributed.
shapiro.test(marinediversity$diversity[marinediversity$season == "Jan"]) #check if the diversity for "Jan" is normally distributed.
shapiro.test(marinediversity$diversity[marinediversity$season == "Aug"]) #check if the diversity for "Aug" is normally distributed.
#the data is normally distributed if the p value is >0.05

boxplot(marinediversity$diversity~marinediversity$lattitude) #boxplot shows everything is different when comparing diversity at equatorial and temperate latitudes.
tapply(marinediversity$diversity, marinediversity$lattitude, var) #check variances are the same.

boxplot(marinediversity$diversity~marinediversity$season) #boxplot shows everything is different when comparing diversity at time of year.
tapply(marinediversity$diversity, marinediversity$season, var) #check variances are the same.

diversitymodel <- lm(marinediversity$diversity~marinediversity$lattitude*marinediversity$season) #make a model.
plot(diversitymodel) #plot the model.  

anova(diversitymodel) #check for significant differences. 

drop1(diversitymodel, test="F") #test the significance of the interaction, p-value 0.6059 so not significant. AIC is 16.951 and 15.294 so it's within 2 so can't distinguish a difference. 

interaction.plot(marinediversity$season, marinediversity$lattitude, marinediversity$diversity, ylab = "Diversity", xlab = "Season", legend = T) #make an interaction plot for the data.

boxplot(marinediversity$diversity~interaction(marinediversity$season, marinediversity$lattitude), col=c("red", "steelblue1"), ylab="Diversity", las=2) #make a boxplot for the data.
