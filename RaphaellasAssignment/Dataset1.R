#read in the file, it has a header so you need "header = T"
MMD <- read.table("273_part1.tdf", header = T)

#attach command lets you use straight variable names i.e. "diversity" and not "MMD$diversity"
attach(MMD)

#check to make sure variables are the right type
str(MMD)

#set the viewing parameters to show two graphs
par(mfrow = c(1,2))

#take a first look at the data
#the first two questions ask about diversity v. lattitude and diversity v. season
boxplot(diversity~season, col = "steel blue")
boxplot(diversity~lattitude, col = "purple")

#to answer the first question, let's run a t-test
#we'll check the variance first seeing as an assumption of the t-test is the variances are the same
seasonvar = tapply(diversity,season,var)
lattitudevar = tapply(diversity, lattitude, var)
seasonvar
lattitudevar

#lattitude variances are very close, but the variances in season are a bit more far apart.
#run t-tests to see if the means are significantly diffferent between the two groups (the test automatically assumes variances are different unless we specifiy they that they are)
t.test(diversity~season)
t.test(diversity~lattitude, var.equal = T)    #here we specify variances as being equal because they are so close together

#Diversity v. Lattitude has a signifcant difference in means but Diversity v. Season does not

#lets look at effect size of diversity v. lattitude
meanmatrix <- tapply(diversity,lattitude, mean)
standdev <- sd(diversity)
effectsize <- (meanmatrix[1]-meanmatrix[2])/standdev
#effect size is 1.126

#Let's make some graphs to illustrate this for a discussion of results
#making a graph for 95% confidence intervals by lattitude

MMDlat.matrix <- tapply(diversity, list(lattitude), mean)   #matrix with sample means
MMDlat.sd <- tapply(diversity, list(lattitude),sd)          #matrix with sample standard deviations
MMDlat.se <- MMDlat.sd/sqrt(10)                             #matrix with sample standard errors (each sample set has 10 values 20/2)
latcritval <- abs(qt(0.025, 9))                             #find critical value of t at 9 degrees of freedom
MMDlat.confidence <- MMDlat.se*latcritval                   #matrix with errors to construct confidence intervals

par(mfrow=c(1,1))  #set parameters to look at one graph

#save locations on barplot for plotting arrows
locationslat <- barplot(MMDlat.matrix, beside = T)

#plot a barplot with the sample means in each category, give them seperate colors, include labels to say what each one is, include a y label to identify it's a measure of diversity
barplot(MMDlat.matrix, beside = T, col = c("steel blue", "light blue"), legend.text = c("Equatorial", expression(paste("Temperate"))),args.legend = list(x ="topright", bty = "n"),ylim = c(-2,4),ylab = "Diversity", font.lab =2, font.axis =2, xaxt ="n")

#add arrows for the confidence intervals, the first one adds an arrow going to mean + se and the second adds an arrow going to mean -se
#angle and length refer to the bit on the end, angle can rotate to make it an actual arrow (here we just want a straight line), length can adjust how long it is
arrows(locationslat, MMDlat.matrix, locationslat, MMDlat.matrix + MMDlat.confidence, angle =90, length =0.1)
arrows(locationslat, MMDlat.matrix, locationslat, MMDlat.matrix - MMDlat.confidence, angle =90, length =0.1)

#making a graph for 95% confidence intervals by season
#this is just a repeat of above but plugging in season instead of lattitude

MMDsea.matrix <- tapply(diversity, list(season), mean)   #matrix with sample means
MMDsea.sd <- tapply(diversity, list(season),sd)          #matrix with sample standard deviations
MMDsea.se <- MMDsea.sd/sqrt(10)                          #matrix with sample standard errors (each sample set has 10 values 20/2)
seacritval <- abs(qt(0.025, 9))                          #find critical value of t at 9 degrees of freedom
MMDsea.confidence <- MMDsea.se*seacritval

par(mfrow=c(1,1))
locationssea <- barplot(MMDsea.matrix, beside = T)
barplot(MMDsea.matrix, beside = T, col = c("coral", "coral4"), legend.text = c("August", expression(paste("January"))),args.legend = list(x ="topright", bty = "n"),ylim = c(0,4),ylab = "Diversity", font.lab =2, font.axis =2, xaxt ="n")
arrows(locationssea, MMDsea.matrix, locationssea, MMDsea.matrix + MMDsea.confidence, angle =90, length =0.1)
arrows(locationssea, MMDsea.matrix, locationssea, MMDsea.matrix - MMDsea.confidence, angle =90, length =0.1)


#Now we need to address the question of whether there is any interaction
#We're going to use multi-factor ANOVA (we have 2 factors)

#Fit a model
MMDmodel <- lm(diversity~season*lattitude)

#Let's look at the diagnostic plots to see if this model is ok
par(mfrow = c(2,2)) ##make space for four graphs
plot(MMDmodel)

#residuals vs. fitted is ok but normal Q-Q isn't great
#note to self:
# we tried log() - ing the data but then realized that was a bad idea, diversity is sometimes greater than -1 (thus log()-ing gets rid of some data points)
# tried adding +3 and then log () but this made the data really weird, so we're going to stick with the unmodified diversity 


#Let's look at what ANOVA and Summary say about the model and try a drop1 F-test for significance as well
anova(MMDmodel)
summary(MMDmodel)
drop1(MMDmodel, test= "F")
#both ANOVA and summary suggest that lattitude is significant but season is not, when each are considered seperately
#did a model for lm(diversity~season+lattitude but it fits worse than this one so it's pointless)


#resounding NON-significant result for season, lattitude interaction


#make a graph of 95% confidence intervals with the eq temp divide on x axis.
MMDrev.matrix <- tapply(diversity, list(season,lattitude), mean)   #matrix with sample means
MMDrev.sd <- tapply(diversity, list(season,lattitude),sd)          #matrix with sample standard deviations
MMDrev.se <- MMDrev.sd/sqrt(5)                                        #matrix with sample standard errors (each sample set has 5 values 20/4)
critval <- abs(qt(0.025, 4))                                       #find critical value of t at 4 degrees of freedom
MMDrev.confidence <- MMDrev.se*critval

par(mfrow=c(1,1))
locationsrev <- barplot(MMDrev.matrix, beside = T)
barplot(MMDrev.matrix, beside = T, col = c("goldenrod", "darkgoldenrod4"), legend.text = c("August", expression(paste("January"))),args.legend = list(x ="topright", bty = "n"),ylim = c(-4,6),ylab = "Diversity", xlab = "Lattitude", font.lab =2, font.axis =2, xaxt ="n")
axis(side=1, at =c(2,5), labels = c("Equatorial","Temperate"),font = 2)
arrows(locationsrev, MMDrev.matrix, locationsrev, MMDrev.matrix + MMDrev.confidence, angle =90, length =0.1)
arrows(locationsrev, MMDrev.matrix, locationsrev, MMDrev.matrix - MMDrev.confidence, angle =90, length =0.1)




