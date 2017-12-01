# clear the environment
rm(list=ls())
# install ggplot2 if not already installed
install.packages(c("ggplot2"))

# reset the plot window
par(mfrow=c(1,1))

# load ggplot2
library(ggplot2)

# import the data
marine_microbial <- read.table("596_part1.tdf", header=TRUE)
# look at the first ten rows
head(marine_microbial)
# check the data types
str(marine_microbial)
attach(marine_microbial)

summary(marine_microbial)

# is diversity normally distributed?
hist(diversity)
# looks to be

# Reorder season levels to be in chronological order (for neatness' sake)
season <- factor(season, 
                 levels = c("Jan", "Aug"))

season_means <- tapply(diversity, season, mean)
season_means
# Season Means
#        Jan        Aug 
# -0.2972546  0.4504886 

season_variance <- tapply(diversity, season, var)
season_variance
# Season Variance
#      Jan      Aug 
# 1.669689 2.830311 

season_sd <- tapply(diversity, season, sd)
season_sd
# Season Standard Deviation
#      Jan      Aug 
# 1.292164 1.682353

lat_means <- tapply(diversity, lattitude, mean)
lat_means
# Lattitude Means
# equatorial  temperate 
#  0.5837621 -0.4305281 

lat_variance <- tapply(diversity, lattitude, var)
lat_variance
# Lattitude Variance
# equatorial  temperate 
#   2.237824   2.001251 

lat_sd <- tapply(diversity, lattitude, sd)
lat_sd
# Lattitude Standard Deviation
# equatorial  temperate 
#   1.495936   1.414656 

hist(diversity[season=="Jan"])
hist(diversity[season=="Aug"])

# Diversity ~ Season Boxplot
# Data points and means (diamonds) overlaid
ggplot(marine_microbial, aes(x = reorder(season,diversity,mean), y = diversity))+
  geom_boxplot()+
  geom_point(size = 2, alpha = 0.5)+
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 5)+
  xlab("Season")+
  ylab("Microbial Diversity")+
  theme_bw()

hist(diversity[lattitude=="temperate"])
hist(diversity[lattitude=="equatorial"])

# Diversity ~ Lattitude Boxplot
# Data points and means (diamonds) overlaid
ggplot(marine_microbial, aes(x = lattitude, y = diversity))+
  geom_boxplot()+
  geom_point(size = 2, alpha = 0.5)+
  stat_summary(fun.y = "mean", geom = "point", shape = 18, size = 5)+
  xlab("Latitude")+
  ylab("Microbial Diversity")+
  theme_bw()

# Are the season means significantly different?
t.test(diversity~season)
# Welch Two Sample t-test

# data:  diversity by season
# t = -1.1147, df = 16.877, p-value = 0.2806
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.1638347  0.6683481
# sample estimates:
# mean in group Jan mean in group Aug 
#        -0.2972546         0.4504886 

# Season means are not statistically different.

# Are the latitude means statistically different?
t.test(diversity~lattitude)
# Welch Two Sample t-test

# data:  diversity by lattitude
# t = 1.5579, df = 17.944, p-value = 0.1367
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.3538873  2.3824677
# sample estimates:
# mean in group equatorial  mean in group temperate 
#                0.5837621               -0.4305281 

# Latitude means are not statistically different

# is there an interaction between season and location
interaction.plot(season, lattitude, diversity,
                 ylab = "Mean of microbial diversity",
                 xlab = "Season")

# interactions are also seen by comparing boxplots side by side
boxplot(diversity~season*lattitude,
        ylab = "Mean of microbial diversity")

# we can see an interaction, but is it significant?
summary(aov(diversity~season*lattitude))
        
#                  Df Sum Sq Mean Sq F value Pr(>F)  
# season            1  2.796   2.796   1.601 0.2239  
# lattitude         1  5.144   5.144   2.945 0.1054  
# season:lattitude  1  7.410   7.410   4.242 0.0561 .
# Residuals        16 27.947   1.747                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1





  
