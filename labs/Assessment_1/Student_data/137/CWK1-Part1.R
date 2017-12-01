#Name: Dionysios Grigoriadis
#ID: 170484367
#BIO782P Statistics and Bioinformatics Assignment 2017
#CWK 1

#Dataset1

#Functions defined to perform the required analyses:
p_stars <- function(pval){
  #Function which takes a p-value as its argument and returns a string 
  #of the type "P < a" followed by the corresponding symbol of statistical
  #significance.
  if(pval<=0.001){p_star <- "P < 0.001 ***"}
  else if(pval<=0.01){p_star <- "P < 0.01 **"}
  else if(pval<=0.05){p_star <- "P < 0.05 *"}
  else if(pval>0.05){p_star <- "P > 0.05"}
  return(p_star)
}
#Reading of the data
marine_data <- read.table("137_part1.tdf", header = TRUE)
attach(marine_data)

#Boxplot to visualise the differences between the two examined groups.
boxplot(diversity~lattitude, names=c("Equatorial","Temperate"), xlab="Lattitude", 
        ylab="Related Diversity", cex.lab=1.3, col="grey 95")

#The number of samples of each group needed for the report are calculated. 
equatorial_samples=length(diversity[lattitude=="equatorial"])
temperate_samples=length(diversity[lattitude=="temperate"])

#Student t-test to check if the observed differences are of statistical significance.
ttest_lat_div<-t.test(diversity[lattitude=="equatorial"], diversity[lattitude=="temperate"])
lat_div_p <- ttest_lat_div$p.value
ttest_lat_div

#The final bloxplot is printed. It depicts the bars of the examined groups, 
#while it provides information about the performed t-test's p-value.
png(filename = "diversity_equatorial_temperate_plot.png")
boxplot(diversity~lattitude, names=c("Equatorial Waters","Temperate Waters"), xlab="Sampling Location", 
        ylab="Relative Microbial Diversity", cex.lab=1.3, col="grey 95")
text(0.7,2.3,p_stars(lat_div_p),cex=1) #Appropriate location on the graph.
dev.off()

#Question 2

#Boxplot to visualise the differences between the two examined groups in bars.
boxplot(diversity~season, names=c("August","January"), xlab="Season", 
        ylab="Relative Microbial Diversity", cex.lab=1.3, col="grey 95")


#The number of samples of each group needed for the group
#is calculated.
January_samples=length(diversity[season=="Jan"])
August_Samples=length(diversity[season=="Aug"])

#Student t-test to check if the observed differences are of statistical significance.
ttest_sea_div<-t.test(diversity[season=="Jan"], diversity[season=="Aug"])
sea_div_p <- ttest_sea_div$p.value
ttest_sea_div

#The final bloxplot is printed. It depicts the bars of the examined groups, 
#while it provides information about the performed t-test's p-value.
png(filename = "diversity_August_January_plot.png")
boxplot(diversity~season, names=c("August","January"), xlab="Season", 
        ylab="Relative Microbial Diversity", cex.lab=1.3, col="grey 95")
text(0.6,2.3,p_stars(sea_div_p),cex=1)
dev.off()

#Question 3
#A contingency table is generated for the location and season to
#perform a chi-squared test.
lat_sea_tbl = table(lattitude, season)
lat_sea_tbl
chisq.test(lat_sea_tbl)



