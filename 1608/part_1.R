###### Catherine Okuboyejo, 180279704
# data set 1608
# NOTE: script assumes all files in the current working directory
# I submitted a pull request on github

#### Dataset 1: Marine microbial diversity ####
# read in data
marine_data <- read.table("part_1_student_1608.tdf", header = TRUE)

# check data
str(marine_data)

## How does microbial diversity change with latitude?
#exploratory plots - looks like there is a difference. Data do not look too skewed either.
plot(marine_data$latitude, marine_data$UniFracInd, xlab = "latitude", ylab = "UniFrac") 
par(mfrow=c(1,2))
hist(marine_data$UniFracInd[marine_data$latitude=="tropical"], xlab = "tropical unifrac", main = "")
hist(marine_data$UniFracInd[marine_data$latitude=="temperate"], xlab = "temperate unifrac", main = "")

# calculate means and standard deviations for reporting
mean(marine_data$UniFracInd[marine_data$latitude=="tropical"])
sd(marine_data$UniFracInd[marine_data$latitude=="tropical"])

mean(marine_data$UniFracInd[marine_data$latitude=="temperate"])
sd(marine_data$UniFracInd[marine_data$latitude=="temperate"])

# run t test (H0=no difference in unifracid between tropical and temperate latitudes)
with(marine_data,
     t.test(UniFracInd[latitude=="tropical"], UniFracInd[latitude=="temperate"], paired = FALSE))

# graph it girl
table(marine_data$latitude) # how many of each?? - 20

# calculate means to plot a bar graph
lat_graph <- tapply(marine_data$UniFracInd, marine_data$latitude, mean)
barplot(lat_graph)

## How does microbial diversity change with time of year?
#exploratory plots - looks like there is a difference. distributions look ok
par(mfrow=c(1,1))
plot(marine_data$season, marine_data$UniFracInd, xlab = "season", ylab = "UniFrac")  
par(mfrow=c(1,2))
hist(marine_data$UniFracInd[marine_data$season=="Jan"], xlab = "jan unifrac", main = "")
hist(marine_data$UniFracInd[marine_data$season=="Aug"], xlab = "aug unifrac", main = "")

# calculate means and standard deviations for reporting
mean(marine_data$UniFracInd[marine_data$season=="Jan"])
sd(marine_data$UniFracInd[marine_data$season=="Jan"])

mean(marine_data$UniFracInd[marine_data$season=="Aug"])
sd(marine_data$UniFracInd[marine_data$season=="Aug"])

# run t test (H0=no difference in unifracid between Jan and Aug)
with(marine_data,
     t.test(UniFracInd[season=="Jan"], UniFracInd[season=="Aug"], paired = FALSE))

# Is there an interaction between the season, and location?
# run a two factor ANOVA
marine_anova <- lm(UniFracInd ~ latitude*season, data = marine_data)
anova(marine_anova) # no significant interaction
