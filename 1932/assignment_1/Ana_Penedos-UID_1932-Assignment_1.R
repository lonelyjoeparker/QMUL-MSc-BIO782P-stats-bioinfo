## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
marine <- read.table("part_1_student_1932.tdf", 
                     sep = "\t",
                     header = TRUE)
head(marine) # check if there are obvious issues with data import
str(marine) # check number of observations, variables and their type

## ------------------------------------------------------------------------
# Defining variables to simplify dataframe references:
diversity.col <-marine$UniFracInd
latitude.col <- marine$latitude
season.col <- marine$season

## ------------------------------------------------------------------------
library(ggplot2)
# set up plot-specific elements
# ggplot object plotting UniFracInd against latitude
marine.latitude.plot <- ggplot(marine,
                             aes(factor(latitude.col), 
                                 diversity.col,
                                 fill = latitude.col))
# specific plot title and axes labels
marine.latitude.titles <- labs(title = "Microbial Density vs. Latitude",
                               x = "Latitude",
                               y = "UniFracInd")
# change fill from the default colours
latitude.fill <- scale_fill_manual(values=c("#009999", "#0099FF"))

# set up re-usable elements to edit violin plots
# create a violin plot with count on the y axis and including outliers
violin.plot <- geom_violin(scale = "count",
                           trim = FALSE)
## add sample points
#geom_jitter(height = 0, width = 0.2) +
# add a boxplot
add.boxplot <- geom_boxplot(width = 0.1,
                            fill = "white")
# add the mean of values
add.mean <- stat_summary(fun.y = mean,
                         geom = "point",
                         shape = 23,
                         size = 2,
                         fill = "red")
# change the plot theme to classic (no grey plot area or gridlines)
set.plot.theme <- theme_classic(base_size = 16)
# set variable for the style of plot titles;
# hjust is the horizontal position of the title
title.style <- element_text(face = "bold.italic",
                            size = 18,
                            hjust = .5)
# set variable for the style of plot axes
axes.labels.style <- element_text(face = "bold")
# apply title and axes format to the plot theme and remove legend
title.axes.theme <- theme(plot.title = title.style,
                          axis.title = axes.labels.style,
                          legend.position="none")

# bring plot and formatting together
marine.latitude.plot + marine.latitude.titles + violin.plot + add.boxplot + 
  add.mean + latitude.fill + set.plot.theme + title.axes.theme

## ------------------------------------------------------------------------
mean(diversity.col[latitude.col=="temperate"])
sd(diversity.col[latitude.col=="temperate"])
mean(diversity.col[latitude.col=="tropical"])
sd(diversity.col[latitude.col=="tropical"])

## ------------------------------------------------------------------------
hist(diversity.col)

## ------------------------------------------------------------------------
par(mfrow = c(1,2))
hist(diversity.col[latitude.col == "temperate"], 
     breaks = 5,
     main = "Diversity in temperate region",
     xlab = "Diversity")
hist(diversity.col[latitude.col == "tropical"], 
     breaks = 5,
     main = "Diversity in tropical region",
     xlab = "Diversity")

## ------------------------------------------------------------------------
shapiro.test(diversity.col[latitude.col=="temperate"])
shapiro.test(diversity.col[latitude.col=="tropical"])

## ------------------------------------------------------------------------
length(latitude.col[latitude.col=="temperate"])
length(latitude.col[latitude.col=="tropical"])

## ------------------------------------------------------------------------
library(car)
leveneTest(diversity.col ~ latitude.col)

## ------------------------------------------------------------------------
t.test(diversity.col[latitude.col=="temperate"], diversity.col[latitude.col=="tropical"])

## ------------------------------------------------------------------------
# set up plot-specific elements
# ggplot object plotting UniFracInd against season
marine.season.plot <- ggplot(marine,
                             aes(factor(season.col), 
                                 diversity.col,
                                 fill = season.col))
# specific plot title and axes labels
marine.season.titles <- labs(title = "Microbial Density vs. Season",
                             x = "Season",
                             y = "UniFracInd")
# change fill from the default colours
season.fill <- scale_fill_manual(values=c("#FF9900", "#6699FF"))

# bring plot and formatting together
marine.season.plot + marine.season.titles + violin.plot + add.boxplot + 
  add.mean + season.fill + set.plot.theme + title.axes.theme

## ------------------------------------------------------------------------
mean(diversity.col[season.col=="Aug"])
sd(diversity.col[season.col=="Aug"])
mean(diversity.col[season.col=="Jan"])
sd(diversity.col[season.col=="Jan"])

## ------------------------------------------------------------------------
par(mfrow=c(1,2))
hist(diversity.col[season.col=="Aug"],
     main="Diversity in Aug",
     xlab="Diversity")
hist(diversity.col[season.col=="Jan"],
     main="Diversity in Jan",
     xlab="Diversity")

## ------------------------------------------------------------------------
shapiro.test(diversity.col[season.col=="Aug"])
shapiro.test(diversity.col[season.col=="Jan"])

## ------------------------------------------------------------------------
length(season.col[season.col=="Aug"])
length(season.col[season.col=="Jan"])

## ------------------------------------------------------------------------
leveneTest(diversity.col ~ season.col)

## ------------------------------------------------------------------------
t.test(diversity.col[season.col=="Aug"], diversity.col[season.col=="Jan"])

## ------------------------------------------------------------------------
# Correlation of diversity with an interaction of latitude and season
corr.seas.lat <- lm(diversity.col ~ season.col * latitude.col)
anova(corr.seas.lat)

## ------------------------------------------------------------------------
luciferase <- read.table("part_2_student_1932.tdf",
                         sep = "\t",
                         header = TRUE)
head(luciferase) # quick check of correct data import
names(luciferase) # check names of columns for dataset
str(luciferase) # check that factors and variables were assigned correctly

## ------------------------------------------------------------------------
# simplify dataframe references
luciferase.expression <- luciferase$expression_fold
aa.substitutions <- luciferase$distance

## ------------------------------------------------------------------------
plot(aa.substitutions, luciferase.expression,
     main="Luciferase Expression vs. Distance",
     xlab="Amino Acid Substitutions",
     ylab="Expression",
     pch=16,
     col="steelblue")

## ------------------------------------------------------------------------
expr.dist.model <- lm(luciferase.expression ~ aa.substitutions)
expr.dist.model

## ------------------------------------------------------------------------
summary(expr.dist.model)

## ------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(expr.dist.model)

## ------------------------------------------------------------------------
# import data from tab-delimitted file
hiv <- read.table("part_3_student_1932.tdf",
                  sep = "\t",
                  header = TRUE)
tail(hiv)  # quick check that data was correctly imported
names(hiv)  # dataframe variable names
str(hiv)  # dataframe structure

## ------------------------------------------------------------------------
# set up dataframe call vaariables
viral.load <- hiv$VLoad
cd4.level <- hiv$CD4
site <- hiv$tissue
shannon <- hiv$score_shannon
distance <- hiv$score_distance

## ------------------------------------------------------------------------
par(mfrow=c(2, 2))
plot(cd4.level, viral.load,
     main = "A. Viral Load vs. CD4 level",
     xlab = "CD4 level",
     ylab = "viral load")
plot(site, viral.load,
     main = "B. Viral Load vs. Tissue",
     xlab = "tissue",
     ylab = "viral load")
plot(shannon, viral.load,
     main = "C. Viral Load vs. Population Diversity",
     xlab = "shannon",
     ylab = "viral load")
plot(distance, viral.load,
     main = "D. Viral Load vs. Distance to Ref",
     xlab = "distance",
     ylab = "viral load")

## ------------------------------------------------------------------------
par(mfrow=c(2, 2))
plot(cd4.level[site=="brain"], viral.load[site=="brain"],
     main = "A. Viral Load in Brain",
     xlab = "CD4 level",
     ylab = "viral load")
plot(cd4.level[site=="spinalCord"], viral.load[site=="spinalCord"],
     main = "B. Viral Load in Spinal Cord",
     xlab = "CD4 level",
     ylab = "viral load")
plot(site[cd4.level=="hi"], viral.load[cd4.level=="hi"],
     main = "C. Viral Load for High CD4 level",
     xlab = "tissue",
     ylab = "viral load")
plot(site[cd4.level=="lo"], viral.load[cd4.level=="lo"],
     main = "D. Viral Load for Low CD4 level",
     xlab = "tissue",
     ylab = "viral load")

## ------------------------------------------------------------------------
par(mfrow=c(2, 4))
plot(shannon[site=="brain"], viral.load[site=="brain"],
     main = "Viral Load (Brain)\nvs. Shannon",
     xlab = "shannon",
     ylab = "viral load")
plot(shannon[site=="spinalCord"], viral.load[site=="spinalCord"],
     main = "Viral Load (Spinal Cord)\nvs. Shannon",
     xlab = "shannon",
     ylab = "viral load")
plot(shannon[cd4.level=="hi"], viral.load[cd4.level=="hi"],
     main = "Viral Load (High CD4)\nvs. Shannon",
     xlab = "shannon",
     ylab = "viral load")
plot(shannon[cd4.level=="lo"], viral.load[cd4.level=="lo"],
     main = "Viral Load (Low CD4)\nvs. Shannon",
     xlab = "shannon",
     ylab = "viral load")
plot(distance[site=="brain"], viral.load[site=="brain"],
     main = "Viral Load (Brain)\nvs. Distance",
     xlab = "distance",
     ylab = "viral load")
plot(distance[site=="spinalCord"], viral.load[site=="spinalCord"],
     main = "Viral Load (Spinal Cord)\nvs. Distance",
     xlab = "distance",
     ylab = "viral load")
plot(distance[cd4.level=="hi"], viral.load[cd4.level=="hi"],
     main = "Viral Load (High CD4)\nvs. Distance",
     xlab = "distance",
     ylab = "viral load")
plot(distance[cd4.level=="lo"], viral.load[cd4.level=="lo"],
     main = "Viral Load (Low CD4)\nvs. Distance",
     xlab = "distance",
     ylab = "viral load")

## ------------------------------------------------------------------------
par(mfrow=c(2, 2))
plot(shannon[site=="brain"][cd4.level=="hi"], 
     viral.load[site=="brain"][cd4.level=="hi"],
     main = "Viral Load (Brain/Hi)\nvs. Shannon",
     xlab = "shannon",
     ylab = "viral load")
plot(shannon[site=="spinalCord"][cd4.level=="hi"], 
     viral.load[site=="spinalCord"][cd4.level=="hi"],
     main = "Viral Load (SC/Hi)\nvs. Shannon",
     xlab = "shannon",
     ylab = "viral load")
plot(shannon[site=="brain"][cd4.level=="lo"], 
     viral.load[site=="brain"][cd4.level=="lo"],
     main = "Viral Load (Brain/Lo)\nvs. Shannon",
     xlab = "shannon",
     ylab = "viral load")
plot(shannon[site=="spinalCord"][cd4.level=="lo"], 
     viral.load[site=="spinalCord"][cd4.level=="lo"],
     main = "Viral Load (SC/Lo)\nvs. distance",
     xlab = "shannon",
     ylab = "viral load")

## ------------------------------------------------------------------------
par(mfrow=c(2, 2))
plot(distance[site=="brain"][cd4.level=="hi"], 
     viral.load[site=="brain"][cd4.level=="hi"],
     main = "Viral Load (Brain/Hi)\nvs. distance",
     xlab = "distance",
     ylab = "viral load")
plot(distance[site=="spinalCord"][cd4.level=="hi"], 
     viral.load[site=="spinalCord"][cd4.level=="hi"],
     main = "Viral Load (SC/Hi)\nvs. distance",
     xlab = "distance",
     ylab = "viral load")
plot(distance[site=="brain"][cd4.level=="lo"], 
     viral.load[site=="brain"][cd4.level=="lo"],
     main = "Viral Load (Brain/Lo)\nvs. distance",
     xlab = "distance",
     ylab = "viral load")
plot(distance[site=="spinalCord"][cd4.level=="lo"], 
     viral.load[site=="spinalCord"][cd4.level=="lo"],
     main = "Viral Load (SC/Lo)\nvs. distance",
     xlab = "distance",
     ylab = "viral load")

## ------------------------------------------------------------------------
start.model <- lm(viral.load ~ site + cd4.level + shannon + distance + site:cd4.level)
summary(start.model)

## ------------------------------------------------------------------------
my.model = step(start.model,
                scope=c(lower=~site,
                        upper=~site * cd4.level * shannon * distance),
                direction="both")

## ------------------------------------------------------------------------
my.model2 <- step(my.model,
                  direction="backward")

## ------------------------------------------------------------------------
my.model3 <- step(lm(viral.load ~ cd4.level:shannon),
                  scope = ~site * cd4.level * shannon * distance,
                  direction = "forward")

## ------------------------------------------------------------------------
final.model <- my.model3
summary(final.model)

## ------------------------------------------------------------------------
par(mfrow = c(2,2))
plot(final.model)

