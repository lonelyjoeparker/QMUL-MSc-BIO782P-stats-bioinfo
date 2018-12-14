###set working directory and read in the data
#setwd("~/Documents/Mydocs/QMUL-2018/Module Three-Statistics and Bioinformatics/assignment ")
HIV=read.table("part_3_student_1204.tdf.txt",header = T)
str(HIV)

###clean the data
VLoad = as.numeric(HIV$VLoad)
CD4 = as.factor(HIV$CD4)
Tissue = as.factor(HIV$tissue)
Shannon = as.numeric(HIV$score_shannon)
score_distance = as.numeric(HIV$score_distance)
Distance = as.numeric(HIV$score_distance)

###plot VLoad(explain) via other different variables to find the preliminary  relationship between VLoad and other factors to see if they affact the VLoad
###jietu meizhang kan fenxi jieguo 
plot(Tissue,VLoad)
plot(CD4,VLoad)
plot(Shannon,VLoad)
plot(Distance,VLoad)
###found that distance has little effect on VLoad

###fit VLoad with only one variable in a model to see which variables are important independent variables:
model1=lm(VLoad ~ CD4 * Tissue * score_distance * Shannon)
anova(model1)
###seems that Shannon, CD4 and their interaction is the most important to explain VLoad

###then do stepwise AIC,first forward for VLoad VS 4 variables:
forward_final1=step(lm(VLoad ~ Shannon),scope=(~Shannon*CD4*Tissue*Distance),direction="forward")
###minimum AIC:174.17   model: VLoad ~ Shannon

###backward for VLoad VS 4 variables:
backwards_final1=step(lm(VLoad ~ Shannon * Distance * CD4 * Tissue),direction="backward")
###minimum AIC:177.65   VLoad ~ Shannon * Distance * CD4 * Tissue

###forward for VLoad VS CD4, Shannon and Tissue:
forward_final2=step(lm(VLoad ~ Shannon),scope=(~Shannon*CD4*Tissue),direction="forward")
###minimum AIC:174.17   model: VLoad ~ Shannon

###backward for VLoad VS CD4, Shannon and Tissue:
backwards_final2=step(lm(VLoad ~ Shannon * CD4 * Tissue),direction="backward")
###minimum AIC:173.33   model:VLoad ~ Shannon + CD4 + Shannon:CD4

###backward for VLoad VS CD4, Shannon and Distance:
backwards_final3=step(lm(VLoad ~ Shannon * CD4 * Distance),direction="backward")
###minimum AIC:173.33   model:VLoad ~ Shannon + CD4 + Shannon:CD4

###backward for VLoad VS Tissue, Shannon and Distance:
backwards_final3=step(lm(VLoad ~ Shannon * CD4 * Distance),direction="backward")
###minimum AIC:173.33   model:VLoad ~ Shannon + CD4 + Shannon:CD4

###backward for VLoad VS CD4, Tissue and Distance:
backwards_final4=step(lm(VLoad ~ Tissue * CD4 * Distance),direction="backward")
###minimum AIC:198.33     VLoad ~ Tissue * CD4 * Distance

###to sum up, the best model for VLoad with equal or more than 2 variables is the following:
###model:VLoad ~ Shannon + CD4 + Shannon:CD4   This result is identical to previous ANOVA analysis
###then test from a single factor to various factors
forward_final5=step(lm(VLoad ~ Shannon),scope=(~Shannon*CD4*Tissue*Distance),direction="forward")
### minimum AIC:174.17     VLoad ~ Shannon
forward_final6=step(lm(VLoad ~ CD4),scope=(~Shannon*CD4*Tissue*Distance),direction="forward")
###minimum AIC:173.33   model:VLoad ~ Shannon + CD4 + Shannon:CD4
forward_final7=step(lm(VLoad ~ Tissue),scope=(~Shannon*CD4*Tissue*Distance),direction="forward")
###minimum AIC:175.73   model:VLoad ~ Tissue + Shannon
forward_final8=step(lm(VLoad ~ Distance),scope=(~Shannon*CD4*Tissue*Distance),direction="forward")
###minimum AIC:176.11   model:VLoad ~ Distance + Shannon

####the best model is VLoad ~ Shannon + CD4 + Shannon:CD4, with AIC:173.33