part3<-read.table("/home/bt17186/Desktop/61_part3.tdf.txt", header = TRUE)
str(part3)
my_model<-lm(part3$viral_load~ part3$CD4 * part3$Shannon_diversity * part3$Tissue *part3$Distance) # interaction with all components
anova(my_model)# produces the analysis of variance, shows which variables are of significance
drop1(my_model,test ="F") #drop test to show single deletions
my_model1a = update(my_model, . ~ . -part3$CD4) # updating "my_model1" to see if significance can be improved
anova(my_model1a)
my_model2a = update(my_model1a, . ~ . -part3$Tissue*part3$CD4)
anova(my_model2a) # the best model