data3= read.table("/Users/vithusaaselva/Desktop/896_part3.tdf.txt", header = TRUE) #reads dataset 3
str(data3) #view one line structure in object is displayed
par(mfrow= c(1,2)) #number of plots displayed per entry
mod1= data3$ct.viral_load.~ data3$CD4 
plot (mod1) # plot to see relationship between viral load & CD4
mod2= data3$ct.viral_load.~ data3$Shannon_diversity
plot (mod2) # plot to see relationship between viral load & Shannon_diversity
mod3= data3$ct.viral_load. ~ data3$Tissue
plot (mod3) # plot to see relationship between viral load & Tissue
mod4= data3$ct.viral_load.~ data3$Distance
plot (mod4) # plot to see relationship between viral load & Distance
modall= lm(data3$ct.viral_load. ~ data3$CD4+data3$Shannon_diversity+data3$Tissue+data3$Distance) #model with all variables
summary(modall) 
step(modall, direction = "backward") #backward stepwise regression, includes drop1 test, leading to best model

