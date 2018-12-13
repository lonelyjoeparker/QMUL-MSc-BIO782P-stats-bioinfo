#work on dataset 1
#Find and open dataset 1
Mar_Micro <- read.table(file.choose(new = FALSE ), header = TRUE)#choose the part_1_student_1493.tdf
str(Mar_Micro)
attach(Mar_Micro) #do this to attach it inorder to utalise the variables within the table without having to type Mar_Micro$ everytime.
hist(UniFracInd) #to see if data is normally distributed.
plot(UniFracInd~latitude, col="pink") #auto produces a boxplot with the axis labels. 
plot(UniFracInd~season, col="pink") 
#get the means for each to create a table of 4 values
Mean_jan_trop <- sum(UniFracInd[1:10])/10
Mean_aug_trop <- sum(UniFracInd[11:20])/10
Mean_jan_temp <- sum(UniFracInd[21:30])/10
Mean_aug_temp <- sum(UniFracInd[31:40])/10

#table for the means to visulise 
SeasonXLat <- matrix(c(Mean_jan_trop, Mean_aug_trop,Mean_jan_temp,Mean_aug_temp), nrow = 2)
rownames(SeasonXLat) <- c("January","August")
colnames(SeasonXLat) <- c("Tropical","Temperate")

model1 <- lm(UniFracInd ~ latitude * season)
anova(model1)
summary(model1)
par(mfrow = c(2,2))
plot(model1)
par(mfrow = c(1,1))


#work on dataset 2
#find an open dataset 2
Nucleo_Sub <- read.table(file.choose(new = FALSE ), header = TRUE)#choose the part_1_student_1493.tdf
str(Nucleo_Sub) #check the structure of the file to ensure it has been loaded in correctly. 
attach(Nucleo_Sub) #attach the Nucleo_Sub object, so you can utalise the factors within the file.
plot(distance,expression_fold, ylab = "Expression Level", xlab = "Genetic Distance", main = "Change in Luciferase Homolog Expression with Genetic Distance", pch = 16, col = "steelblue")
#this plots the expression variable as the dependent on the y axis and distance as the independent variable on the x axis. 
#this in turn allows us to see how the genetic distance changes expression levels. 
model2 <- lm(expression_fold ~ distance) #the variables for the model are in this order as expression id dependent on distance which is independent. 
summary(model2) #check the model but more importantly find the coefficents for the line y=a+bx
abline(model2, col="red") #plot the liner line of the model against the data points.

par(mfrow = c(2,2))
plot(model2) #these plots will be used to check the assumptions of the linear model. 
par(mfrow = c(1,1))
#the QQ-plot for normality of errors looks as if it is normally distributed.
hist(model2$residuals) #nother visulaition to see if the errors are normally distributed. 
shapiro.test(model2$residuals) #formal normality test of residuals to check normal distribution. 
#accept null hyp - noramlly dist. however it is likely there isnt enough data to see minute changes 



#Work on dataset 3
#find and open dataset 3
HIV <- read.table(file.choose(new = FALSE ), header = TRUE)#choose the part_1_student_1493.tdf
str(HIV)
attach(HIV)
hist(VLoad)
hist(score_distance)
hist(score_shannon)

backwards_final <- step(lm(VLoad ~ CD4 * tissue * score_distance * score_shannon), direction = "backward")
forwards_final <- step(lm(VLoad ~ CD4*tissue), scope = (~CD4*tissue*score_distance*score_shannon), direction = "forward")

#best both direction
both_final <- step(lm(VLoad~score_shannon*score_distance), scope = c(lower=~score_distance, upper=~score_shannon*score_distance*tissue), direction = "both")



#assesment 2


stimulation_potential = seq(-100, 40, by=10)
response_voltage_control = c(-70.00000,-70.00000,-70.00000,-70.00000,-70.00000,35.79871,38.75082,39.30423,39.65390,35.55835,35.66762,36.55754,39.51027,35.65274,39.17013)
response_voltage_concN_1 = c(-70.00000,-70.00000,-70.00000,-70.00000,37.88144,35.64345,36.54319,36.18773,36.67542,38.56634,38.53102,37.73421,38.69826,38.67744,37.21896)
response_voltage_concN_2 = c(-70.00000,-70.00000,-70.00000,37.16110,39.28471,36.57506,38.03037,39.37853,39.92131,39.01116,39.46393,36.16073,37.70418,35.88598,38.53137)
response_voltage_concN_3 = c(-70.00000,-70.00000,36.25729,35.70035,35.61478,36.81245,37.01299,39.34139,36.59520,35.00749,37.94440,36.10611,37.15617,35.29182,35.32412)
plot(stimulation_potential,stimulation_potential,type='n',xlab='Stimulation, mV',ylab='Excitation, mV')
lines(stimulation_potential,response_voltage_control)
lines(stimulation_potential,response_voltage_concN_1,col="green")
lines(stimulation_potential,response_voltage_concN_2,col="orange")
lines(stimulation_potential,response_voltage_concN_3,col="red")
legend(0,20,"control",fill="black")
legend(0,00,"concentration 1",fill="green")
legend(0,-20,"concentration 2",fill="orange")
legend(0,-40,"concentration 3",fill="red")

predict = function(activation_threshold,stimulation_voltage){
  activation = rep(-70,length(stimulation_voltage))
  activation[stimulation_voltage>activation_threshold] = 40
  return(activation)
}
calculate_errors = function(predicted, observed){
  total_errors = sum((observed - predicted)^2)
  return(total_errors)
}
fit_threshold = function(input_values_stimulation,input_values_response,threshold){
  predicted_values = predict(threshold,input_values_stimulation)
  fit_errors = calculate_errors(predicted_values,input_values_response)
  return(fit_errors)
}
fitted_threshold = runif(1,-100,40)
error = fit_threshold(stimulation_potential,response_voltage_control,fitted_threshold)
for(i in 1:20){
  new_threshold = runif(1,-100,40)
  new_error     = fit_threshold(stimulation_potential,response_voltage_control,new_threshold)
  if(new_error < error){
    fitted_threshold = new_threshold
    error = new_error
  }
}
fitted_threshold
error

write.csv(data.frame('fitted value'=fitted_threshold,'errors SS'=error),file = 'excitation.csv',row.names = F)


