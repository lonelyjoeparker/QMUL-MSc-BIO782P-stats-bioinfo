######## "Assignment 2 - Part B"
######## Author: HAJAR SAIHI
######## Due: 14 December
######## Examiner: Dr Parker

################################################################################################################################################
######## Initial Code ##########################################################################################################################
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
write.csv(data.frame('fitted value'=fitted_threshold,'errors SS'=error),file = 'initial_excitation.csv',row.names = F)

######## Question 6 and 7a: Optimisation #######################################################################################################
response_list=list(response_voltage_control, # Create a list with all the input variables
                   response_voltage_concN_1, 
                   response_voltage_concN_2, 
                   response_voltage_concN_3)

errors=c() #Empty list - to be filled with the errors for each treatment
fitted_thresholds=c() #Empty list - to be filled with the thresholds for each treatment

# The initial optimisation only gave one 'optimal' fitted value, to further optimise this, it is better to run a loop ten times where there are 10 
# 'optimal' fitted values and then work out the average of these.

set.seed(80)
for(count in 1:10){ # Run the loop ten times.
  
  for(exp in response_list){ # Start with the experiment in the response_list - ie. control then concN1..concN3
    
    fitted_threshold = runif(1,-100,40) # Fit a threshold using a random value from a uniform distribution with a upper limit of 40 and a lower limit of -100
    error = fit_threshold(stimulation_potential,exp,fitted_threshold)  # Work out the error for this fitted threshold.
    
    new_threshold = runif(1,-100,40) # Fit a new random threshold.
    
    for(i in 1:100){ # To optimise it futher - instead of running it 20 times, run it 100 times
      new_error = fit_threshold(stimulation_potential,exp,new_threshold) # Use the new threshold to work out a new error for the new threshold. 
      
      if(new_error < error){ # Check if the new error is less than the error (from initial fitted threshold)
        fitted_threshold = new_threshold # If it is, then set the new threshold as the fitted threshold...
        error = new_error # ...and the error as the new error
        new_threshold = rnorm(1, fitted_threshold, 20) # redefine a new threshold *around* this new fitted threshold. Select a random number from a normal 
                                                       # distribution where the mean is fitted_threshold and the sd is 20.
      }
      else if(new_error >= error){ # If the new error is more than or the same as the previous error
        new_threshold = runif(1,-100,40) # Define a new threshold and start over
      }
    }
    
    errors <- c(errors,error) # Store the final error in the errors list, this will give one error for each treatment (control, conc1, conc2, conc3) 
                              # and then repeat ten times.
    
    fitted_thresholds <- c(fitted_thresholds,fitted_threshold) #Store the final fitted threshold in the fitted_thresholds list, this will give 
                              # one threshold for each treatment (control, conc1, conc2, conc3) and then repeat ten times.
    
  }
}

errors
fitted_thresholds

write.csv(data.frame('fitted value'=fitted_thresholds,'errors SS'=errors ), # Write out the CSV file
          file = 'excitation.csv',
          row.names = F)

######## Question 7b [1]: Compare Goodness of Fit ##############################################################################################
errors_gof=matrix(errors,nrow=10 ,ncol=4, byrow=TRUE)
colnames(errors_gof)=c("Control", "Conc1","Conc2","Conc3")

comp_errors=matrix(colMeans(errors_gof)) # Work out the mean error per column
rownames(comp_errors) = c("Control", "Conc 1", "Conc 2", "Conc 3") # Define row names
colnames(comp_errors) = c("Error") # Define col names

print(comp_errors) 
# This table shows how the error varies for the model, ie a larger error demonstrates that the model is not a good fit whereas a lower 
# error suggests the model fits well with the observed data.

######## Question 7b [2]: Does Nicotine Treatment Lower The Activation Threshold? #############################################################
fit_thresholds_table <- matrix(fitted_thresholds,nrow=10 ,ncol=4, byrow = TRUE) # Create a table to contain the fitted thresholds, ordered by columns.
colnames(fit_thresholds_table) <- c("Control", "Conc 1", "Conc 2", "Conc 3") # Define column names
rownames(fit_thresholds_table) <- seq(1,10) # Define row names

fit_thresholds_table # Final threshold table


# Plot a t test to see if the average thresholds in the increasing nicotine treatments are significantly different from the control treatment.
cont <- t.test(fit_thresholds_table[,1],fit_thresholds_table[,1])
cont_con1 <- t.test(fit_thresholds_table[,1],fit_thresholds_table[,2])
cont_con2 <- t.test(fit_thresholds_table[,1],fit_thresholds_table[,3])
cont_con3 <- t.test(fit_thresholds_table[,1],fit_thresholds_table[,4])

p_values <- c(cont$p.value,cont_con1$p.value,cont_con2$p.value,cont_con3$p.value) # Create a list of all the p values 

boxplot(fit_thresholds_table, ylab="Modelled Activation Threshold (mV)", # Plot a box plot of the modelled threshold for each of the treatments (control, concN1, concN2, concN3)
        xlab = "Treatment", 
        cex.lab = 0.9, 
        cex.main = 0.9,
        main = "The Effect of Nicotine Concentration on Activation Threshold")

# Generate the final matrix that has the p values of the differences - p <0.05 indicates that there is a significant difference between the increasing concentrations and the control.
final_mat <- matrix(data = p_values, nrow=4, ncol=1)
rownames(final_mat) <- c('Control', 'Control and Conc 1: ','Control and Conc 2: ','Control and Conc 3: ')
colnames(final_mat) <- 'p Value'
final_mat

######## Data/Analysis visualisation: Plotting a graph to show the differences in mean ########################################################
graph_data <- c(mean (fit_thresholds_table[,1]),mean (fit_thresholds_table[,2]), # Work out the mean for each coloumn.
                mean (fit_thresholds_table[,3]),mean (fit_thresholds_table[,4]))
graph_mat <- matrix(data = graph_data, nrow = 1, ncol = 4) # Create a matrix with the means as the data
colnames(graph_mat) <- c("Control", "Conc 1", "Conc 2", "Conc 3") # Define column names
rownames(graph_mat) <- 1 

#Plot a bar plot for the mean activation threshold per treatment
bar<- barplot(graph_mat, col = 'gray88', main = 'Mean Activation Threshold per Treatment', 
              xlab = 'Treatment', ylab = 'Activation threshold (mV)', ylim = c(-120, 1),
              border = c('firebrick1'),
              cex.main = 0.9, cex.lab = 0.9)

# Work out the SD for each column and store this into another variable
graph1_data <- c(sd(fit_thresholds_table[,1]),sd(fit_thresholds_table[,2]), 
                 sd(fit_thresholds_table[,3]),sd(fit_thresholds_table[,4]))

arrows(bar, graph_data-graph1_data, # Plot error bars, this will be the barplot data +- standard deviations for each bar plot.
       bar, graph_data+graph1_data,
       angle=90,code=3)

######## Data/Analysis visualisation: Visualising Model Precited Values vs Observed Values ####################################################
prediction_values = predict(mean(fit_thresholds_table[,1]),stimulation_potential)
plot(prediction_values~stimulation_potential, type = "l") # Plot a line plot of prediction values against stimulation potential
lines(response_voltage_control~stimulation_potential,lty = 2, col = 'black') # Plot the observed control line

prediction_values_1 = predict(mean(fit_thresholds_table[,2]),stimulation_potential)
lines(prediction_values_1~stimulation_potential, col = 'green') # Plot the model concentration 1 line
lines(response_voltage_concN_1~stimulation_potential,lty = 2, col = 'green')  # Plot the observed concentration 1 line

prediction_values_2 = predict(mean(fit_thresholds_table[,3]),stimulation_potential)
lines(prediction_values_2~stimulation_potential, col = 'orange') # Plot the model concentration 2 line
lines(response_voltage_concN_2~stimulation_potential,lty = 2, col = 'orange') # Plot the observed concentration 2 line

prediction_values_3 = predict(mean(fit_thresholds_table[,4]),stimulation_potential)
lines(prediction_values_3~stimulation_potential, col = 'red') # Plot the model concentration 3 line
lines(response_voltage_concN_3~stimulation_potential,lty = 2, col = 'red') # Plot the observed concentration 3 line

legend (5, 4, legend = c('Control', 'Conc 1', 'Conc 2', 'Conc 3'), # Define the legend
        fill = c('black', 'green','orange', 'red'))
legend (5, -35, legend = c('Model', 'Observed'), lty=1:2)

##############################################################- end -#############################################################################
