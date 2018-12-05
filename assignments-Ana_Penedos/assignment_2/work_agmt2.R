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
  # Calculates total sum of squares between observed and predicted values.
  #
  # Args:
  #    2 numeric vectors of the same length
  #    predicted - vector containing the predicted values (numeric vector)
  #    observed - vector containing the observed values (numeric vector)
  # 
  # Returns:
  #    Total sum of squares (float) between two numeric vectors of 
  #    observed and predicted values.
  total_errors = sum((observed - predicted)^2)
  return(total_errors)
}

fit_threshold = function(input_values_stimulation,input_values_response,threshold){
  # Calculates the fit error using observed response values and response values 
  # predicted from stimulation values and threshold.
  #
  # Args:
  #    input_values_stimulation - vector of numeric values for stimulation 
  #                voltage used.
  #    input_values_response - vector of numeric values for response 
  #                measured.
  #    threshold - activation threshold for the neuron (numeric)
  #
  # Returns: 
  #    Total sum of squares between 'input_values_response' and predicted 
  #    response values from 'input_values_stimulation' and 'threshold'.
  
  # predict response values from activation threshold and stimulation values
  predicted_values = predict(threshold,input_values_stimulation)
  # calculate total sum of squares between predicted and observed values
  fit_errors = calculate_errors(predicted_values,input_values_response)
  return(fit_errors)
}

# Generate a random threshold from a uniform distribution between -100 and 40
fitted_threshold = runif(1,-100,40)
# Calculate the error between the response values predicted with the fitted threshold
error = fit_threshold(stimulation_potential,response_voltage_control,fitted_threshold)

# 20 iterations
for(i in 1:20){
  # generate a new random threshold from a uniform distribution 
  # between -100 and 40
  new_threshold = runif(1,-100,40)
  # calculate the error associated to the new threshold
  new_error     = fit_threshold(stimulation_potential,response_voltage_control,new_threshold)
  if(new_error < error){
    # keep the new threshold and error if improvement from previous
    fitted_threshold = new_threshold
    error = new_error
  }
}

fitted_threshold
error

write.csv(data.frame('fitted value'=fitted_threshold,'errors SS'=error),file = 'excitation.csv',row.names = F)
