# 2
## 1. functions
The 'predict' function takes an activation threshold and a vestor of stimulations used and produces a vector of the same length of the vector containing the voltages of stimulation used, with -70 in the positions corresponding to stimulation under the threshold and 40 in those where the stimulation voltage was above the threshold.
The 'calculate_errors' function calculates the total sum of squares (sum((observed - expected)^2)) estimating the total deviation of a vector of measured values from a vector of predicted values.
The 'fit_threshold' function takes a vector of the stimulation voltages, a vector of observed responses and threshold. It uses the 'predict' function to calculate a vector of predicted responses and the 'calculate_errors' function to determine the total sum of squares between the response values and the predicted response vector obatined.

## 2. modelling
An initial threshold is randomly generated from a uniform distribution that can take values between -100 and 40. The error is then calculated betwee

