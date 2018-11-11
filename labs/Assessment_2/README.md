# BIO782P Statistics and Bioinformatics Assignment 2018 (2 of 2)

## Due: 17:00 Friday 14 December 2018

## Introduction

The second assessment mainly refers to material covered in the second week of the course. There are two parts, **A** and **B**. These will test your code comprehension and big-data analysis skills. Bonuses are available. See the .html or .pdf in this directory for details.

## Part A

To determine the effect of increasing nicotine concentration on the nervous system, action potentials (in mV) were measured in a series of giant squid axons infused with nicotine at various concentrations. For each series, the axon was stimulated with increasing voltages, and the peak response voltage within the following 3ms recorded. The experiment was repeated with differing concentrations of nicotine.

A colleague has written some R code to analyse this behaviour by fitting a model of excitation to the data. Unfortunately she has left the lab for another study, and her code is not very well documented.

With reference to the code above, answer the following questions (you may wish to answer separately, in comments to the code, or both):

 1) Describe what the inputs, outputs and role for each of the functions `predict`, `calculate_errors` and `fit_threshold` are.

 2) How is the data modelled?

 3) How many degrees of freedom are present in (a) the model, and (b) the residuals?

 4) Describe what happens when the code is executed, in statistical terms.

 5) How is the model fit to the data optimised?

 6) How could you improve the optimisation?

 7) How would you modify the code to:
    a) fit the data given under the three experimental nicotine treatments and
    b) compare the goodness of fit amongst these fitted models to determine whether each nicotine treatment lowers the activation threshold, compared to the control treatment
    c) what is the null hypothesis in this case?


## Part B

We have modified your code so that it prints each fit out to a logfile as the optimisation progresses. The logfiles are called `fit_01`, `fit_02`... etc and print out each time optimisation loop runs. The final `fitted_threshold` and `errors` will be written to another .csv, called `final.csv`. We have also modified the code to pass a user-specified random seed to the script for setting the initial value for `fitted_threshold`. Pseudocode for a Docker container to run that code is given.

 1) How would we use this code to run our analysis on a grid? Outline what steps we should take.

 2) How would you rewrite this jobfile to achieve the following:
     - Move the final outputs to a directory (`../output`)?
     - Change random seed (set with the `-p [some integer]` argument)?
     - Run as an array job, with 42 replicates?
