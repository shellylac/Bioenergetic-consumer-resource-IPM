# Bioenergetic-consumer-resource-IPM

This code runs the bioenergetics consumer-resource IPM described in “Investigating population responses to environmental change using a mechanistic integral projection model: elk population dynamics in a seasonal environment” submitted to American Naturalist by Lachish et al., 2019.

There are six scripts included. 

The baseline model is run and the required output is saved by running the script called “ModelRunBaselineSimulationCode_part3.R”. This script calls the following scripts in this order:
(1)	ModelSetUpCode_part1.R (loads the required packages and defines all the parameters and functions of the model)
(2)	BaselineSeasonsCode.R (defines the baseline season)
(3)	MakeInitialPopVector.R (makes and saves the initial population vector as input to the model)
(4)	ModelIterationsCode_part2.R (iterates the model forward in time)

The script CreateSeasonalVariants.R modifies the baseline seasons according to the argument specified and the type of season required for the seasonality analyses. 
