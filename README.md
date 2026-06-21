# OMICS_PB_Cheese
Objective: Find the drivers for liking aroma of plant-based cheeses based on sensory and chemical indicators and subject level data.

We considered OMICs data for 20 different samples of cheeses and randomly assigned each sample of cheese to each test subject and had them provide sensory evaluations of these cheeses. We additionally collected some subject level data such as demographics data, inclination to plant based cheeses and preferences, preference for particular dairy cheeses etc. Our goal was to find the chemicals which should be reinjected into the particular plant-based cheese that would make someone more likely to rate that cheese more positively, not just based on chemical data but also accounting the variability introduced by personal preferences and/or biases.

Initially, 2 data files were provided, one for the OMICs (chemical) data for each sample and each of its replicates along with aroma rating provided by each test subject, and the test subject data along with ratings for each cheese they tried. This was the format of the raw data. 

I analyzed the chemical data and the subject level data and came up with an ensemble  of statistical, ML and subject-specific approaches to answer the research questions. I further used bootstrapped resampling, to help with voting in the ensemble, to make sure the results were reliable and stable due to the small n large p setting.

The project was done predominantly in R, I used rmd to write the codes and generate reports. For every file listed below you can find the rmd file along with a rendered pdf report. Finally, there's a report collating all the findings during the project and the recommended next steps. 

I would suggest going through the files in the order mentioned below.

All rights reserved.

Files:

data_exploration_cleaning.rmd : Cleaning both the OMICs (chemical) and subject-level data and creating datasets for further downstream analysis

Chemical_Data_Analysis : Exploratory analysis of OMICs (chemical) data for different plant based cheeses and their sensory outcome

subject_preference_dim_red : This file has the code for analysis of the subject level data and how that correlates with the sensory outcome(s), after data exploration, I suggest some data reduction techniques to incorporate the subject level data in order to help with the decision making process of finding chemicals that correlate positively with higher sensory ratings.

screening_chemicals : This file has the code for running the developed ensemble method, which is further imporved using bootstrapped resampling by weighting more stable methods higher than the less stable methods, and using different kinds of methods/models to arrive at the final answer. I also created some final data tables and visualisations that you can find near the end of the file to have a look at the results.

Method_Stability_Weights.csv : A table consisting of the ranks standard deviation for each method and the weight contribution for each method in the final ranking calculation.

composite_ranking_top20 : Composite ranks based on the different methods and corresponding weights for each of the methods for the selected top 20 chemicals.

ranks_each_method.png : Line plot for each of the top 20 chemicals, based on composite scores of each chemical based on the different models/methods.


