# Data and Scripts

This repository contains the data and code associated with the manuscript: 

<b>The epidemiological impact, costs, and cost-effectiveness of implementing household contact investigation with tuberculosis preventive treatment in Nepal: a model-based analysis</b>

Rajan Paudel<sup>1</sup>, Anchal Thapa<sup>1</sup>, Suvesh Shrestha<sup>2</sup>, Kunchok Dorjee<sup>3</sup>, Raghu Dhital<sup>1</sup>, David Dowdy<sup>4</sup>, Maxine Caws<sup>1,5</sup>, Sourya Shrestha<sup>4</sup>

<sup>1</sup>Birat Nepal Medical Trust, Kathmandu, Nepal.

<sup>2</sup>School of Epidemiology and Public Health, University of Ottawa, Ottawa, Canada.

<sup>3</sup>Johns Hopkins School of Medicine, Baltimore, US.

<sup>4</sup>Department of Epidemiology, Johns Hopkins School of Public Health, Baltimore, US.

<sup>5</sup>Department of Clinical Sciences, Liverpool School of Tropical Medicine, Liverpool, UK.

## Data Description
par_range: Range used to obtain distribution of each parameters for simulation. <br>
cascade_for_model: Intervention cascade for both districts developed based on the pilot implementation data <br>
cal_targets_district.csv: Calibration targets for named district <br>
costing_parms_district.csv: Parameters used for cost-effectiveness analysis for each district <br>

## Code Description
01_model_setup: Setting up model to simulate TB transmission as well as scale-up scenario <br>
02_parameters_sampling: Generating distribution of parameters using provided range <br>
03_calibration_setup: Setting up calibration and running until transience for 100 years and 22 years after transience (from 2000 to 2022) <br>
04_selection_from_calibration: Selection of the simulations that fall within the calibration targets for each district. Also includes scripts to plot results <br>
05_intervention_sim: Simulating intervention scenarios including no intervention for comparison. Calculation of overall and yearwise cases and deaths averted and plots <br>
06_cost_effectiveness: Setup for and results from cost-effectiveness analysis. <br>
07_scale_up: Simulating scale-up scenario described in the manuscript using the scale-up model setup up in 01_model_setup. <br>
08_sensitivity_analysis: Sensitivity analysis, willgness to pay analysis and cases and deaths averted for the scale-up scenario including plots <br>

