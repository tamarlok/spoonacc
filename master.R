###############################################################################
###################### R-code associated with the paper ####################### 
### Lok T, van der Geest M, Bom RA, de Goeij P, Piersma T & Bouten W (2023) ###
######## Prey ingestion rates revealed by back-mounted accelerometers #########
############### in Eurasian spoonbills. Animal Biotelemetry. ##################
###############################################################################

# code works with R versions 3.6.0 and 4.1.3

# load required libraries
library(gdata)
library(rpart)
library(lubridate)
library(gplots)
library(reshape)
library(reshape2)
library(cpm)
library(moments)
library(plyr)
library(randomForest)
library(rpart)
library(caret)
library(RODBC)
library(RColorBrewer)
library(maptools)
library(rgdal)
library(lme4)
library(nlme)
library(MuMIn)
library(mgcv)
library(gamm4)
library(plotrix)
library(ggplot2)
library(gganimate)
library(av)
library(OpenStreetMap)
library(osmdata)
library(tidyverse)
library(sf)
library(raster)
library(dplyr)
library(png)
library(patchwork)

# Set system time to UTC/GMT
Sys.setenv(TZ="GMT") 

source("functions.R")
set.seed(3) # to be able to reproduce random processes

# (1) Data preparation
source("1_data_preparation/1_visually_check_and_select_soaring_flight_data.R")
source("1_data_preparation/2_combine_annotated_data_with_soaring_flight_data.R")
source("3_visualisation/1_plot_acceleration_data.R")

# (2) MODEL DEVELOPMENT
# exploratory data analyses for training and testing random forest models on annotated data
source("2_analyses/1_exploring_effect_clean_train_segments.R")
source("2_analyses/2_exploring_effect_max_segment_length_and_startup_value.R")
source("2_analyses/3_exploring_effect_reduced_predictor_set.R")
source("2_analyses/4_exploring_effect_down-_vs_upsampling.R")
source("2_analyses/5_exploring_effect_sampling_frequency.R")

# compare model performance when using the fixed versus flexible segmentation method, while downsampling standing data 4x in the training dataset
source("2_analyses/6_comparing_fixed_vs_flexible_segmentation.R")
source("3_visualisation/2_plot_results_model_development.R") # best models use fixed-length segments of 0.4 s or variable-length segments

# (3) MODEL APPLICATION
# apply the best models to application data (GPS and acceleration data of adult females during the breeding season of 2016-2019)
source("2_analyses/7_apply_best_models_to_application_data.R")
source("2_analyses/8_link_classified_acc_data_with_habitat.R")
source("2_analyses/9_statistical_analysis_of_prey_ingestion_rates.R")
source("3_visualisation/3_plot_results_application_data_analysis.R") 