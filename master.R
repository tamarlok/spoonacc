# code works with R version 3.6.0 and 4.1.3
rm(list=ls()) # clear workspace

# libraries for data preparation:
library(gdata)
library(rpart)
library(lubridate)
library(gplots)
# additional libraries for data analysis:
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
library(plotrix)
# library for plotting (data on) maps:
library(OpenStreetMap)
library(osmdata)
library(tidyverse)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
# to load and plot images:
library(png)
library(patchwork)

# in case of different functions with the same name used in different packages, call the function as package::function 

### Set system time to UTC/GMT
Sys.setenv(TZ="GMT") 
###

source("functions.R")
set.seed(3) # to be able to reproduce random processes (though the order of running the different codes should then be exactly the same)

# data preparation and visualization
source("1_data_preparation/1_visually_check_and_select_soaring_flight_data.R")
source("1_data_preparation/2_combine_annotated_data_with_soaring_flight_data.R")
source("3_visualisation/1_plot_acceleration_data.R")

# exploratory data analyses for training and testing random forest models on annotated data
source("2_analyses/1_exploring_effect_clean_train_segments.R") # shows that clean segments do not improve model performance
save.image("data/tmp/Results.effect.clean.train.segments.RData")
source("2_analyses/2_exploring_effect_max_segment_length_and_startup_value.R") 
save.image("data/tmp/Results.effect.max.segment.length.and.startup.value.RData")
source("2_analyses/3_exploring_effect_reduced_predictor_set.R")
save.image("data/tmp/Results.effect.reduced.predictor.set.RData")
source("2_analyses/4_exploring_effect_down-_vs_upsampling.R")
save.image("data/tmp/Results.effect.down.up.sampling.RData") 
load("data/tmp/Results.effect.down.up.sampling.RData")
source("2_analyses/5_exploring_effect_sampling_frequency.R")
save.image("data/tmp/Results.effect.sampling.frequency.RData")
# end of exploratory analyses

# comparing model performance when using the fixed versus flexible segmentation method, while downsampling standing data 4x in the training dataset
source("2_analyses/6_comparing_fixed_vs_flexible_segmentation.R")
save.image("data/tmp/Results.fixed.vs.flexible.segmentation.RData") 
source("3_visualisation/2_plot_results_model_development.R")
# best models use fixed-length segments of 0.4 s or flexible-length segments

# apply the best models to application data (of adult females during the breeding season of 2016-2019)
source("1_data_preparation/3_load_and_process_application_data.R")
source("2_analyses/7_apply_best_models_to_application_data.R")
save.image("data/tmp/application.predicted.behaviour.0124.RData") 
source("2_analyses/8_link_classified_acc_data_with_habitat.R")
save.image("data/tmp/application.data.linked.to.habitat.RData")
#load("data/tmp/application.data.linked.to.habitat.RData")
source("2_analyses/9_statistical_analysis_of_prey_ingestion_rates.R")
source("3_visualisation/3_comparing_results_fixed_flexible_segmentation_on_application_data.R") 
source("3_visualisation/4_plot_spatial_distribution_of_foraging_habitats.R")