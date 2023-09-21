#load library-----
library(dplyr)
library(lubridate)
library(gepaf)
library(factoextra)
library(cluster)
library(sf)
library(raster)
library(spData)
library( leaflet )
library(leaflet.extras)
library( magrittr )
library(fitdistrplus)
library(gamlss)
library(gamlss.mx)
library(gamlss.dist)
library(ggplot2)
library(wesanderson)

source('_Functions_.R')

#Postprocessing Workflow-----------
#Read raw activity-travel survey
#Decode GPS trajectory data using 'gepaf' packages
#Perform basic data formatting
source('Step1-Read-Data-Formatting.R')

#Define data incompleteness and inconsistency issues, make sure:
##1) Each episode contains all the necessary spatial, temporal, and thematic attributes
##2) Any two consecutive episodes show continuity in space and time.

#For detected problematic episode, follow interpolation framework below:
##1) Temporal Classification (Duration): mixture model classification
##2) Spatio-Temporal clustering (Distance-Speed): a few candidate clustering methods
##3) Thematic Visualization: Transition Maxtrix
##4) Label SpatioTemporal-Thematic (ST-T) groups and decide interpolation rule

source('Step2-Data-Completeness.R')
source('Step3-Temporal-Inconsistency.R')
source('Step4-Spatial-Inconsistency.R')
source('Step5-Attribute-Redundancy.R')

#Evaluation our post-processing framework by comparing processed data with raw data
source('Step6-Evaluation.R')