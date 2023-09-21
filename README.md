# GPS Activity–Travel Survey Quality Postprocessing

## Overview
This repository designs a data-driven postprocessing pipeline aimed at enhancing the data quality of smartphone-based activity–travel survey data. 
The framework adopts a series of methods to systematically identify and address data quality issues, ensuring attribute completeness and logical consistency. 
The study case utilizes data collected through the [Daynamica app](https://daynamica.com/smartphone-app/).

Additionally, this repository includes visualization and analysis components tailored for paper publication. 
The associated paper has been officially published and is accessible at [(Zhang et al. 2021)](https://doi-org.ezp2.lib.umn.edu/10.1111/tgis.12865).

## Files 
The repository contains the following key files and scripts:

* `Step1-Read-Data-Formatting.R`: This script includes data reading, trajectory decoding, and data formatting processes.
* `Step2-Data-Incompleteness.R`: This script defines and detects instances of data incompleteness, offering solutions to interpolating missing trajectories.
* `Step3-Temporal-Inconsistency.R`: This script defines, detects, and addresses temporal inconsistency in the data.
* `Step4-Spatial-Inconsistency.R`: This script defines, detects, and addresses spatial inconsistency in the data.
* `Step5-Attribute-Redundancy.R`: This script defines, detects, and addresses attribute redundancy in the data.
* `Step6-Evaluation.R`: This script evaluates the effectiveness of the proposed postprocessing framework and methods by comparing raw data to processed data.
* `_Functions_.R`: This script defines several functions that can be reused throughout the postprocessing pipeline.
* `PostProcessing-Pipeline.R`: This script integrates the entire data postprocessing into a seamless workflow.
* Some example visualization plots can be accessed in the dir `/Example Visualization`

## Future Work
At present, the code serves the purpose of presenting results and visualizations for the published paper. 
In the future, there is potential to rewrite the code into reusable functions, enhancing user customization options and input flexibility.
