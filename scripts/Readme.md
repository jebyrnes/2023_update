# Scripts for Pérez-Matus, A., Micheli, F., Konar, B., Shears, N., Low, N., Okamoto, D., Wernberg, T., Krumhansl, K., Ling, S., Kinsgford, M., Navarrete-Fernandez, T.,  Ruz, C., and Byrnes, J. 2024. Kelp forests as nursery and foundational habitat for reef fish. Ecology.

## Scripts

In order to replicate analysis from the paper, code in this scripts folder is enumerated in the order it should be run. We have tried to provide brief overviews and comment our code in order to facilitate understanding of precisely what we did. 

The scripts are as follows


### 0_Clean_data.R

This script takes the raw data entered by the co-authors for the meta-analysis and calculates meta-analytic metrics, merges it with information on kelp species, and adds geospatial information on the marine ecoregions of the world. The script was developed with many checks along the way to ensure proper calculations were taking place. This generates all the derived data files in the `data` folder.

### 1_maps.R
Creates maps of where each sample for each measurement was taken. Outputs to `figures` folder.  

### 2_paper_info.R
Calculates summary statistics for the literature mining, as well as outputs number of publications per year figure.  

### 3_total_analysis_figures.R
Meta-analysis and accompanying figures of the effect of kelp on fish total abundance.

### 4_richness_analysis_figures.R
Meta-analysis and accompanying figures of the effect of kelp on fish community species richness.

### 5_scale_analysis.R
Meta-analysis and accompanying figures of the effect of kelp experimental removal size or total area sampled altered the results of the total fish abundance or fish community richness results.

### 6_adult_trophic_bodysize_analysis.R
Meta-analysis and accompanying figures of the effect of kelp on individual fish species abundances split by trophic level and body size.

### 7_juvenile_analysis.R
Meta-analysis and accompanying figures of the effect of kelp on juvenile fish total abundance.

### make_three_panel_plot.R
Function for each data set that takes data and a fit model to make a plot showing effect sizes split by experimental/observational data and single versus multi-stype systems. Used by most analysis files above.

### open_all_data_for_analysis.R
Script that loads all data files and applies any additional cleaning. Better to have in one place than to have to copy-paste the same code block in each file.