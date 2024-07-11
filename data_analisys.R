############################################################################
# DATA IMPORT, FILTER AND CLEANING FOR CANCER DEPRESSION ANALYSES ##########
############################################################################

# Load necessary libraries for data manipulation and visualization
library(tidyverse)    # Collection of R packages for data science
library(magrittr)     # Provides the %>% pipe operator for chaining commands
library(here)         # Constructs paths to your project's files
library(haven)        # Imports and exports data from SPSS, Stata, and SAS files
library(labelled)     # Works with labelled data
library(ggplot2)      # Creates elegant data visualizations using the grammar of graphics
library(gridExtra)    # Provides functions to arrange multiple grid-based figures on a page
library(patchwork)    # Makes it easy to combine separate ggplot2 plots into the same graphic
library(plotly)       # Creates interactive web-based graphics via plotly's JavaScript graphing library
library(gt)           # Easily create presentation-ready display tables
library(ggalluvial)   # Implements alluvial plots for categorical longitudinal data
library(officer)      # Provides tools for working with Microsoft Word and PowerPoint documents
library(flextable)    # Produces tabular reporting tables
library(caret)        # Streamlines the process for creating predictive models
library(rpart)        # Recursive partitioning for classification and regression trees
library(randomForest) # Implements Breiman's random forest algorithm for classification and regression
library(e1071)        # Provides various functions for statistical learning and support vector machines
library(parallel)     # Supports parallel computing
library(doParallel)   # Provides a parallel backend for the foreach package
library(GA)           # Genetic algorithms for optimization
library(nnet)         # Software for feed-forward neural networks with a single hidden layer
library(tidyr)        # Tools for changing the layout of your data sets
library(forcats)      # Tools for working with categorical data
library(RColorBrewer) # Provides color schemes for maps and other graphics
library(pROC)         # Produces pROC and calibration curves

# Source custom scripts
source("ddop_data.R")     # Import and clean dataset
source("ddop_utility.R")  # Utility functions for data analysis
source("ddop_plot.R")     # Plotting functions for analysis
source("cv.R")

############################################################################
# RUNNING MAIN FUNCTION ####################################################
############################################################################

rpm <- function(){
  
  dataset <- read_sav("data/dataset.sav")
  model <- double_cv(dataset)
  
}