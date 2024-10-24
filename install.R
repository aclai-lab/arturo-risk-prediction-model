install.packages(c(
  "tidyverse",    # Collection of R packages for data science
  "magrittr",     # Provides the %>% pipe operator for chaining commands
  "here",         # Constructs paths to your project's files
  "haven",        # Imports and exports data from SPSS, Stata, and SAS files
  "labelled",     # Works with labelled data
  "ggplot2",      # Creates elegant data visualizations using the grammar of graphics
  "gridExtra",    # Provides functions to arrange multiple grid-based figures on a page
  "patchwork",    # Makes it easy to combine separate ggplot2 plots into the same graphic
  "plotly",       # Creates interactive web-based graphics via plotly's JavaScript graphing library
  "gt",           # Easily create presentation-ready display tables
  "ggalluvial",   # Implements alluvial plots for categorical longitudinal data
  "officer",      # Provides tools for working with Microsoft Word and PowerPoint documents
  "flextable",    # Produces tabular reporting tables
  "caret",        # Streamlines the process for creating predictive models
  "rpart",        # Recursive partitioning for classification and regression trees
  "randomForest", # Implements Breiman's random forest algorithm for classification and regression
  "e1071",        # Provides various functions for statistical learning and support vector machines
  "parallel",     # Supports parallel computing
  "doParallel",   # Provides a parallel backend for the foreach package
  "GA",           # Genetic algorithms for optimization
  "nnet",         # Software for feed-forward neural networks with a single hidden layer
  "tidyr",        # Tools for changing the layout of your data sets
  "forcats",      # Tools for working with categorical data
  "RColorBrewer", # Provides color schemes for maps and other graphics
  "pROC",         # Produces pROC and calibration curves
  "rpart.plot",   # Plots rpart models in a more readable format
  "rattle"        # Graphical user interface for data mining
))

library(tidyverse)   # Collection of R packages for data science
library(magrittr)    # Provides the %>% pipe operator for chaining commands
library(here)        # Constructs paths to your project's files
library(haven)       # Imports and exports data from SPSS, Stata, and SAS files
library(labelled)    # Works with labelled data
library(ggplot2)     # Creates elegant data visualizations using the grammar of graphics
library(gridExtra)   # Provides functions to arrange multiple grid-based figures on a page
library(patchwork)   # Combines separate ggplot2 plots into the same graphic
library(plotly)      # Creates interactive web-based graphics
library(gt)          # Creates presentation-ready display tables
library(ggalluvial)  # Implements alluvial plots for categorical data
library(officer)     # Tools for working with Microsoft Word and PowerPoint documents
library(flextable)   # Produces tabular reporting tables
library(caret)       # Streamlines the process for creating predictive models
library(rpart)       # Recursive partitioning for classification and regression trees
library(randomForest) # Implements Breiman's random forest algorithm
library(e1071)       # Functions for statistical learning and support vector machines
library(parallel)    # Supports parallel computing
library(doParallel)  # Parallel backend for the foreach package
library(GA)          # Genetic algorithms for optimization
library(nnet)        # Software for feed-forward neural networks
library(tidyr)       # Tools for reshaping your data sets
library(forcats)     # Tools for working with categorical data
library(RColorBrewer) # Provides color schemes for maps and other graphics
library(pROC)        # Produces pROC and calibration curves
library(rpart.plot)  # Plots rpart models in a more readable format
library(rattle)      # Graphical user interface for data mining
library(tidyjson)