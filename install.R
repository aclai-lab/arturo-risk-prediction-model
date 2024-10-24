
packages <- c(
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
  "pROC",          # Produces pROC and calibration curves
  "rpart.plot",
  "rattle"
)

install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
}
invisible(sapply(packages, install_if_missing))
lapply(packages, library, character.only = TRUE)
 write.csv(results_fwfs_bl_under$all_predictions, "predictions/balance/predictions_fwfs_bl_under.csv")