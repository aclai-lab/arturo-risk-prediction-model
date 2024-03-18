############################################################################
# DATA IMPORT, FILTER AND CLEANING FOR CANCER DEPRESSION ANALYSES ##########
############################################################################

# Load necessary libraries for data manipulation and visualization
library(tidyverse)  # Collection of R packages for data science
library(magrittr)   # Provides the %>% pipe operator for chaining commands
library(here)       # Constructs paths to your project's files
library(haven)      # Imports and exports data from SPSS, Stata, and SAS files
library(labelled)   # Works with labelled data
library(ggplot2)
library(gridExtra)
library(patchwork)
library(plotly)
library(gt)
library(officer)
library(flextable)
source("ddop_data.R")     # Import and cleaning dataset
source("ddop_utility.R")  # Utility function for data analysis
source("ddop_plot.R")     # Plotting analysis

############################################################################
# LOAD DATASET #############################################################
############################################################################

#combined_dataset <- load_dataset()
#write_sav(combined_dataset, "data/combined_dataset.sav")

combined_dataset <- read_sav("data/combined_dataset.sav")

############################################################################
# FILTERING DATASET & MAP VARIABLE DOMAIN ##################################
############################################################################

# Filtering Dataset
filtered_dataset <- filtering_dataset(combined_dataset)

# Find variable domain
column_names <- names(filtered_dataset)

domains <- sapply(column_names, function(column_name) {
  
  column_data <- filtered_dataset[[column_name]]
  
  if (column_name == "ac012_" | column_name == "ch001_" | column_name == "ch021_") {
    get_unlabel_domain(column_data)
  } else {
    get_domain(column_data)
  }
})

# Create a dataframe mapping variables to their respective domains
domain_mapping <- tibble(
  variable = names(domains),
  domain = domains
)

current_labels<- attr(combined_dataset$hh017e, "label")
v <- sprintf("%.0f", as.numeric(combined_dataset$hh017e))
v <- as.numeric(v)
combined_dataset$hh017e <- labelled(v)
attr(combined_dataset$hh017e, "label") <- current_labels

# remove negative labels from dataset
filtered_dataset <- filtered_dataset %>% mutate(across(everything(), update_labels))

# Get list of variables type
var_type <- get_var_type(filtered_dataset, domain_mapping)
# write_sav(filtered_dataset, "filtered_dataset.sav")

############################################################################
# DATA ANALISYS ############################################################
############################################################################

# Function to plot distribution of categorials, ordianal and continues variables
pre_analysis_plot(filtered_dataset, var_type, path = "filtered_dataset/")

# NA percentage for all variable
var_na_perc <- var_na_perc(filtered_dataset)

############################################################################
# CLEANING DATASET #########################################################
############################################################################

#cant_remove <- c("ph008d", "ph084_", "ph087d", "ph066_")
#protected_vars <- unlist(lapply(cant_remove, function(prefix) {
#  grep(prefix, names(combined_dataset), value = TRUE)
#}))

secondary_var <- c("ph008d", "ac035d", "ph006d", "ph011d", "ph048d", "ph049d", "ph089d", "ph059d", "ph076_", "ph077_", "ph080d", "ph087d", "ph690d")
secondary_var <- unlist(lapply(secondary_var, function(prefix) {
  grep(prefix, names(filtered_dataset), value = TRUE)
})) 

#make_docx_table(filtered_dataset, secondary_var, "plot/secondary_var.docx")

clean_dataset <- cleaning_dataset(filtered_dataset, secondary_var, protected_var = "")
clean_domain_mapping <- domain_mapping %>%
  filter(variable %in% names(clean_dataset))

clean_var_type = get_var_type(clean_dataset, clean_domain_mapping, var_euro = NULL)
pre_analysis_plot(clean_dataset, var_type, path = "clean_dataset/")

# descriptive variable, desc_removed <- c("dn503_", "ch001_", "ch021_")
desc_var <- c("dn042_", "ep005_", "isced1997_r", "bmi2", "country", "language", "age_int", "ph012_")

make_distribution_pdf(dataset = filtered_dataset,
                      variables = desc_var,
                      var_type = var_type,
                      pdf_name = paste("desc_distribution.pdf", sep=""),
                      width = 25, 
                      height = 22, 
                      per_page = 8, 
                      layout = c(3, 4))

# predictive variable
p_var <- setdiff(names(clean_dataset), desc_var)

make_distribution_pdf(dataset = filtered_dataset,
                      variables = p_var,
                      var_type = var_type,
                      pdf_name = paste("desc_distribution.pdf", sep=""),
                      width = 25, 
                      height = 22, 
                      per_page = 8, 
                      layout = c(3, 4))

p_ds<- clean_dataset %>% select(all_of(p_var))

# variancy of predictive variable
p_variancy <- sapply(p_ds, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA)
variancy_ds <- data.frame(
  variable = names(p_variancy),
  value = p_variancy
) %>% na.omit(variancy_ds)
make_barplot(variancy_ds, "plot/variancy.pdf", xlabs = "variable", ylabs = "variancy")

# ordinal & not ordinal predictive variable
p_ord <- clean_dataset %>% select(all_of(intersect(var_ord, p_var)))
p_not_ord <- clean_dataset %>% select(all_of(intersect(var_not_ord, p_var)))

# test chi2
res_chi2 <- ipo_test(dataset = p_ds, pvar = "euro_d", type_test = "chi2")
pvalue <- res_chi2$value
res_chi2$value <- 1 - pvalue
make_barplot(res_chi2, "plot/ipo_test/chi2/all_chi2.pdf", xlabs = "variable", ylabs = "pvalue")

# test  chi2 log order
res_chi2$value <- -log10(pvalue)
make_barplot(res_chi2, "plot/ipo_test/chi2/log_all_chi2.pdf", xlabs = "variable", ylabs = "pvalue")

# test chi2 on not ordinal variable
res_chi2 <- ipo_test(dataset = p_not_ord, pvar = "euro_d", type_test = "chi2")
pvalue <- res_chi2$value
res_chi2$value <- 1 - pvalue
make_barplot(res_chi2, "plot/ipo_test/chi2/not_ord_chi2.pdf", xlabs = "variable", ylabs = "pvalue")

# test  chi2 on not ordinal varible with log order
res_chi2$value <- -log10(pvalue)
make_barplot(res_chi2, "plot/ipo_test/chi2/log_not_ord_chi2.pdf", xlabs = "variable", ylabs = "pvalue")

# test mann whitney
res_mann_whitney <- ipo_test(dataset = p_ds, pvar = "euro_d", type_test = "mann_whitney")
pvalue <- res_mann_whitney$value
res_mann_whitney$value <- 1 - pvalue
make_barplot(res_chi2, "plot/ipo_test/mw/all_mw.pdf", xlabs = "variable", ylabs = "pvalue")

# test mann whitney log order
res_mann_whitney$value <- -log10(pvalue)
make_barplot(res_chi2, "plot/ipo_test/mw/log_all_mw.pdf", xlabs = "variable", ylabs = "pvalue")

# test mann on ordinal variable
res_mann_whitney <- ipo_test(dataset = p_ord, pvar = "euro_d", type_test = "mann_whitney")
pvalue <- res_mann_whitney$pvalue
res_mann_whitney$value <- 1 - pvalue
make_barplot(res_chi2, "plot/ipo_test/mw/ord_mw.pdf", xlabs = "variable", ylabs = "pvalue")

# test mann on ordinal variable with log order
res_mann_whitney$value <- -log10(pvalue)
make_barplot(res_chi2, "plot/ipo_test/mw/log_ord_mw.pdf", xlabs = "variable", ylabs = "pvalue")
