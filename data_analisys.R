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

# Set up directories using here() to ensure file paths are correct
here()  # Prints the top-level directory of your project
here("share/share data/sharew7_rel8-0-0_ALL_datasets_spss")  # Specifies the path to the data

############################################################################
# FUNCTION #################################################################
############################################################################

# Function to find variable code by variable label
find_variable_by_label <- function(data, label) {
  for (var_name in names(data)) {
    # Obtein label
    var_label <- attr(data[[var_name]], "label")
    # Label Check
    if (!is.null(var_label) && any(var_label == label)) {
      return(var_name)
    }
  }
  return(NULL) 
}

# Function to find variable label by variable code
get_label_by_variable <- function(data, variable_name) {
  # check if variable exists
  if (!variable_name %in% names(data)) {
    stop("La variabile specificata non esiste nel dataframe.")
  }
  # Obtein variable
  variable_label <- attr(data[[variable_name]], "label")
  # Check on label
  if (is.null(variable_label)) {
    warning("La variabile specificata non ha un'etichetta.")
    return(NA)
  } 
  return(variable_label)
}

get_labels_by_variable <- function(dataset, var_name) {
  
  if (!var_name %in% names(dataset)) {
    stop("The variable name provided does not exist in the dataset.")
  }
  var <- dataset[[var_name]]
  if (is.null(attr(var, "labels"))) {
    stop("The variable does not have labels attribute.")
  }
  labels <- attr(var, "labels")
  labels_df <- data.frame(value = as.numeric(labels), label = names(labels), stringsAsFactors = FALSE)
  return(labels_df)
  
}


# Function for find variable column domain
get_domain <- function(column) {
  if (is.labelled(column)){
    domain = val_labels(column)
    return(paste(domain, collapse = "/"))  
  }
  else{
    return("")
  }
}

check_domain <- function(column) {
  unique_values <- sort(unique(na.omit(column)))
  if (is.numeric(unique_values)) {
    if (length(unique_values) > 1 && all(diff(unique_values) == 1)) {
      return(paste(range(unique_values), collapse = ":")) 
    } else {
      return(paste(unique_values, collapse = "/"))
    }
  } else {
    return(paste(unique_values, collapse = "/"))
  }
}

# Function to check if variables domain is sequential or not
is_sequential <- function(domain) {
  str_detect(domain, pattern = ":")
}

# function for count and list variable occurrencies in a dataset
create_var_occ <- function(dataset, domain_mapping) {
  var_occ <- list()
  
  for (var in names(dataset)) {
    # Extract the domain for the variable
    var_domain <- filter(domain_mapping, variable == var)$domain
    # Split the domain string into individual levels
    val_domain <- strsplit(var_domain, "/")[[1]]
    # Create a factor with the specified levels and compute the table
    occ <- table(factor(dataset[[var]], levels = val_domain))
    # Store the table in the list with the variable name as the key
    var_occ[[var]] <- occ
  }
  
  return(var_occ)
}

# Function to make pdf for var distribution
make_distribution_pdf <- function(var_occ = NULL, dataset, pdf_name, width = 20, height = 17, per_page, layout, type) {
  pdf(pdf_name, width = width, height = height)
  if(type == "categorial" | type == "euro") {
    
    num_pages <- ceiling(length(var_occ) / per_page)
    
    for (page in 1:num_pages) {
      
      plots_page <- list()
      start_index <- (page - 1) * per_page + 1
      end_index <- min(page * per_page, length(var_occ))
      
      
      for (i in start_index:end_index) {
        var_name <- names(var_occ)[i]
        
        df_occ <- as.data.frame(var_occ[[var_name]])
        names(df_occ) <- c("Value", "Count")
        var_labels_df <- get_labels_by_variable(combined_dataset, var_name)
        
        if(type == "euro"){
          df_occ$ValueLabel <- with(var_labels_df, paste(value, "-", label))
          df_occ$ValueLabel <- factor(df_occ$ValueLabel, levels = df_occ$ValueLabel)
          
          p <- ggplot(df_occ, aes(x = ValueLabel, y = Count)) +
            geom_bar(stat = "identity", fill = "skyblue", color = "black") +
            labs(x = "", y = "") +
            theme_minimal() +
            theme(legend.position = "none")
          
          plots_page[[i - start_index + 1]] <- p
        }
        else if (type == "categorial"){
          
          if(var_name != "more2y_c" & var_name != "language" & var_name != "country"){
          var_labels_df$label <- substr(var_labels_df$label, 1, 25)
          var_labels_df$legend_label <- paste(var_labels_df$value, "->", var_labels_df$label)
          df_occ$Value <- factor(df_occ$Value, levels = var_labels_df$value)
          p <- ggplot(df_occ, aes(x = Value, y = Count, fill = Value)) +
            geom_bar(stat = "identity", color = "black") +
            scale_fill_manual(values = rep("skyblue", nrow(var_labels_df)), 
                              labels = var_labels_df$legend_label, 
                              name = "Labels") +
            labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name)), y = "") +
            theme_minimal() +
            guides(fill = guide_legend(title = "Value - Label"))
          }else{
            p <- ggplot(df_occ, aes(x = Value, y = Count)) +
              geom_bar(stat = "identity", fill = "skyblue", color = "black") +
              labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name)), y = "") +
              theme_minimal()
          }
        }
        
        
        plots_page[[i - start_index + 1]] <- p
      }
      
      
      do.call(grid.arrange, c(plots_page, ncol = layout[1], nrow = layout[2], padding = unit(0.5, "lines")))
    }
  }
  else{
    
    plot_list <- list()
    
    for (var in names(dataset)) {
      var_domain <- filter(domain_mapping, variable == var)$domain
      val_domain <- unlist(strsplit(var_domain, "/"))
      val_domain <- as.numeric(val_domain[val_domain != ""])
      
      val_continue <- dataset %>%
        filter(!get(var) %in% val_domain)
      
      p_d <- ggplot(val_continue, aes(x = !!sym(var))) +
        geom_density(aes(y = ..count..), fill = "skyblue", alpha = 0.5) +
        labs(x = "", y = "") +
        theme_minimal()  +
        theme(plot.margin = margin(0, 0, 0, 0)) 
      
      val_categ <- dataset %>%
        filter(get(var) %in% val_domain)
      
      occ <- table(factor(val_categ[[var]], levels = val_domain))
      df_occ <- as.data.frame(occ)
      names(df_occ) <- c("Value", "Count")
      
      p_ist <- ggplot(df_occ, aes(x = Value, y = Count)) +
        geom_bar(stat = "identity", fill = "red", color = "black", alpha = 0.5) +
        labs(x = "", y = "") +
        theme_minimal()  +
        theme(plot.margin = margin(0, 0, 0, 0)) 
      
      
      combined_plot <- (p_d | p_ist) + 
        plot_annotation(tag_levels = 'A') +
        plot_layout(guides = 'collect') &
        theme(
          plot.tag.position = "bottom",  
          plot.tag = element_text(size = 15, face = "bold"), 
        )
      
      tag_label <- ggplot() + 
        theme_void() + 
        theme(plot.margin = margin(t = 1, b = 1, l = 10, r = 10)) +
        annotate("text", x = 0.5, y = 0.5, label = paste(var, "\n", get_label_by_variable(dataset, var)), size = 4)
      
      
      combined_plot <- combined_plot / tag_label + 
        plot_layout(heights = c(1, 0.2))
      
      plot_list[[length(plot_list) + 1]] <- combined_plot
      
      if (length(plot_list) == per_page) {
        combined_plots <- wrap_plots(plot_list, ncol = layout[1])
        print(combined_plots)
        plot_list <- list()  
      }
    }
    
    
    if (length(plot_list) > 0) {
      combined_plots <- wrap_plots(plot_list, ncol = layout[1])
      print(combined_plots)
    }
  }
  
  dev.off()
}

# Function to calculate basic statistics, variance, and standard deviation for each sequential variable
calculate_statistics <- function(variables_df, data_df) {
  variables_df %>%
    pull(variable) %>%
    map_df(~{
      selected_col <- data_df[[.x]]
      
      # Check for empty column or columns with only NA values
      if (all(is.na(selected_col))) {
        return(tibble(
          Variable = .x,
          Min = NA,
          `1st Qu.` = NA,
          Median = NA,
          Mean = NA,
          `3rd Qu.` = NA,
          Max = NA,
          Variance = NA,
          St_Dev = NA
        ))
      }
      
      # Calculate basic statistics using the summary function
      base_stats <- summary(selected_col)
      
      # Calculate variance and standard deviation, excluding NA values
      variance <- var(selected_col, na.rm = TRUE)
      st_dev <- sd(selected_col, na.rm = TRUE)
      
      # Create a tibble with all calculated statistics
      tibble(
        Variable = .x,
        Min = base_stats[1],
        `1st Qu.` = base_stats[2],
        Median = base_stats[3],
        Mean = base_stats[4],
        `3rd Qu.` = base_stats[5],
        Max = base_stats[6],
        Variance = variance,
        St_Dev = st_dev
      )
    })
}

############################################################################
# SHARE DATASET FILTER #####################################################
############################################################################

# Define filters for each sub-dataset indicating which variables to keep
#sharelife_hs_filter = c("sl_ac002d1-10", "sl_hs045d3") not exists?
#variable "co201_", "ac035d6", "hc002_", "br002_", "br023_" not found in w7
filters <- list(
  cv_r_filter = c("age_int", "hhsize"),
  dn_filter = c("dn042_", "dn503_", "dn041_", "dn014_", "dn034_"),
  iv_filter = c("iv009_"),
  hh_filter = c("hh022_", "hh025_", "hh017e"),
  sp_filter = c("sp002_", "sp008_"),
  ch_filter = c("ch001_", "ch021_"),
  ep_filter = c("ep005_"),
  co_filter = c("co007_"),
  ac_filter = c("ac035d1", "ac035d4", "ac035d5", "ac035d7", "ac035d8", "ac035d9", "ac035d10","ac035dno", "ac012_", "ac014_", "ac015_", "ac016_", "ac017_", "ac018_", "ac019_", "ac020_", "ac021_", "ac022_", "ac022_", "ac023_", "ac024_", "ac025_"),
  it_filter = c("it003_"),
  gv_health_filter = c("euro1", "euro2", "euro3", "euro4", "euro5", "euro6", "euro7", "euro8", "euro9", "euro10", "euro11", "euro12", "bmi2", "phactiv"),
  ph_filter = c("ph011d10", "ph006d16", "ph006d1", "ph072_1", "ph006d2", "ph006d4", "ph072_2", "ph006d5", "ph006d6", "ph006d12", "ph006d14", "ph072_3", "ph072_4", "ph011d1", "ph011d11", "ph011d13", "ph011d14", "ph011d15", "ph011d2", "ph011d3", "ph011d4", "ph011d6", "ph011d7", "ph011d8", "ph011d9", "ph011dno", "ph011dot", "ph011d15", "ph003_", "ph006d3", "ph046_", "ph043_", "ph044_", "ph092_", "ph065_", "ph048d1", "ph048d2", "ph048d3", "ph048d4", "ph048d5", "ph048d6", "ph048d7", "ph048d8", "ph048d9", "ph048d10", "ph048dno", "ph049d1", "ph049d2", "ph049d3", "ph049d4", "ph049d5", "ph049d6", "ph049d7", "ph049d8", "ph049d9", "ph049d10", "ph049d11", "ph049d12", "ph049d13", "ph049dno", "ph089d1", "ph089d2", "ph089d3", "ph084_", "ph089d4", "ph006d10", "ph071_3"),
  mh_filter = c("mh037_"),
  hc_filter = c( "hc012_", "hc029_", "hc114_", "hc115_", "hc125_"),
  br_filter = c("br015_", "br016_")
)

# Share dataset names
data_names = c("cv_r", "dn", "iv", "hh", "sp", "ch", "ep", "co", "ac", "it", "gv_health", "ph", "mh", "hc", "br")

############################################################################
# DATA STORE & FILTER ######################################################
############################################################################

# Initialize lists to store the data for each wave, IDs of patients diagnosed with cancer, and subsets of data for cancer patients
waves <- list()
id_cancer <- list()
cancer_data <- list()
combined_dataset <- tibble()

# Loop through each wave (5 to 7) to process the data
for (wave in 7:5) {
  
  waves_ds <- list()# Initialize a list to store sub-datasets for the current wave
  
  # Read and filter datasets for the current wave
  for (name in data_names){
    ds_path <- paste0("share data/sharew", wave, "_rel8-0-0_ALL_datasets_spss/sharew", wave, "_rel8-0-0_", name, ".sav")
    
    # Check if the dataset file exists
    if (file.exists(ds_path)) {
      dataset <- read_sav(ds_path)
      filter_name <- paste0(name, "_filter")
      
      if (name == "ph") {
        waves_ds[[name]] <- dataset
      } else {
        # Apply the relevant filter if it exists
        if (filter_name %in% names(filters)) {
          columns_to_select <- c("mergeid", filters[[filter_name]])
          filtered_ds <- dataset %>% select(all_of(columns_to_select))
          waves_ds[[name]] <- filtered_ds
        }
      }
    }
  }
  
  # Combine all sub-datasets for the current wave
  combined_wave <- reduce(waves_ds, full_join, by = "mergeid")
  
  # Filter for cancer diagnosis
  # ph071_3 == 1 | ph072_3 == 1
  cancer_filter <- combined_wave %>% filter(ph006d10 == 1)
  cancer_ids <- cancer_filter %>% pull(mergeid)
  
  # Create a subset with only cancer patients and their wave number
  cancer_subset <- combined_wave %>% filter(mergeid %in% cancer_ids)
  
  # For the first wave, include all cancer patients
  cancer_subset <- cancer_subset %>% mutate(wave = wave)
  if (wave == 7) {
    combined_dataset <- cancer_subset
  } else {
    # For subsequent waves, include only new cancer patients not already in the dataset
    new_participants <- anti_join(cancer_subset, combined_dataset, by = "mergeid")
    combined_dataset <- bind_rows(combined_dataset, new_participants)
  }
  
  # Store processed data for the current wave
  waves[[paste0("wave", wave)]] <- cancer_subset
  id_cancer[[paste0("w", wave, "_id_cancer")]] <- cancer_ids
  cancer_data[[paste0("w", wave, "_cancer")]] <- cancer_subset
}

############################################################################
# DATASET MANIPULATION #####################################################
############################################################################

# Find variable domain without 'mergeid' and 'wave'
domains <- sapply(combined_dataset[setdiff(names(combined_dataset), c("mergeid", "wave"))], get_domain)

# Create a dataframe mapping variables to their respective domains
domain_mapping <- tibble(
  variable = names(domains),
  domain = domains
)

# new variable 2 more years with cancer
combined_dataset <- combined_dataset %>%
  mutate(`more2y_c` = case_when(
    ph072_3 == 1 ~ "yes",
    ph072_3 == 5 ~ "no",
    TRUE ~ "dk" 
  )) %>%
  mutate(more2y_c = labelled(more2y_c, labels = c(
    "2 or more years with cancer" = "yes",
    "Less than 2 years with cancer" = "no",
    "Don't know" = "dk"
  )))
var_label(combined_dataset$more2y_c) <- "Two or more year with cancer"

adapt_variable <- domain_mapping %>%
  filter(domain == "-2/-1/1/2/3/4/5/97" | domain == "-2/-1/1/2/3/97") %>%
  pull(variable)

for(var in adapt_variable) {
  combined_dataset[[var]] <- recode( combined_dataset[[var]], `97` = 6)
  current_labels <- attr(combined_dataset[[var]], "labels")
  new_labels <- current_labels[names(current_labels) != "97"]
  new_labels["Other"] <- "6"
  attr( combined_dataset[[var]], "labels") <- new_labels
  
}

# Find variable domain without 'mergeid' and 'wave'
domains <- sapply(combined_dataset[setdiff(names(combined_dataset), c("mergeid", "wave"))], get_domain)

# Create a dataframe mapping variables to their respective domains
domain_mapping <- tibble(
  variable = names(domains),
  domain = domains
)

############################################################################
# DATA ANALISYS ############################################################
############################################################################

var_filter <- c("mergeid", "wave", "hhid6", "mergeidp6", "coupleid6", "hhid5", "mergeidp5", "coupleid5", "hhid7", "mergeidp7", "coupleid7", "hhsize", "ph074_4", "ph074_2", "ph071_4")

# continue variables
var_continue <- domain_mapping %>%
  filter(domain %in% c("-9/-2/-1", "-2/-1", "-3/-2/-1", "-9999992/-9999991")) %>%
  pull(variable)
var_continue <- setdiff(var_continue, var_filter)
ds_continue <- combined_dataset %>% select(all_of(var_continue))

# euro variables
var_euro <- c("euro1", "euro2", "euro3", "euro4", "euro5", "euro6", "euro7", "euro8", "euro9", "euro10", "euro11", "euro12")
ds_euro <- combined_dataset %>% select(all_of(var_euro))

# categorial variables
var_categ <- setdiff(names(combined_dataset), c(var_continue, var_filter, var_euro))
ds_categ <- combined_dataset %>% select(all_of(var_categ))

var_occ <- list()
for (var in names(ds_categ)) {
  var_domain <- filter(domain_mapping, variable == var)$domain
  val_domain <- unlist(strsplit(var_domain, "/"))
  occ <- table(factor(ds_categ[[var]], levels = val_domain))
  var_occ[[var]] <- occ
}

# categorial variable occurency
var_occ <- create_var_occ(ds_categ, domain_mapping)

# categorial distribution hist
make_distribution_pdf(var_occ = var_occ, 
                      dataset = combined_dataset, 
                      pdf_name = "categorial_distribution.pdf", 
                      width = 25, 
                      height = 22, 
                      per_page = 12, 
                      layout = c(3, 4),
                      type = "categorial")

# euro variable occurency
var_occ <- create_var_occ(ds_euro, domain_mapping)

# euro distribution hist
make_distribution_pdf(var_occ = var_occ, 
                      dataset = combined_dataset, 
                      pdf_name = "euro_distribution.pdf", 
                      width = 20, 
                      height = 17, 
                      per_page = 12, 
                      layout = c(3, 4),
                      type = "euro")

# continue distribution hist
make_distribution_pdf(dataset = ds_continue, 
                      pdf_name = "continue_distribution.pdf", 
                      width = 13, 
                      height = 8.5, 
                      per_page = 4, 
                      layout = c(2, 2),
                      type = "continue")

# Calculate basic statistics, variance, and standard deviation for each sequential variable
summary_stats <- calculate_statistics(seq_domain, combined_dataset)

m <- function(var_occ = NULL, dataset, pdf_name, width = 20, height = 17, per_page, layout, type) {
  pdf(pdf_name, width = width, height = height)
  
  num_pages <- ceiling(length(var_occ) / per_page)
  
  for (page in 1:num_pages) {
    
    plots_page <- list()
    start_index <- (page - 1) * per_page + 1
    end_index <- min(page * per_page, length(var_occ))
    
    
    for (i in start_index:end_index) {
      var_name <- names(var_occ)[i]
      df_occ <- as.data.frame(var_occ[[var_name]])
      names(df_occ) <- c("Value", "Count")
      
      if(var_name != "country" & var_name != "language" & var_name != "more2y_c"){
        var_labels_df <- get_labels_by_variable(combined_dataset, var_name)
        var_labels_df$label <- substr(var_labels_df$label, 1, 25)
        var_labels_df$legend_label <- paste(var_labels_df$value, "->", var_labels_df$label)
        df_occ$Value <- factor(df_occ$Value, levels = var_labels_df$value)
      p <- ggplot(df_occ, aes(x = Value, y = Count, fill = Value)) +
        geom_bar(stat = "identity", color = "black") +
        scale_fill_manual(values = rep("skyblue", nrow(var_labels_df)), 
                          labels = var_labels_df$legend_label, 
                          name = "Labels") +
        labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name)), y = "") +
        theme_minimal() +
        guides(fill = guide_legend(title = "Value - Label"))}
      else{
        p <- ggplot(df_occ, aes(x = Value, y = Count)) +
          geom_bar(stat = "identity", fill = "skyblue", color = "black") +
          labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name)), y = "") +
          theme_minimal()
      }
      
      plots_page[[i - start_index + 1]] <- p
      
    }
    
    
    do.call(grid.arrange, c(plots_page, ncol = layout[1], nrow = layout[2], padding = unit(0.5, "lines")))
  }
  
  dev.off()
}



