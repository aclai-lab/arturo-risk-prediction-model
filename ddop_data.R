
load_dataset <- function(
filters = list(
  cv_r_filter = c("age_int", "hhsize"),
  dn_filter = c("dn042_", "dn503_", "dn014_", "dn034_"),
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
  br_filter = c("br015_", "br016_"),
  gv_isced_filter = c("isced1997_r") ),
data_names = c("cv_r", "dn", "iv", "hh", "sp", "ch", "ep", "co", "ac", "it", "gv_health", "ph", "mh", "hc", "br", "gv_isced")
){
  
  # Initialize lists to store the data for each wave, IDs of patients diagnosed with cancer, and subsets of data for cancer patients
  waves <- list()
  id_cancer <- list()
  cancer_data <- list()
  complete_ds <- tibble()
  
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
      complete_ds <- cancer_subset
    } else {
      # For subsequent waves, include only new cancer patients not already in the dataset
      new_participants <- anti_join(cancer_subset, complete_ds, by = "mergeid")
      complete_ds <- bind_rows(complete_ds, new_participants)
    }
    
    # Store processed data for the current wave
    waves[[paste0("wave", wave)]] <- cancer_subset
    id_cancer[[paste0("w", wave, "_id_cancer")]] <- cancer_ids
    cancer_data[[paste0("w", wave, "_cancer")]] <- cancer_subset
  }
  combined_dataset <- complete_ds
  return(combined_dataset)
}


filtering_dataset <- function(
    combined_dataset,
    var_euro = c("euro1", "euro2", "euro3", "euro4", "euro5", "euro6", "euro7", "euro8", "euro9", "euro10", "euro11", "euro12")
    ){
  
  # Negative value to NA
  combined_dataset <- combined_dataset %>%
    mutate(across(where(~ is.numeric(.) || is.labelled(.)), ~ replace(., . %in% c(-9999992, -9999991, -1, -2, -3, -9), NA)))
  
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
      ph072_3 == 1 ~ 1,
      ph072_3 == 5 ~ 0,
      TRUE ~ 2 
    )) %>%
    mutate(more2y_c = labelled(more2y_c, labels = c(
      "2 or more years with cancer" = 1,
      "Less than 2 years with cancer" = 0,
      "Don't know" = 2
    )))
  var_label(combined_dataset$more2y_c) <- "Two or more year with cancer"
  
  adapt_variable <- domain_mapping %>%
    filter(domain == "-2/-1/1/2/3/4/5/97") %>%
    pull(variable)
  
  for(var in adapt_variable) {
    combined_dataset[[var]] <- recode( combined_dataset[[var]], `97` = 6)
    current_labels <- attr(combined_dataset[[var]], "labels")
    new_labels <- current_labels[names(current_labels) != 97]
    new_labels[names(current_labels[8])] <- 6
    attr(combined_dataset[[var]], "labels") <- new_labels
  }
  
  adapt_variable <- domain_mapping %>%
    filter(domain == "-2/-1/0/1/2/3/4/5/6/95/97") %>%
    pull(variable)
  
  for(var in adapt_variable) {
    combined_dataset[[var]] <- recode( combined_dataset[[var]], `95` = 7, `97` = 8)
    current_labels <- attr(combined_dataset[[var]], "labels")
    new_labels <- current_labels[!names(current_labels) %in% c(95,97)]
    new_labels[names(current_labels[10])] <- 7
    new_labels[names(current_labels[11])] <- 8
    attr(combined_dataset[[var]], "labels") <- new_labels
  }
  
  adapt_variable <- domain_mapping %>%
    filter(domain == "-2/-1/1/3/5") %>%
    pull(variable)
  
  for(var in adapt_variable) {
    combined_dataset[[var]] <- recode( combined_dataset[[var]], `1` = 1, `2` = 2, `3` = 3)
    current_labels <- attr(combined_dataset[[var]], "labels")
    new_labels <- current_labels[!names(current_labels) %in% c(1,3,5)]
    new_labels[names(current_labels[3])] <- 1
    new_labels[names(current_labels[4])] <- 2
    new_labels[names(current_labels[5])] <- 3
    attr(combined_dataset[[var]], "labels") <- new_labels
  }
  
  adapt_variable <- domain_mapping %>%
    filter(domain == "-2/-1/1/5") %>%
    pull(variable)
  
  for(var in adapt_variable) {
    combined_dataset[[var]] <- recode( combined_dataset[[var]], `1` = 1, `5` = 0)
    current_labels <- attr(combined_dataset[[var]], "labels")
    new_labels <- current_labels[!names(current_labels) %in% c(1,5)]
    new_labels[names(current_labels[3])] <- 1
    new_labels[names(current_labels[4])] <- 0
    attr(combined_dataset[[var]], "labels") <- new_labels
  }
  
  adapt_variable <- domain_mapping %>%
    filter(domain == "-2/-1/1/2") %>%
    pull(variable)
  
  for(var in adapt_variable) {
    combined_dataset[[var]] <- recode( combined_dataset[[var]], `1` = 0, `2` = 1)
    current_labels <- attr(combined_dataset[[var]], "labels")
    new_labels <- current_labels[!names(current_labels) %in% c(1,5)]
    new_labels[names(current_labels[3])] <- 0
    new_labels[names(current_labels[4])] <- 1
    attr(combined_dataset[[var]], "labels") <- new_labels
  }
  
  adapt_variable <- domain_mapping %>%
    filter(domain == "-2/-1/1/2/3/97") %>%
    pull(variable)
  
  for(var in adapt_variable) {
    combined_dataset[[var]] <- recode( combined_dataset[[var]], `97` = 4)
    current_labels <- attr(combined_dataset[[var]], "labels")
    new_labels <- current_labels[names(current_labels) != 97]
    new_labels[names(current_labels[6])] <- 4
    attr(combined_dataset[[var]], "labels") <- new_labels
  }
  
  adapt_variable <- c("ph071_3", "ph071_1", "ph071_2")
  
  for(var in adapt_variable) {
    current_labels<- attr(combined_dataset[[var]], "label")
    values <- as.numeric(combined_dataset[[var]])
    combined_dataset[[var]] <- labelled(values)
    attr(combined_dataset[[var]], "labels") <- c("Refusal" = -2, "Don't know" = -1, "1" = 1, "2" = 2, "3 or more" = 3)
    attr(combined_dataset[[var]], "label") <- current_labels
  }
  
  combined_dataset <- combined_dataset %>%
    mutate(
      ch001_ = if_else(ch001_ > 10, NA_real_, ch001_),
      ch021_ = if_else(ch021_ > 10, NA_real_, ch021_),
      ph072_3 = if_else(ph072_3 == 10, NA_real_, ph072_3),
      ph013_ = if_else(ph013_ < 130, NA_real_, ph013_),
      ph012_ = if_else(ph012_ < 40 | ph012_ > 150 , NA_real_, ph012_)
    )
  
  # remove cancer variable
  combined_dataset <- combined_dataset %>%
    select(-ph006d10)
  
  # euro_d depression variable
  combined_dataset <- combined_dataset %>%
    mutate(euro_d = if_else(rowSums(.[var_euro], na.rm = TRUE) > 4, "yes", "no"))
  combined_dataset$euro_d <- labelled(combined_dataset$euro_d,
                                      labels = c("depressed" = "yes", "not depressed" = "no"),
                                      label = "depression")
  
  return(combined_dataset)
}

cleaning_dataset <- function(
    combined_dataset,
    protected_var,
    secondary_var,
    var_remove = c("euro1", "euro2", "euro3", "euro4", "euro5", "euro6", "euro7", "euro8", "euro9", "euro10", "euro11", "euro12", "mergeid", "wave", "hhid6", "mergeidp6", "coupleid6", "hhid5", "mergeidp5", "coupleid5", "hhid7", "mergeidp7", "coupleid7", "hhsize")
    ){
  
  # Removing euro and id variables from dataset
  combined_dataset <- combined_dataset %>%
    select(-all_of(c(var_remove, secondary_var)))
  
  # Removing variables with more than 10% of NA 
  na_perc <- round(colSums(is.na(combined_dataset)) / nrow(combined_dataset) * 100, 2)
  more10_var <- setdiff(names(na_perc[na_perc > 10]), protected_var)
  
  #combined_dataset <- combined_dataset %>% select(!all_of(names(more10_var)))
  combined_dataset <- combined_dataset %>%
    select(-all_of(setdiff(more10_var, protected_var)))
  return(combined_dataset)
}
