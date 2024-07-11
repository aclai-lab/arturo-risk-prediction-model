euro_analysis <- function(data, ids) {
  filtered_data <- data %>%
    filter(mergeid %in% ids)
  
  # Contare il numero di righe che hanno più di una variabile che inizia con "euro" con valori -1, -2 o NA
  count_rows <- filtered_data %>%
    rowwise() %>%
    mutate(euro_conditions = sum(across(starts_with("euro"), ~ .x %in% c(-1, -2) | is.na(.x)))) %>%
    ungroup() %>%
    filter(euro_conditions > 1) %>%
    nrow()
  
  return(count_rows)
}

score_process_wave <- function(data, ids) {
  filtered_data <- data %>%
    filter(mergeid %in% ids) %>%
    rowwise() %>%
    mutate(
      # Contare il numero di valori anomali (NA, -1, -2) per riga
      num_anomalous_values = sum(across(starts_with("euro"), ~ .x %in% c(NA, -1, -2))),
      # Contare il numero di valori 1 per riga
      num_ones = sum(across(starts_with("euro"), ~ .x == 1))
    ) %>%
    ungroup()
  
  filtered_data <- filtered_data %>%
    select(mergeid, starts_with("euro"), num_anomalous_values, num_ones) %>%
    mutate(
      euro_d = case_when(
        num_ones >= 4 & num_anomalous_values < 4 ~ num_ones,
        num_ones < 4 & num_anomalous_values < 4 ~ num_ones,
        TRUE ~ NA
      ),
    )
  
  filtered_data <- filtered_data %>%
    select(mergeid, euro_d)
  
  return(filtered_data)
  
}

old_process_wave <- function(data, ids) {
  filtered_data <- data %>%
    filter(mergeid %in% ids) %>%
    rowwise() %>%
    mutate(
      # Contare il numero di valori anomali (NA, -1, -2) per riga
      num_anomalous_values = sum(across(starts_with("euro"), ~ .x %in% c(NA, -1, -2))),
      # Contare il numero di valori 1 per riga
      num_ones = sum(across(starts_with("euro"), ~ .x == 1))
    ) %>%
    ungroup()
  
  
  filtered_data <- filtered_data %>%
    select(mergeid, starts_with("euro"), num_anomalous_values, num_ones) %>%
    mutate(
      euro_d = case_when(
        num_ones >= 4 & num_anomalous_values < 4 ~ "yes",
        num_ones < 4 & num_anomalous_values < 4 ~ "no",
        TRUE ~ NA_character_
      ),
      euro_d = labelled(euro_d, labels = c("depressed" = "yes", "not depressed" = "no"), label = "depression")
    )
  
  filtered_data <- filtered_data %>%
    select(mergeid, euro_d)
  
  
  return(filtered_data)
}

old_combine_wave_dataset <- function(  
    only_onco = TRUE,
    only_onco_fu = TRUE,
    score = TRUE,
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
      gv_isced_filter = c("isced1997_r")),
    data_names = c("cv_r", "dn", "iv", "hh", "sp", "ch", "ep", "co", "ac", "it", "gv_health", "ph", "mh", "hc", "br", "gv_isced")
)
{
  
  # Initialize lists to store the data for each wave, IDs of patients diagnosed with cancer, and subsets of data for cancer patients
  waves <- list()
  id_cancer <- list()
  cancer_data <- list()
  
  # Loop through each wave (5 to 8) to process the data
  for (wave in 5:8) {
    
    waves_ds <- list()  # Initialize a list to store sub-datasets for the current wave
    
    # Determine which data_names to use for the current wave
    if (wave == 8) {
      current_data_names <- c("gv_health", "ph")
    } else {
      current_data_names <- data_names
    } 
    
    # Read and filter datasets for the current wave
    for (name in current_data_names) {
      ds_path <- paste0("share data/sharew", wave, "_rel8-0-0_ALL_datasets_spss/sharew", wave, "_rel8-0-0_", name, ".sav")
      
      if (file.exists(ds_path)) {
        dataset <- read_sav(ds_path)
        filter_name <- paste0(name, "_filter")
        
        if (name == "ph" || (wave == 8 && name == "gv_health")) {
          waves_ds[[name]] <- dataset
        } else {
          if (filter_name %in% names(filters)) {
            columns_to_select <- c("mergeid", filters[[filter_name]])
            filtered_ds <- dataset %>% select(all_of(columns_to_select))
            waves_ds[[name]] <- filtered_ds
          }
        }
      }
    }
    
    combined_wave <- reduce(waves_ds, full_join, by = "mergeid")
    combined_wave <- combined_wave %>% mutate(wave = wave)
    
    cancer_filter <- combined_wave %>% filter(ph006d10 == 1 | ph071_3 > 0 | ph072_3 == 1)
    cancer_ids <- cancer_filter %>% pull(mergeid)
    
    cancer_subset <- combined_wave %>% filter(mergeid %in% cancer_ids)
    
    waves[[paste0("wave", wave)]] <- combined_wave
    id_cancer[[paste0("w", wave, "_id_cancer")]] <- cancer_ids
    cancer_data[[paste0("w", wave, "_cancer")]] <- cancer_subset
  }
  
  if(only_onco == TRUE){
    #w4_id_cancer <- intersect(waves$wave4$mergeid, id_cancer$w4_id_cancer)
    w5_id_cancer <- intersect(waves$wave5$mergeid, id_cancer$w5_id_cancer)
    w6_id_cancer <- intersect(waves$wave6$mergeid, id_cancer$w6_id_cancer)
    w7_id_cancer <- intersect(waves$wave7$mergeid, id_cancer$w7_id_cancer)
    w8_id_cancer <- intersect(waves$wave8$mergeid, id_cancer$w8_id_cancer)
    if(only_onco_fu == TRUE){
      #ids_wave_4_5 <- intersect(w4_id_cancer, waves$wave5$mergeid)
      ids_wave_5_6 <- intersect(w5_id_cancer, w6_id_cancer)
      ids_wave_not5_6_7 <- setdiff(intersect(w6_id_cancer, w7_id_cancer),  ids_wave_5_6)
      ids_wave_not5_not6_7_8 <- setdiff(intersect(w7_id_cancer, w8_id_cancer), union(ids_wave_5_6, ids_wave_not5_6_7))
    }
    else{
      #ids_wave_4_5 <- intersect(w4_id_cancer, waves$wave5$mergeid)
      ids_wave_5_6 <- intersect(w5_id_cancer, waves$wave6$mergeid)
      ids_wave_not5_6_7 <- setdiff(intersect(w6_id_cancer, waves$wave7$mergeid),  ids_wave_5_6)
      ids_wave_not5_not6_7_8 <- setdiff(intersect(w7_id_cancer, waves$wave8$mergeid), union(ids_wave_5_6, ids_wave_not5_6_7))
    }
  }else{
    all_cancer_ids <- unique(unlist(id_cancer))
    
    w5_id_cancer <- setdiff(waves$wave5$mergeid, all_cancer_ids)
    w6_id_cancer <- setdiff(waves$wave6$mergeid, all_cancer_ids)
    w7_id_cancer <- setdiff(waves$wave7$mergeid, all_cancer_ids)
    w8_id_cancer <- setdiff(waves$wave8$mergeid, all_cancer_ids)
    
    #ids_wave_4_5 <- intersect(w4_id_cancer, waves$wave5$mergeid)
    ids_wave_5_6 <- intersect(w5_id_cancer, waves$wave6$mergeid)
    ids_wave_not5_6_7 <- setdiff(intersect(w6_id_cancer, waves$wave7$mergeid),  ids_wave_5_6)
    ids_wave_not5_not6_7_8 <- setdiff(intersect(w7_id_cancer, waves$wave8$mergeid), union(ids_wave_5_6, ids_wave_not5_6_7))
  }
  
  #data_wave_4 <- process_wave(waves$wave4, ids_wave_4_5, initial = TRUE)
  #euro_sum_wave_5 <- process_wave(waves$wave6, ids_wave_4_5, sum_only = TRUE)
  
  data_wave_5 <- waves$wave5 %>%
    filter(mergeid %in% ids_wave_5_6)
  euro_sum_wave_6 <- if(score == TRUE) score_process_wave(waves$wave6, ids_wave_5_6) else process_wave(waves$wave6, ids_wave_5_6)
  
  data_wave_6 <- waves$wave6 %>%
    filter(mergeid %in% ids_wave_not5_6_7)
  euro_sum_wave_7 <- if(score == TRUE) score_process_wave(waves$wave7, ids_wave_not5_6_7) else process_wave(waves$wave7, ids_wave_not5_6_7)
  
  data_wave_7 <- waves$wave7 %>%
    filter(mergeid %in% ids_wave_not5_not6_7_8)
  euro_sum_wave_8 <- if(score == TRUE) score_process_wave(waves$wave8,  ids_wave_not5_not6_7_8) else process_wave(waves$wave8,  ids_wave_not5_not6_7_8)
  
  #data_wave_4_5 <- left_join(data_wave_4, euro_sum_wave_5, by = "mergeid")
  data_wave_5_6 <- left_join(data_wave_5, euro_sum_wave_6, by = "mergeid")
  data_wave_6_7 <- left_join(data_wave_6, euro_sum_wave_7, by = "mergeid")
  data_wave_7_8 <- left_join(data_wave_7, euro_sum_wave_8, by = "mergeid")
  
  if(only_onco == TRUE && only_onco_fu == FALSE){
    data_wave_5_6  <- data_wave_5_6  %>%
      mutate( flag_cfu = if_else(mergeid %in% w6_id_cancer, 0, 1))
    data_wave_5_6$flag_cfu <- labelled(data_wave_5_6$flag_cfu,
                                       labels = c( "yes" = 0,"no" = 1),
                                       label = "have cancer in follow up wave?")
    
    
    data_wave_6_7  <- data_wave_6_7  %>%
      mutate( flag_cfu = if_else(mergeid %in% w7_id_cancer, 0, 1))
    data_wave_6_7$flag_cfu <- labelled(data_wave_6_7$flag_cfu,
                                       labels = c( "yes" = 0,"no" = 1),
                                       label = "have cancer in follow up wave?")
    
    
    data_wave_7_8  <- data_wave_7_8  %>%
      mutate( flag_cfu = if_else(mergeid %in% w8_id_cancer, 0, 1))
    data_wave_7_8$flag_cfu <- labelled(data_wave_7_8$flag_cfu,
                                       labels = c( "yes" = 0,"no" = 1),
                                       label = "have cancer in follow up wave?")
  }
  
  combined_data <- bind_rows(data_wave_5_6, data_wave_6_7, data_wave_7_8)
  combined_data <- combined_data %>% filter(!is.na(euro_d))
  return(combined_data)
}

process_wave <- function(data, ids, initial = FALSE) {
  filtered_data <- data %>%
    filter(mergeid %in% ids) %>%
    rowwise() %>%
    mutate(
      # Contare il numero di valori anomali (NA, -1, -2) per riga
      num_anomalous_values = sum(across(starts_with("euro"), ~ .x %in% c(NA, -1, -2))),
      # Contare il numero di valori 1 per riga
      num_ones = sum(across(starts_with("euro"), ~ .x == 1))
    ) %>%
    ungroup()
  
  if (initial) {
    filtered_data <- filtered_data %>%
      mutate(
        initial_euro_d = case_when(
          num_ones >= 4 & num_anomalous_values < 4 ~ "yes",
          num_ones < 4 & num_anomalous_values < 4 ~ "no",
          TRUE ~ NA_character_
        ),
        initial_euro_d = labelled(initial_euro_d, labels = c("depressed" = "yes", "not depressed" = "no"), label = "depression")
      )
    filtered_data <- filtered_data %>%
      select(-num_anomalous_values, -num_ones)
  }else {
    filtered_data <- filtered_data %>%
      select(mergeid, starts_with("euro"), num_anomalous_values, num_ones) %>%
      mutate(
        euro_d = case_when(
          num_ones >= 4 & num_anomalous_values < 4 ~ "yes",
          num_ones < 4 & num_anomalous_values < 4 ~ "no",
          TRUE ~ NA_character_
        ),
        euro_d = labelled(euro_d, labels = c("depressed" = "yes", "not depressed" = "no"), label = "depression")
      )
    
    filtered_data <- filtered_data %>%
      select(mergeid, euro_d)
  }
  
  return(filtered_data)
}

load_dataset <- function(  
    only_onco = TRUE,
    only_onco_fu = TRUE,
    score = TRUE,
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
)
{
  
  # Initialize lists to store the data for each wave, IDs of patients diagnosed with cancer, and subsets of data for cancer patients
  waves <- list()
  id_cancer <- list()
  cancer_data <- list()
  
  # Loop through each wave (5 to 8) to process the data
  for (wave in 5:8) {
    
    waves_ds <- list()  # Initialize a list to store sub-datasets for the current wave
    
    # Determine which data_names to use for the current wave
    if (wave == 8) {
      current_data_names <- c("gv_health", "ph")
    } else {
      current_data_names <- data_names
    }
    
    # Read and filter datasets for the current wave
    for (name in current_data_names) {
      ds_path <- paste0("share data/sharew", wave, "_rel8-0-0_ALL_datasets_spss/sharew", wave, "_rel8-0-0_", name, ".sav")
      
      if (file.exists(ds_path)) {
        dataset <- read_sav(ds_path)
        filter_name <- paste0(name, "_filter")
        
        if (name == "ph" || (wave == 8 && name == "gv_health")) {
          waves_ds[[name]] <- dataset
        } else {
          if (filter_name %in% names(filters)) {
            columns_to_select <- c("mergeid", filters[[filter_name]])
            filtered_ds <- dataset %>% select(all_of(columns_to_select))
            waves_ds[[name]] <- filtered_ds
          }
        }
      }
    }
    
    combined_wave <- reduce(waves_ds, full_join, by = "mergeid")
    combined_wave <- combined_wave %>% mutate(wave = wave)
    
    cancer_filter <- combined_wave %>% filter(ph006d10 == 1 | ph071_3 > 0 | ph072_3 == 1)
    cancer_ids <- cancer_filter %>% pull(mergeid)
    
    cancer_subset <- combined_wave %>% filter(mergeid %in% cancer_ids)
    
    waves[[paste0("wave", wave)]] <- combined_wave
    id_cancer[[paste0("w", wave, "_id_cancer")]] <- cancer_ids
    cancer_data[[paste0("w", wave, "_cancer")]] <- cancer_subset
  }
  
  if(only_onco == TRUE){
    #w4_id_cancer <- intersect(waves$wave4$mergeid, id_cancer$w4_id_cancer)
    w5_id_cancer <- intersect(waves$wave5$mergeid, id_cancer$w5_id_cancer)
    w6_id_cancer <- intersect(waves$wave6$mergeid, id_cancer$w6_id_cancer)
    w7_id_cancer <- intersect(waves$wave7$mergeid, id_cancer$w7_id_cancer)
    w8_id_cancer <- intersect(waves$wave8$mergeid, id_cancer$w8_id_cancer)
    if(only_onco_fu == TRUE){
      #ids_wave_4_5 <- intersect(w4_id_cancer, waves$wave5$mergeid)
      ids_wave_5_6 <- intersect(w5_id_cancer, w6_id_cancer)
      ids_wave_not5_6_7 <- setdiff(intersect(w6_id_cancer, w7_id_cancer),  ids_wave_5_6)
      ids_wave_not5_not6_7_8 <- setdiff(intersect(w7_id_cancer, w8_id_cancer), union(ids_wave_5_6, ids_wave_not5_6_7))
    }
    else{
      #ids_wave_4_5 <- intersect(w4_id_cancer, waves$wave5$mergeid)
      ids_wave_5_6 <- intersect(w5_id_cancer, waves$wave6$mergeid)
      ids_wave_not5_6_7 <- setdiff(intersect(w6_id_cancer, waves$wave7$mergeid),  ids_wave_5_6)
      ids_wave_not5_not6_7_8 <- setdiff(intersect(w7_id_cancer, waves$wave8$mergeid), union(ids_wave_5_6, ids_wave_not5_6_7))
    }
  }else{
    all_cancer_ids <- unique(unlist(id_cancer))
    
    w5_id_cancer <- setdiff(waves$wave5$mergeid, all_cancer_ids)
    w6_id_cancer <- setdiff(waves$wave6$mergeid, all_cancer_ids)
    w7_id_cancer <- setdiff(waves$wave7$mergeid, all_cancer_ids)
    w8_id_cancer <- setdiff(waves$wave8$mergeid, all_cancer_ids)
    
    #ids_wave_4_5 <- intersect(w4_id_cancer, waves$wave5$mergeid)
    ids_wave_5_6 <- intersect(w5_id_cancer, waves$wave6$mergeid)
    ids_wave_not5_6_7 <- setdiff(intersect(w6_id_cancer, waves$wave7$mergeid),  ids_wave_5_6)
    ids_wave_not5_not6_7_8 <- setdiff(intersect(w7_id_cancer, waves$wave8$mergeid), union(ids_wave_5_6, ids_wave_not5_6_7))
  }
  
  
  
  #data_wave_4 <- process_wave(waves$wave4, ids_wave_4_5, initial = TRUE)
  #euro_sum_wave_5 <- process_wave(waves$wave6, ids_wave_4_5, sum_only = TRUE)
  
  data_wave_5 <- if(score == TRUE) score_process_wave(waves$wave5, ids_wave_5_6, initial = TRUE) else process_wave(waves$wave5, ids_wave_5_6, initial = TRUE)
  euro_sum_wave_6 <- if(score == TRUE) score_process_wave(waves$wave6, ids_wave_5_6, initial = FALSE) else process_wave(waves$wave6, ids_wave_5_6, initial = FALSE)
  
  data_wave_6 <- if(score == TRUE) score_process_wave(waves$wave6, ids_wave_not5_6_7, initial = TRUE) else process_wave(waves$wave6, ids_wave_not5_6_7, initial = TRUE)
  euro_sum_wave_7 <- if(score == TRUE) score_process_wave(waves$wave7, ids_wave_not5_6_7, initial = FALSE) else process_wave(waves$wave7, ids_wave_not5_6_7, initial = FALSE)
  
  data_wave_7 <- if(score == TRUE) score_process_wave(waves$wave7, ids_wave_not5_not6_7_8, initial = TRUE) else process_wave(waves$wave7, ids_wave_not5_not6_7_8, initial = TRUE)
  euro_sum_wave_8 <- if(score == TRUE) score_process_wave(waves$wave8,  ids_wave_not5_not6_7_8, initial = FALSE) else process_wave(waves$wave8,  ids_wave_not5_not6_7_8, initial = FALSE)
  
  #data_wave_4_5 <- left_join(data_wave_4, euro_sum_wave_5, by = "mergeid")
  data_wave_5_6 <- left_join(data_wave_5, euro_sum_wave_6, by = "mergeid")
  data_wave_6_7 <- left_join(data_wave_6, euro_sum_wave_7, by = "mergeid")
  data_wave_7_8 <- left_join(data_wave_7, euro_sum_wave_8, by = "mergeid")
  
  if(only_onco == TRUE){
    data_wave_5_6  <- data_wave_5_6  %>%
      mutate( flag_cfu = if_else(mergeid %in% w6_id_cancer, 0, 1))
    data_wave_5_6$flag_cfu <- labelled(data_wave_5_6$flag_cfu,
                                       labels = c( "yes" = 0,"no" = 1),
                                       label = "have cancer in follow up wave?")
    
    
    data_wave_6_7  <- data_wave_6_7  %>%
      mutate( flag_cfu = if_else(mergeid %in% w7_id_cancer, 0, 1))
    data_wave_6_7$flag_cfu <- labelled(data_wave_6_7$flag_cfu,
                                       labels = c( "yes" = 0,"no" = 1),
                                       label = "have cancer in follow up wave?")
    
    
    data_wave_7_8  <- data_wave_7_8  %>%
      mutate( flag_cfu = if_else(mergeid %in% w8_id_cancer, 0, 1))
    data_wave_7_8$flag_cfu <- labelled(data_wave_7_8$flag_cfu,
                                       labels = c( "yes" = 0,"no" = 1),
                                       label = "have cancer in follow up wave?")
  }
  
  combined_data <- bind_rows(data_wave_5_6, data_wave_6_7, data_wave_7_8)
  combined_data <- combined_data %>% filter(!is.na(euro_d))
  #combined_data <- combined_data %>% filter(!is.na(euro_d), !is.na(initial_euro_d))
  return(combined_data)
}

load_death_dataset <- function(  
    filters = list(
      cv_r_filter = c("age_int", "hhsize"),
      dn_filter = c("dn042_", "dn503_", "dn014_", "dn034_"),
      iv_filter = c("iv009_"),
      hh_filter = c("hh022_", "hh025_"),
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
      gv_isced_filter = c("isced1997_r")),
    data_names = c("cv_r", "dn", "iv", "hh", "sp", "ch", "ep", "co", "ac", "it", "gv_health", "ph", "mh", "hc", "br", "gv_isced")
){
  
  waves <- list()
  id_cancer <- list()
  cancer_data <- list()
  
  # Loop through each wave (5 to 8) to process the data
  for (wave in 4:8) {
    
    waves_ds <- list()  # Initialize a list to store sub-datasets for the current wave
    
    # Determine which data_names to use for the current wave
    if (wave == 8) {
      current_data_names <- c("gv_health", "ph")
    } else if(wave == 4){
      current_data_names <- c("ph")
    }
    else{
      current_data_names <- data_names
    } 
    
    # Read and filter datasets for the current wave
    for (name in current_data_names) {
      ds_path <- paste0("share data/sharew", wave, "_rel8-0-0_ALL_datasets_spss/sharew", wave, "_rel8-0-0_", name, ".sav")
      
      if (file.exists(ds_path)) {
        dataset <- read_sav(ds_path)
        filter_name <- paste0(name, "_filter")
        
        if (name == "ph" || (wave == 8 && name == "gv_health")) {
          waves_ds[[name]] <- dataset
        } else {
          if (filter_name %in% names(filters)) { 
            columns_to_select <- c("mergeid", filters[[filter_name]])
            filtered_ds <- dataset %>% select(all_of(columns_to_select))
            waves_ds[[name]] <- filtered_ds
          }
        }
      }
    }
    
    combined_wave <- reduce(waves_ds, full_join, by = "mergeid")
    combined_wave <- combined_wave %>% mutate(wave = wave)
    
    cancer_filter <- combined_wave %>% filter(ph006d10 == 1 | ph071_3 > 0 | ph072_3 == 1)
    cancer_ids <- cancer_filter %>% pull(mergeid)
    
    cancer_subset <- combined_wave %>% filter(mergeid %in% cancer_ids)
    
    waves[[paste0("wave", wave)]] <- combined_wave
    id_cancer[[paste0("w", wave, "_id_cancer")]] <- cancer_ids
    cancer_data[[paste0("w", wave, "_cancer")]] <- cancer_subset
  }
  
  # ALL WAVE DEATH DATASET
  death_w4 <- read_sav("share data/sharew4_rel8-0-0_ALL_datasets_spss/sharew4_rel8-0-0_xt.sav")
  death_w5 <- read_sav("share data/sharew5_rel8-0-0_ALL_datasets_spss/sharew5_rel8-0-0_xt.sav")
  death_w6 <- read_sav("share data/sharew6_rel8-0-0_ALL_datasets_spss/sharew6_rel8-0-0_xt.sav")
  death_w7 <- read_sav("share data/sharew7_rel8-0-0_ALL_datasets_spss/sharew7_rel8-0-0_xt.sav")
  death_w8 <- read_sav("share data/sharew8_rel8-0-0_ALL_datasets_spss/sharew8_rel8-0-0_xt.sav")
  
  # CANCER ID
  w4_id_cancer <- intersect(waves$wave4$mergeid, id_cancer$w4_id_cancer)
  w5_id_cancer <- intersect(waves$wave5$mergeid, id_cancer$w5_id_cancer)
  w6_id_cancer <- intersect(waves$wave6$mergeid, id_cancer$w6_id_cancer)
  w7_id_cancer <- intersect(waves$wave7$mergeid, id_cancer$w7_id_cancer)
  w8_id_cancer <- intersect(waves$wave8$mergeid, id_cancer$w8_id_cancer)
  
  # DEATH ID FOR ONCO PATIENT IN FU
  ids_death_w4_to_w5_with_cancer <- intersect(w4_id_cancer, death_w5$mergeid) 
  ids_death_w5_to_w6_with_cancer <- intersect(w5_id_cancer, death_w6$mergeid) 
  ids_death_w6_to_w7_with_cancer <- intersect(w6_id_cancer, death_w7$mergeid) 
  ids_death_w7_to_w8_with_cancer <- intersect(w7_id_cancer, death_w8$mergeid)  
  
  # FIND MISSING PATINET 
  ids_wave_5_not6 <- setdiff(w5_id_cancer, waves$wave6$mergeid)
  ids_wave_6_not7 <- setdiff(w6_id_cancer, waves$wave7$mergeid)
  ids_wave_7_not8 <- setdiff(w7_id_cancer, waves$wave8$mergeid)
  
  # REMOVING DEATH IN BL
  waves$wave5 <- waves$wave5 %>% filter(!mergeid %in% ids_death_w4_to_w5_with_cancer)
  waves$wave6 <- waves$wave6 %>% filter(!mergeid %in% ids_death_w5_to_w6_with_cancer)
  waves$wave7 <- waves$wave7 %>% filter(!mergeid %in% ids_death_w6_to_w7_with_cancer)
  
  # DATASET FOR DEATH ONCO PATIENT
  #death_w5_to_w6 <- waves$wave5 %>% filter(mergeid %in% ids_death_w5_to_w6_with_cancer)
  #death_w6_to_w7 <- waves$wave6 %>% filter(mergeid %in% ids_death_w6_to_w7_with_cancer)
  #death_w7_to_w8 <- waves$wave7 %>% filter(mergeid %in% ids_death_w7_to_w8_with_cancer)
  
  # FILTER WAVE FOR ID IN DEATH OR MISSING
  #waves$wave5 <- waves$wave5 %>% filter(mergeid %in% union(ids_wave_5_not6, ids_death_w5_to_w6_with_cancer))
  #waves$wave6 <- waves$wave6 %>% filter(mergeid %in% union(ids_wave_6_not7, ids_death_w6_to_w7_with_cancer))
  #waves$wave7 <- waves$wave7 %>% filter(mergeid %in% union(ids_wave_7_not8, ids_death_w7_to_w8_with_cancer))
  
  waves$wave5 <- waves$wave5  %>%
    mutate(condition_fu = if_else(mergeid %in% ids_death_w5_to_w6_with_cancer, 1, 
                                  if_else(mergeid %in% ids_wave_5_not6, 2, 0)))
  waves$wave5$condition_fu <- labelled(waves$wave5$condition_fu, 
                                       labels = c( "alive" = 0, "death" = 1,"missing" = 2),
                                       label = "death or missing in follow up wave?")
  
  waves$wave6 <- waves$wave6  %>%
    mutate(condition_fu = if_else(mergeid %in% ids_death_w6_to_w7_with_cancer, 1, 
                                  if_else(mergeid %in% ids_wave_6_not7, 2, 0)))
  waves$wave6$condition_fu <- labelled(waves$wave6$condition_fu,
                                       labels = c("alive" = 0, "death" = 1,"missing" = 2),
                                       label = "death or missing in follow up wave?")
  
  waves$wave7 <- waves$wave7  %>%
    mutate(condition_fu = if_else(mergeid %in% ids_death_w7_to_w8_with_cancer, 1, 
                                  if_else(mergeid %in% ids_wave_7_not8, 2, 0)))
  waves$wave7$condition_fu <- labelled(waves$wave7$condition_fu,
                                       labels = c("alive" = 0, "death" = 1,"missing" = 2),
                                       label = "death or missing in follow up wave?") 
  
  waves$wave5  <- waves$wave5  %>% 
    mutate( flag_cfu = if_else(mergeid %in% w6_id_cancer, 0, 1))
  waves$wave5$flag_cfu <- labelled(waves$wave5$flag_cfu,
                                   labels = c( "yes" = 0,"no" = 1),
                                   label = "have cancer in follow up wave?")
  
  
  waves$wave6  <- waves$wave6  %>%
    mutate( flag_cfu = if_else(mergeid %in% w7_id_cancer, 0, 1))
  waves$wave6$flag_cfu <- labelled(waves$wave6$flag_cfu,
                                   labels = c( "yes" = 0,"no" = 1),
                                   label = "have cancer in follow up wave?")
  
  
  waves$wave7  <- waves$wave7  %>%
    mutate( flag_cfu = if_else(mergeid %in% w8_id_cancer, 0, 1))
  waves$wave7$flag_cfu <- labelled(waves$wave7$flag_cfu,
                                   labels = c( "yes" = 0,"no" = 1),
                                   label = "have cancer in follow up wave?")
  
  waves$wave5 <- waves$wave5 %>% filter(mergeid %in% w5_id_cancer)
  waves$wave6 <- waves$wave6 %>% filter(mergeid %in% w6_id_cancer)
  waves$wave7 <- waves$wave7 %>% filter(mergeid %in% w7_id_cancer)
  
  combined_data <- bind_rows(waves$wave5, waves$wave6, waves$wave7)
  return(combined_data)
}

filtering_dataset <- function(
    combined_dataset,
    var_euro = c("euro1", "euro2", "euro3", "euro4", "euro5", "euro6", "euro7", "euro8", "euro9", "euro10", "euro11", "euro12")
    ){
  
  # Negative value to NA
  combined_dataset <- combined_dataset %>%
    mutate(across(where(~ is.numeric(.) || is.labelled(.)), ~ replace(., . %in% c(-9999992, -9999991, -1, -2, -3, -9), NA)))

  # NA to -1
  combined_dataset <- combined_dataset %>%
    mutate(across(where(is.integer), ~if_else(is.na(.), as.integer(-1), .)),
           across(where(is.double), ~if_else(is.na(.), as.double(-1), .)),
           across(where(is.character), ~if_else(is.na(.), "-1", .)))
  
  combined_dataset <- combined_dataset %>%
    mutate(initial_euro_d = labelled(initial_euro_d, labels = c(
                    "missing" = -1,
                    "depressed" = "yes",
                    "not depressed" = "no"
                  )))
  
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
      ph072_3 == 5 ~ 0,
      ph072_3 == 1 ~ 1,
      TRUE ~ -1
    )) %>% mutate(more2y_c = labelled(more2y_c, labels = c(
      "missing" = -1,
      "Less than 2 years with cancer" = 0,
      "2 or more years with cancer" = 1
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
    combined_dataset[[var]] <- recode( combined_dataset[[var]], `1` = 1, `3` = 2, `5` = 3)
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
    combined_dataset[[var]] <- recode( combined_dataset[[var]], `1` = 0, `5` = 1)
    current_labels <- attr(combined_dataset[[var]], "labels")
    new_labels <- current_labels[!names(current_labels) %in% c(1,5)]
    new_labels[names(current_labels[3])] <- 0
    new_labels[names(current_labels[4])] <- 1
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
  
  adapt_variable <- domain_mapping %>%
    filter(domain == "-2/-1/1/3") %>%
    pull(variable)
  
  for(var in adapt_variable) {
    combined_dataset[[var]] <- recode( combined_dataset[[var]], `1` = 0, `3` = 1)
    current_labels <- attr(combined_dataset[[var]], "labels")
    new_labels <- current_labels[!names(current_labels) %in% c(1,3)]
    new_labels[names(current_labels[3])] <- 0
    new_labels[names(current_labels[4])] <- 1
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
      ch001_ = if_else(ch001_ > 10, -2, ch001_),
      ch021_ = if_else(ch021_ > 10, -2, ch021_),
      ph072_3 = if_else(ph072_3 == 10, -2, ph072_3),
      ph012_ = ifelse(ph012_ >= 0 & ph012_ < 35, -1, ph012_),
      ph013_ = ifelse(ph013_ >= 0 & ph013_ < 135, -1, ph013_)
    )
  
  combined_dataset <- combined_dataset %>%
    mutate(
      bmi = ifelse(ph012_ == -1 | ph013_ == -1, -1, ph012_ / (ph013_ / 100)^2),
      bmi = ifelse(bmi == -1, -1, round(bmi, 2))
    )%>%
  mutate(bmi = labelled(bmi, labels = c(
    "missing" = -1
  )))
var_label(combined_dataset$bmi) <- "bmi"
  
  # remove cancer variable
  combined_dataset <- combined_dataset %>%
    select(-ph006d10)
  
  return(combined_dataset)
}

categorization_variable <- function(dataset, variables){
  for (var in variables){
    
    prefix_cols <- grep(var, names(dataset), value = TRUE)
    vars_with_dno <- grep("dno$", prefix_cols, value = TRUE)
    if (length(vars_with_dno) > 0) {
      prefix_cols <- setdiff(prefix_cols, vars_with_dno)
    }
    
    if(var == "ph059"){
      prefix_cols <- c("ph045_", prefix_cols)
    }
    
    if (var != "ph009"){
      vars <- paste(var, "_", sep = "")
      dataset[[vars]] <- apply(dataset[, prefix_cols], 1, function(row) {
        numeric_row <- as.numeric(as.character(row))
        
        if(all(numeric_row %in% c(-1, -2))) {
          return(if(all(numeric_row == -1)) -1 else -2)
        }
        
        count_selected <- sum(numeric_row == 1, na.rm = TRUE)
        
        if(count_selected == 1) {
          return(which(numeric_row == 1)[1])
        } else if(count_selected > 1) {
          return(length(prefix_cols) + 1)
        } else {
          return(0)
        }
      })
      
      labels_vector <- c( "-2" = "Refusal", "-1" = "Don't know", "0" = "None Selected")
      if(var != "ph006"){
        for (i in seq_along(prefix_cols)) {
          label <- get_label_by_variable(dataset, prefix_cols[i])
          if(prefix_cols[i]!= "ph045_"){
            labels_vector[as.character(i)] <- strsplit(label, ": ")[[1]][[2]]
          }else{
            labels_vector[as.character(i)] <- label
          }
        }
        labels_vector[as.character(length(prefix_cols) + 1)] <- "Multiple selection"
        
        labels_vector <- setNames(as.numeric(names(labels_vector)), labels_vector)
        dataset[[vars]] <- labelled(x = dataset[[vars]], labels = labels_vector)
        
        var_label(dataset[[vars]]) <- strsplit(label, ": ")[[1]][[1]]
      }else{
        for (i in seq_along(prefix_cols)) {
          label <- get_label_by_variable(dataset, prefix_cols[i])
          labels_vector[as.character(i)] <- strsplit(label, ": ")[[1]][[1]]
        }
        labels_vector[as.character(length(prefix_cols) + 1)] <- "Multiple selection"
        
        labels_vector <- setNames(as.numeric(names(labels_vector)), labels_vector)
        dataset[[vars]] <- labelled(x = dataset[[vars]], labels = labels_vector)
        
        var_label(dataset[[vars]]) <- "Condition"
      }
    }else{
      
      vars_with_cancer<- grep("_10$", prefix_cols, value = TRUE)
      if (length(vars_with_cancer) > 0) {
        prefix_cols <- setdiff(prefix_cols, vars_with_cancer)
      }
      
      prefix_cols<- list(prefix_cols, vars_with_cancer)
      
      for(i in 1:2){
        vars <- paste(var, i, "_", sep = "")
        
        dataset[[vars]] <- apply(dataset[, prefix_cols[[i]]], 1, function(row) {
          numeric_row <- as.numeric(as.character(row))
          
          if(all(numeric_row %in% c(-1, -2))) {
            return(if(all(numeric_row == -1)) -1 else -2)
          }
          
          positive_row <- numeric_row > 0
          count_selected <- sum(positive_row, na.rm = TRUE)
          
          if(count_selected == 1) {
            years <- dataset$age_int[which(positive_row)] - row[positive_row]
            if(years < 2){
              return(1)
            }else if( years <= 5){
              return(2)
            }else{
              return(3)
            }
          }else if(count_selected > 1){ 
            return(4)
          }else{ 
            return(0)
          }
        })
        
        labels_vector <- c(
          "-2" = "Refusal", 
          "-1" = "Don't know", 
          "0" = "None Selected",
          "1" = "Less than 2 years",
          "2" = "Between 2 and 5 years",
          "3" = "More than 5 years",
          "4" = "Multiple selection")
        
        labels_vector <- setNames(as.numeric(names(labels_vector)), labels_vector)
        dataset[[vars]] <- labelled(x = dataset[[vars]], labels = labels_vector)
        if(i == 1){
          var_label(dataset[[vars]]) <- "Years of condition"
        }else{
          var_label(dataset[[vars]]) <- "Years of cancer"
        }
      }
      
    }
  }
  
  return(dataset)
}

cleaning_dataset <- function(
    dataset,
    protected_var,
    to_be_categ = c( "ac035", "ph006", "ph011", "ph048", "ph049", "ph089", "ph059", "ph008", "ph087", "ph690", "ph009", "ph074"),
    secondary_var = c("ph008", "ac035", "ph006", "ph011", "ph048", "ph049", "ph089", "ph059", "ph087", "ph690", "ph009", "ph071",  "ph072", "ph073", "ph075", "ph076", "ph077", "ph080", "ph074"),
    to_be_remove = c("mergeid", "wave", "hhid6", "mergeidp6", "coupleid6", "hhid5", "mergeidp5", "coupleid5", "hhid7", "mergeidp7", "coupleid7", "hhsize", "language", "ph012_", "ph013_")
    ){
  
  secondary_var <- unlist(lapply(secondary_var, function(prefix) {
    grep(prefix, names(dataset), value = TRUE)
  })) 
  
  to_be_remove <- c(secondary_var, to_be_remove)
  
  #categorization of secondary variables
  dataset <- categorization_variable(dataset, to_be_categ)
   
  clean_domain_mapping <- get_domain_map(dataset)
  clean_var_type = get_var_type(dataset, clean_domain_mapping)
  
  # Removing 
  dataset <- dataset %>%
    select(-all_of(c(to_be_remove)))
  
  dataset <- dataset %>% 
      mutate(across(-c(euro_d) , update_labels))
  
  dataset <- alpha_removing(dataset, clean_var_type$var_continue)
  
  return(dataset)
}

load_dataset <- function(
    only_onco = TRUE, 
    only_onco_fu = FALSE, 
    score = FALSE
    ){
  
  combined_dataset <- combine_wave_dataset(only_onco, only_onco_fu, score)
  
  filtered_dataset <- filtering_dataset(combined_dataset)

  clean_dataset <- cleaning_dataset(filtered_dataset, protected_var = "")

  return(clean_dataset)
}

