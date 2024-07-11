##################################################################
####################### UTILITY FUNCTIONS ########################
##################################################################

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
    return("The variable does not have label attribute.")
  } 
  return(variable_label)
}

# Function to find variable labels by variable code
get_labels_by_variable <- function(dataset, var_name) {
  if (!var_name %in% names(dataset)) {
    stop("The variable name provided does not exist in the dataset.")
  }
  var <- dataset[[var_name]]
  if (is.null(attr(var, "labels"))) {
    return("The variable does not have labels attribute.")
  }
  labels <- attr(var, "labels")
  labels_df <- data.frame(value = as.numeric(labels), label = names(labels), stringsAsFactors = FALSE)
  return(labels_df)
}

# Function to find variable column domain
get_domain <- function(column) {
  if (is.labelled(column)){
    domain = val_labels(column)
    return(paste(domain, collapse = "/"))  
  }
  else{
    return("")
  }
}

# Function to find unlabelled variable column domain
get_unlabel_domain <- function(column) {
  unique_values <- sort(unique(na.omit(column)))
  return(paste(unique_values, collapse = "/"))
}

# Function to find all variable domain and mapping
get_domain_map <- function(dataset){
  
  column_names <- names(dataset)
  
  domains <- sapply(column_names, function(column_name) {
    
    column_data <- dataset[[column_name]]
    
    if (column_name == "ac012_") {
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
}

# Function to check variable domain
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


# Function to remove negative labels different from -1
update_labels <- function(column) {
  if (is.labelled(column)) {
    labels <- attr(column, "labels")
    
    # Filtra le etichette per rimuovere quelle con valori negativi
    positive_labels <- labels[labels >= 0]
    
    # Aggiunge una nuova etichetta per -1 con il nome "missing"
    # Assicurati che -1 sia coerente con il tipo di dato della colonna
    if (is.integer(column)) {
      # Se i dati sono interi, la nuova etichetta deve essere un intero
      new_labels <- c(setNames(as.integer(-1), "missing"), positive_labels)
    } else {
      # Altrimenti, considera i dati come double (predefinito)
      new_labels <- c(setNames(as.double(-1), "missing"), positive_labels)
    }
    
    attr(column, "labels") <- new_labels
  }
  return(column)
}

# Function to remove negative labels different from -1
update_labels_2 <- function(column) {
  if (is.labelled(column)) {
    labels <- attr(column, "labels")
    
    # Filtra le etichette per rimuovere quelle con valori negativi
    new_labels <- labels[labels >= 0]
    
    
    attr(column, "labels") <- new_labels
  }
  return(column)
}


# Function for count and list variable occurrencies in a dataset
create_var_occ <- function(dataset, type = "") {
  var_occ <- list()
  
  for (var in names(dataset)) {
    # Extract the domain for the variable
    if(type=="continue")
      var_domain <- get_unlabel_domain(dataset[[var]])
    else
      var_domain <- get_domain(dataset[[var]])
    
    if(var_domain == "") var_domain <- get_unlabel_domain(dataset[[var]])
    
    # Split the domain string into individual levels
    val_domain <- strsplit(var_domain, "/")[[1]]
    # Create a factor with the specified levels and compute the table
    occ <- table(factor(dataset[[var]], levels = val_domain))
    # Store the table in the list with the variable name as the key
    var_occ[[var]] <- occ
  }
  
  return(var_occ)
}

# Function to calculate basic statistics, variance, and standard deviation for each sequential variablee
# summary_stats <- calculate_statistics(seq_domain, combined_dataset)
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


var_na_perc <- function(dataset) {
  # Calculate the percentage of NA values for each column
  na_perc <- round(colSums(is.na(dataset)) / nrow(dataset) * 100, 2)
  
  # Initialize an empty tibble to store results
  wo_na <- tibble(
    perc = character(),  # Percentage threshold
    remains = integer()  # Remaining columns after applying threshold
  )
  
  # Iterate over thresholds from 5% to 95% in steps of 5%
  for (i in seq(from = 5, to = 95, by = 5)) {
    # Count columns with NA percentage greater than or equal to the threshold
    var_na <- sum(na_perc >= i)
    # Add a row to the tibble with the threshold and remaining column count
    wo_na <- wo_na %>%
      add_row(
        perc = paste(i, "%", sep=""),
        remains = (length(names(dataset)) - var_na)
      )
  }
  
  # Return the result tibble
  return(wo_na)
}

# Function to get variables types (continuos, categorial..)
get_var_type <- function(
    dataset, 
    domain_mapping,
    var_filter = c("mergeid", "wave", "hhid6", "mergeidp6", "coupleid6", "hhid5", "mergeidp5", "coupleid5", "hhid7", "mergeidp7", "coupleid7", "hhsize"),
    unlabel_filter = c("ac012_"),
    var_euro = c("euro1", "euro2", "euro3", "euro4", "euro5", "euro6", "euro7", "euro8", "euro9", "euro10", "euro11", "euro12"),
    continue_filter = c("-9/-2/-1", "-2/-1", "-3/-2/-1", "-9999992/-9999991"),
    not_ord_filter = c("country", "language", "ph054_", "ph066_", "ph071_1", "ph071_2", "ph071_3", "ph071_4", "ph072_3", "ph076_1", "ph076_2", "ph076_3", "ph076_4", "ph077_1",  "ph077_2", "ph077_3", "ph077_4", "hc029_", "ph075_1", "ph075_3", "ep005_", "isced1997_r", "dn014_", "dn034_", "iv009_", "sp002_", "sp008_", "phactiv", "ph004_", "ph041_", "ph044_", "ph050_", "ph061_", "ph065_", "ph082_", "ph084_", "ph745_", "hc012_", "hc114_", "hc115_", "ph088_", "ph090_", "ph091_", "ac035_", "ph006_", "ph011_", "ph048_", "ph049_", "ph089_", "ph059_", "ph008_", "ph087_", "ph690_", "ph074_")
){
  # Identify continuous variables
  var_continue <- domain_mapping %>%
    filter(domain %in% continue_filter) %>%
    pull(variable)
  var_continue <- setdiff(var_continue, c(var_filter, unlabel_filter))
  var_continue <- c(var_continue, "bmi")
  
  # Identify categorical variables
  var_categ <- setdiff(names(dataset), c(var_continue, var_filter, var_euro))
  ds_categ <- dataset %>% select(all_of(var_categ))
  
  # Map domains to categorical variables
  categ_domains <- sapply(ds_categ, get_unlabel_domain)
  
  domain_mapping <- tibble(
    variable = names(categ_domains),
    domain = categ_domains
  ) %>%
    mutate(domain_size = sapply(strsplit(domain, "/"), length))
  
  # Identify ordered variables
  var_ord <- filter(domain_mapping, domain_size > 2) %>%
    pull(variable)
  var_ord <- setdiff(var_ord, not_ord_filter)
  
  # Identify non-ordered variables
  var_not_ord <- setdiff(names(ds_categ), var_ord)
  
  # Return list of variable types
  return(list(
    var_continue = var_continue, 
    var_euro = var_euro, 
    var_ord = var_ord, 
    var_not_ord = var_not_ord
  ))
}

# function to check the percentage of -1 value in variables
negative_perc <- function(dataset) {
  # Calculate the percentage of -1 values for each column
  perc_minus1 <- sapply(dataset, function(column) {
    round(mean(as.factor(column) == -1, na.rm = TRUE) * 100, 2)
  })
  
  # Convert the result to a list
  p <- as.list(perc_minus1)
  
  # Return the list of percentages
  return(p)
}

# Function to remove categorical variables from the dataset if the second highest frequency is less than or equal to an alpha threshold
alpha_removing <- function(dataset, variables, alpha = 0.05) {
  # Identify categorical variables
  var_categ <- setdiff(names(dataset), variables)
  ds_categ <- dataset %>% select(all_of(var_categ))
  
  # Initialize a list to store percentage information and a vector to store variables to be removed
  info_perc <- list()
  to_be_remove <- c()
  
  # Iterate over each categorical variable
  for (var_name in names(ds_categ)) {
    # Calculate the frequency of each level
    freq <- table(ds_categ[[var_name]])
    freq_ord <- sort(freq, decreasing = TRUE)
    perc <- freq_ord / sum(freq_ord)
    
    # Store the percentage information
    info_perc[[var_name]] <- perc
    
    # Determine if the variable should be removed
    if (length(perc) > 1 && perc[2] <= alpha) {
      info_perc[[var_name]]$to_be_remove <- TRUE
      to_be_remove <- c(to_be_remove, var_name)
    } else {
      info_perc[[var_name]]$to_be_remove <- FALSE
    }
  }
  
  # Remove the identified variables from the dataset
  dataset <- dataset %>% select(-all_of(to_be_remove))
  
  # Return the modified dataset
  return(dataset)
}


# Function for another possible interpretation of class euro_d 
update_depression_var <- function(dataset) {
  dataset <- dataset %>%
    mutate(euro_d =  ifelse(initial_euro_d == "no" & euro_d == "yes", "yes",
                            ifelse(initial_euro_d == "no" & euro_d == "no", "no", NA))) %>%
    filter(!is.na(euro_d))
  return(dataset)
}
