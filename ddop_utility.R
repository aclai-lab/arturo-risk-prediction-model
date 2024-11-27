##################################################################
# UTILITY FUNCTIONS ##############################################
##################################################################

##################################################################
# find_variable_by_label #########################################
##################################################################
#' Find variable code by variable label
#'
#' This function searches for a variable in the dataset by its label and returns the variable code (name) 
#' if the label is found. It checks the label attribute of each variable in the dataset.
#'
#' @param data A data frame containing the dataset.
#' @param label A string representing the label to search for.
#' 
#' @return The variable name (code) corresponding to the provided label. If no match is found, it returns NULL.
#' 
#' @examples
#' # Example usage:
#' find_variable_by_label(data, "Age")
#'
find_variable_by_label <- function(data, label) {
  # Loop through each variable in the dataset
  for (var_name in names(data)) {
    # Obtain the label attribute of the variable
    var_label <- attr(data[[var_name]], "label")
    
    # Check if the label matches the provided label
    if (!is.null(var_label) && any(var_label == label)) {
      return(var_name)  # Return the variable name if a match is found
    }
  }
  return(NULL)  # Return NULL if no match is found
}

##################################################################
# get_label_by_variable #########################################
##################################################################
#' Get variable label by variable code
#'
#' This function retrieves the label (if available) associated with a specific variable in the dataset.
#'
#' @param data A data frame containing the dataset.
#' @param variable_name A string representing the name of the variable to retrieve the label for.
#' 
#' @return The label of the variable. If the variable does not have a label, a message is returned.
#' 
#' @examples
#' # Example usage:
#' get_label_by_variable(data, "age")
#'
get_label_by_variable <- function(data, variable_name) {
  # Check if the variable exists in the dataset
  if (!variable_name %in% names(data)) {
    stop("The specified variable does not exist in the dataset.")
  }
  
  # Obtain the label attribute of the variable
  variable_label <- attr(data[[variable_name]], "label")
  
  # Check if the variable has a label
  if (is.null(variable_label)) {
    return("The variable does not have a label attribute.")
  } 
  
  return(variable_label)  # Return the variable label
}

##################################################################
# get_labels_by_variable #########################################
##################################################################
#' Get value labels of a variable by its code
#'
#' This function retrieves the value labels (if available) associated with a specific variable in the dataset.
#' The function returns a data frame with the value and corresponding label for each value.
#'
#' @param dataset A data frame containing the dataset.
#' @param var_name A string representing the name of the variable to retrieve the value labels for.
#' 
#' @return A data frame with two columns: value and label. If the variable has no labels, a message is returned.
#' 
#' @examples
#' # Example usage:
#' get_labels_by_variable(data, "age_group")
#'
get_labels_by_variable <- function(dataset, var_name) {
  # Check if the variable exists in the dataset
  if (!var_name %in% names(dataset)) {
    stop("The variable name provided does not exist in the dataset.")
  }
  
  # Retrieve the variable from the dataset
  var <- dataset[[var_name]]
  
  # Check if the variable has value labels
  if (is.null(attr(var, "labels"))) {
    return("The variable does not have labels attribute.")
  }
  
  # Get the labels and return them in a data frame
  labels <- attr(var, "labels")
  labels_df <- data.frame(value = as.numeric(labels), label = names(labels), stringsAsFactors = FALSE)
  
  return(labels_df)
}

##################################################################
# get_domain #####################################################
##################################################################
#' Get domain of a labelled variable column
#'
#' This function retrieves the domain (value labels) of a labelled variable column.
#' It assumes that the variable is labelled and returns the labels in a string format, separated by "/".
#'
#' @param column A labelled column from the dataset.
#'
#' @return A string containing the domain values (labels) separated by "/". If the variable is not labelled, it returns an empty string.
#'
#' @examples
#' # Example usage:
#' get_domain(dataset$age_group)
get_domain <- function(column) {
  if (is.labelled(column)){
    domain <- val_labels(column)
    return(paste(domain, collapse = "/"))  # Concatenate labels with "/"
  } else {
    return("")  # Return empty string if not labelled
  }
}

##################################################################
# get_unlabel_domain #############################################
##################################################################
#' Get domain of an unlabelled variable column
#'
#' This function retrieves the unique values of an unlabelled variable and returns them in a string format, 
#' separated by "/".
#'
#' @param column An unlabelled column from the dataset.
#'
#' @return A string containing the unique values of the variable, separated by "/".
#'
#' @examples
#' # Example usage:
#' get_unlabel_domain(dataset$income)
get_unlabel_domain <- function(column) {
  unique_values <- sort(unique(na.omit(column)))  # Remove NAs and get unique sorted values
  return(paste(unique_values, collapse = "/"))  # Concatenate values with "/"
}

##################################################################
# get_domain_map #################################################
##################################################################
#' Get domain mapping for all variables in a dataset
#'
#' This function creates a domain mapping for all variables in a dataset by checking if each variable
#' is labelled or unlabelled, and then retrieves the domain values.
#'
#' @param dataset A data frame containing the dataset.
#'
#' @return A tibble with two columns: 'variable' and 'domain', mapping each variable to its domain values.
#'
#' @examples
#' # Example usage:
#' get_domain_map(dataset)
get_domain_map <- function(dataset){
  
  column_names <- names(dataset)
  
  # Loop through each variable and get the domain (labelled or unlabelled)
  domains <- sapply(column_names, function(column_name) {
    column_data <- dataset[[column_name]]
    
    if (column_name == "ac012_") {  # Special case for a specific variable
      get_unlabel_domain(column_data)
    } else {
      get_domain(column_data)
    }
  })
  
  # Create a tibble to map variables to their domains
  domain_mapping <- tibble(
    variable = names(domains),
    domain = domains
  )
  
  return(domain_mapping)
}

##################################################################
# check_domain ###################################################
##################################################################
#' Check the domain of a variable column
#'
#' This function checks the unique values of a column and returns them in a string format. If the values 
#' are numeric and sequential, it returns a range in the format "min:max". Otherwise, it returns the unique values separated by "/".
#'
#' @param column A column from the dataset.
#'
#' @return A string representing the domain of the variable, either as a range or unique values separated by "/".
#'
#' @examples
#' # Example usage:
#' check_domain(dataset$age)
check_domain <- function(column) {
  unique_values <- sort(unique(na.omit(column)))  # Remove NAs and get unique sorted values
  
  # If numeric and sequential, return range as "min:max"
  if (is.numeric(unique_values)) {
    if (length(unique_values) > 1 && all(diff(unique_values) == 1)) {
      return(paste(range(unique_values), collapse = ":")) 
    } else {
      return(paste(unique_values, collapse = "/"))
    }
  } else {
    return(paste(unique_values, collapse = "/"))  # Return unique values for non-numeric variables
  }
}

##################################################################
# is_sequential ##################################################
##################################################################
#' Check if a domain is sequential
#'
#' This function checks whether a given domain string represents a sequential range (e.g., "min:max").
#'
#' @param domain A string representing the domain of a variable.
#'
#' @return TRUE if the domain is sequential (i.e., contains ":"), otherwise FALSE.
#'
#' @examples
#' # Example usage:
#' is_sequential("1:10")
is_sequential <- function(domain) {
  str_detect(domain, pattern = ":")  # Check if the domain contains ":"
}

##################################################################
# update_labels ##################################################
##################################################################
#' Update labels for a variable, keeping -1 as "missing"
#'
#' This function removes all negative labels except for -1, which is kept and labeled as "missing".
#' It updates the labels of the variable to remove any negative values and ensure that -1 is properly labeled.
#'
#' @param column A labelled column from the dataset.
#'
#' @return The updated column with the modified labels.
#'
#' @examples
#' # Example usage:
#' dataset$age <- update_labels(dataset$age)
update_labels <- function(column) {
  if (is.labelled(column)) {
    labels <- attr(column, "labels")
    
    # Filter the labels to remove those with negative values
    positive_labels <- labels[labels >= 0]
    
    # Add a new label for -1 as "missing"
    # Ensure that -1 is consistent with the data type of the column
    if (is.integer(column)) {
      # If the data is integer, ensure the new label is integer
      new_labels <- c(setNames(as.integer(-1), "missing"), positive_labels)
    } else {
      # Otherwise, treat the data as double (default)
      new_labels <- c(setNames(as.double(-1), "missing"), positive_labels)
    }
    
    # Update the labels attribute with the new labels
    attr(column, "labels") <- new_labels
  }
  
  return(column)  # Return the updated column
}


##################################################################
# update_labels_2 #################################################
##################################################################
#' Update labels for a variable, removing all negative labels
#'
#' This function removes all negative labels from the variable, including -1.
#'
#' @param column A labelled column from the dataset.
#'
#' @return The updated column with negative labels removed.
#'
#' @examples
#' # Example usage:
#' dataset$income <- update_labels_2(dataset$income)
update_labels_2 <- function(column) {
  if (is.labelled(column)) {
    labels <- attr(column, "labels")
    
    # Filter the labels to remove those with negative values
    new_labels <- labels[labels >= 0]
    
    # Update the labels attribute with the new labels
    attr(column, "labels") <- new_labels
  }
  
  return(column)  # Return the updated column
}

##################################################################
# create_var_occ #################################################
##################################################################
#' Count and list variable occurrences in a dataset
#'
#' This function creates a list of occurrences for each variable in the dataset. For each variable, it calculates 
#' the number of times each level (domain value) occurs. It uses the domain of the variable to determine the levels.
#'
#' @param dataset A data frame containing the dataset.
#' @param type A string indicating the type of variable ("continue" for continuous variables, otherwise categorical).
#'
#' @return A list where each element contains the frequency table of a variable, with the variable name as the key.
#'
#' @examples
#' # Example usage:
#' var_occurrences <- create_var_occ(dataset, type = "categorical")
create_var_occ <- function(dataset, type = "") {
  var_occ <- list()  # Initialize an empty list to store occurrences
  
  # Loop through each variable in the dataset
  for (var in names(dataset)) {
    
    # Get the domain for the variable based on its type (continuous or categorical)
    if (type == "continue") {
      var_domain <- get_unlabel_domain(dataset[[var]])  # Use unlabelled domain for continuous variables
    } else {
      var_domain <- get_domain(dataset[[var]])  # Use labelled domain for categorical variables
    }
    
    # If no domain is found, fall back to unlabelled domain
    if (var_domain == "") var_domain <- get_unlabel_domain(dataset[[var]])
    
    # Split the domain string into individual levels
    val_domain <- strsplit(var_domain, "/")[[1]]
    
    # Create a factor with the specified levels and compute the occurrence table
    occ <- table(factor(dataset[[var]], levels = val_domain))
    
    # Store the occurrence table in the list, using the variable name as the key
    var_occ[[var]] <- occ
  }
  
  return(var_occ)  # Return the list of occurrences
}


##################################################################
# calculate_statistics ###########################################
##################################################################
#' Calculate basic statistics, variance, and standard deviation for each sequential variable
#'
#' This function calculates basic statistics (e.g., min, 1st quartile, median, mean, 3rd quartile, max),
#' variance, and standard deviation for each sequential variable in the dataset. It handles missing values
#' by excluding them from the calculations.
#'
#' @param variables_df A data frame containing the list of variables to calculate statistics for.
#' @param data_df A data frame containing the actual dataset with the variables.
#'
#' @return A data frame (tibble) where each row contains the statistics for a variable, including
#' minimum value, quartiles, median, mean, maximum value, variance, and standard deviation.
#'
#' @examples
#' # Example usage:
#' summary_stats <- calculate_statistics(variables_df, data_df)
calculate_statistics <- function(variables_df, data_df) {
  # Pull the 'variable' column from the variables_df and map over each variable
  variables_df %>%
    pull(variable) %>%
    map_df(~{
      selected_col <- data_df[[.x]]  # Select the column corresponding to the current variable
      
      # Check if the column is empty or contains only NA values
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
      
      # Calculate basic statistics using the summary() function
      base_stats <- summary(selected_col)
      
      # Calculate variance and standard deviation, ignoring NA values
      variance <- var(selected_col, na.rm = TRUE)
      st_dev <- sd(selected_col, na.rm = TRUE)
      
      # Create and return a tibble with all the calculated statistics for the variable
      tibble(
        Variable = .x,
        Min = base_stats[1],       # Minimum value
        `1st Qu.` = base_stats[2],  # 1st Quartile
        Median = base_stats[3],     # Median
        Mean = base_stats[4],       # Mean
        `3rd Qu.` = base_stats[5],  # 3rd Quartile
        Max = base_stats[6],        # Maximum value
        Variance = variance,        # Variance
        St_Dev = st_dev             # Standard Deviation
      )
    })
}


##################################################################
# var_na_perc ####################################################
##################################################################
#' Calculate the percentage of NA values and remaining variables after threshold
#'
#' This function calculates the percentage of missing values (NA) for each column in the dataset and 
#' then iteratively checks how many columns remain when applying different thresholds for the maximum allowed NA percentage.
#' It returns a tibble showing how many columns would remain for each threshold.
#'
#' @param dataset A data frame containing the dataset.
#'
#' @return A tibble showing the percentage threshold and the number of columns that remain after applying the threshold.
#'
#' @examples
#' # Example usage:
#' var_na_perc(dataset)
var_na_perc <- function(dataset) {
  # Calculate the percentage of NA values for each column in the dataset
  na_perc <- round(colSums(is.na(dataset)) / nrow(dataset) * 100, 2)
  
  # Initialize an empty tibble to store the results
  wo_na <- tibble(
    perc = character(),  # Column for percentage threshold (e.g., "5%")
    remains = integer()  # Column for the number of remaining variables
  )
  
  # Iterate over thresholds from 5% to 95% in steps of 5%
  for (i in seq(from = 5, to = 95, by = 5)) {
    # Count how many columns have a NA percentage greater than or equal to the current threshold
    var_na <- sum(na_perc >= i)
    
    # Add a row to the tibble with the threshold and the number of remaining columns
    wo_na <- wo_na %>%
      add_row(
        perc = paste(i, "%", sep = ""),  # Append "%" to the threshold value
        remains = (length(names(dataset)) - var_na)  # Remaining columns after excluding those with NA >= threshold
      )
  }
  
  # Return the result tibble
  return(wo_na)
}

##################################################################
# get_var_type ####################################################
##################################################################
# Function to get variables types (continuos, categorial..)
#' Identify variable types (continuous, categorical, ordered, etc.)
#'
#' This function classifies variables in a dataset into different types: continuous, categorical, ordered, 
#' and non-ordered. It uses a combination of filters and domain mapping to categorize the variables.
#'
#' @param dataset A data frame containing the dataset.
#' @param domain_mapping A data frame that maps variables to their domains.
#' @param var_filter A vector of variable names to exclude (e.g., IDs, household identifiers) (default provided).
#' @param unlabel_filter A vector of variable names that are unlabelled (default provided).
#' @param var_euro A vector of Euro-related variable names (default provided).
#' @param continue_filter A vector of domain values that identify continuous variables (default provided).
#' @param not_ord_filter A vector of variable names that should be treated as non-ordered (default provided).
#'
#' @return A list with four components:
#' \describe{
#'   \item{var_continue}{A vector of continuous variables.}
#'   \item{var_euro}{A vector of Euro-related variables.}
#'   \item{var_ord}{A vector of ordered categorical variables.}
#'   \item{var_not_ord}{A vector of non-ordered categorical variables.}
#' }
#'
#' @examples
#' # Example usage:
#' var_types <- get_var_type(dataset, domain_mapping)
get_var_type <- function(
    dataset, 
    domain_mapping,
    var_filter = c("mergeid", "wave", "hhid6", "mergeidp6", "coupleid6", "hhid5", "mergeidp5", "coupleid5", "hhid7", "mergeidp7", "coupleid7", "hhsize"),
    unlabel_filter = c("ac012_"),
    var_euro = c("euro1", "euro2", "euro3", "euro4", "euro5", "euro6", "euro7", "euro8", "euro9", "euro10", "euro11", "euro12"),
    continue_filter = c("-9/-2/-1", "-2/-1", "-3/-2/-1", "-9999992/-9999991"),
    not_ord_filter = c("country", "language", "ph054_", "ph066_", "ph071_1", "ph071_2", "ph071_3", "ph071_4", "ph072_3", "ph076_1", "ph076_2", "ph076_3", "ph076_4", "ph077_1",  "ph077_2", "ph077_3", "ph077_4", "hc029_", "ph075_1", "ph075_3", "ep005_", "isced1997_r", "dn014_", "dn034_", "iv009_", "sp002_", "sp008_", "phactiv", "ph004_", "ph041_", "ph044_", "ph050_", "ph061_", "ph065_", "ph082_", "ph084_", "ph745_", "hc012_", "hc114_", "hc115_", "ph088_", "ph090_", "ph091_", "ac035_", "ph006_", "ph011_", "ph048_", "ph049_", "ph089_", "ph059_", "ph008_", "ph087_", "ph690_", "ph074_")
) {
  # Identify continuous variables based on the domain mapping and filters
  var_continue <- domain_mapping %>%
    filter(domain %in% continue_filter) %>%
    pull(variable)
  
  var_continue <- setdiff(var_continue, c(var_filter, unlabel_filter))  # Remove filtered variables
  var_continue <- c(var_continue, "bmi")  # Manually include BMI as continuous
  
  # Identify categorical variables by excluding continuous, Euro-related, and filtered variables
  var_categ <- setdiff(names(dataset), c(var_continue, var_filter, var_euro))
  
  # Subset the dataset to categorical variables
  ds_categ <- dataset %>% select(all_of(var_categ))
  
  # Get the domain for each categorical variable
  categ_domains <- sapply(ds_categ, get_unlabel_domain)
  
  # Create a domain mapping for categorical variables
  domain_mapping <- tibble(
    variable = names(categ_domains),
    domain = categ_domains
  ) %>%
    mutate(domain_size = sapply(strsplit(domain, "/"), length))  # Calculate the size of the domain
  
  # Identify ordered variables (those with more than 2 levels)
  var_ord <- filter(domain_mapping, domain_size > 2) %>%
    pull(variable)
  
  var_ord <- setdiff(var_ord, not_ord_filter)  # Exclude non-ordered variables
  
  # Identify non-ordered variables
  var_not_ord <- setdiff(names(ds_categ), var_ord)
  
  # Return a list of variable types
  return(list(
    var_continue = var_continue,  # Continuous variables
    var_euro = var_euro,          # Euro-related variables
    var_ord = var_ord,            # Ordered categorical variables
    var_not_ord = var_not_ord     # Non-ordered categorical variables
  ))
}


##################################################################
# negative_perc ##################################################
##################################################################
#' Calculate the percentage of -1 values in each variable
#'
#' This function calculates the percentage of -1 values for each column in a dataset. It helps to identify 
#' variables where -1 represents missing or invalid data, providing insight into the distribution of such values.
#'
#' @param dataset A data frame containing the dataset.
#'
#' @return A list where each element corresponds to a variable, and the value represents the percentage of -1 values in that variable.
#'
#' @examples
#' # Example usage:
#' negative_perc(dataset)
negative_perc <- function(dataset) {
  # Calculate the percentage of -1 values for each column in the dataset
  perc_minus1 <- sapply(dataset, function(column) {
    round(mean(as.factor(column) == -1, na.rm = TRUE) * 100, 2)  # Calculate percentage and round to 2 decimal places
  })
  
  # Convert the result to a list
  p <- as.list(perc_minus1)
  
  # Return the list of percentages
  return(p)
}


##################################################################
# alpha_removing ####################################################
##################################################################
#' Remove categorical variables based on the second highest frequency threshold
#'
#' This function removes categorical variables from a dataset if the second most frequent level 
#' in the variable has a frequency percentage less than or equal to a specified threshold (alpha).
#' It is used to eliminate variables that lack meaningful variability.
#'
#' @param dataset A data frame containing the dataset.
#' @param variables A vector of variable names to exclude from the analysis (e.g., non-categorical variables).
#' @param alpha A numeric value representing the frequency threshold for removal (default is 0.05).
#'
#' @return A modified data frame with categorical variables removed based on the alpha threshold.
#'
#' @examples
#' # Example usage:
#' dataset_cleaned <- alpha_removing(dataset, variables, alpha = 0.05)
alpha_removing <- function(dataset, variables, alpha = 0.05) {
  # Identify categorical variables by excluding the ones in the variables vector
  var_categ <- setdiff(names(dataset), variables)
  ds_categ <- dataset %>% select(all_of(var_categ))
  
  # Initialize a list to store percentage information and a vector to store variables to be removed
  info_perc <- list()
  to_be_remove <- c()
  
  # Iterate over each categorical variable
  for (var_name in names(ds_categ)) {
    # Calculate the frequency of each level in the categorical variable
    freq <- table(ds_categ[[var_name]])
    freq_ord <- sort(freq, decreasing = TRUE)  # Sort frequencies in decreasing order
    perc <- freq_ord / sum(freq_ord)  # Calculate the percentage for each level
    
    # Store the percentage information for the variable
    info_perc[[var_name]] <- perc
    
    # Check if the second most frequent level has a percentage less than or equal to the alpha threshold
    if (length(perc) > 1 && perc[2] <= alpha) {
      info_perc[[var_name]]$to_be_remove <- TRUE
      to_be_remove <- c(to_be_remove, var_name)  # Mark the variable for removal
    } else {
      info_perc[[var_name]]$to_be_remove <- FALSE  # Keep the variable if it doesn't meet the removal condition
    }
  }
  
  # Remove the identified variables from the dataset
  dataset <- dataset %>% select(-all_of(to_be_remove))
  
  # Return the modified dataset
  return(dataset)
}

##################################################################
# update_depression_var ##########################################
################################################################## 
#' Update depression variable based on initial and current depression status
#'
#' This function updates the `euro_d` variable in the dataset based on the initial depression status (`initial_euro_d`). 
#' If the initial status is "no" and the current status is "yes", the `euro_d` value is updated to "yes". 
#' If both the initial and current statuses are "no", the `euro_d` value is updated to "no". 
#' All other cases are set to `NA` and filtered out from the dataset.
#'
#' @param dataset A data frame containing the dataset with the `initial_euro_d` and `euro_d` variables.
#'
#' @return A modified data frame where the `euro_d` variable is updated and rows with `NA` values in `euro_d` are removed.
#'
#' @examples
#' # Example usage:
#' updated_dataset <- update_depression_var(dataset)
update_depression_var <- function(dataset) {
  # Update the 'euro_d' variable based on conditions for 'initial_euro_d' and 'euro_d'
  dataset <- dataset %>%
    mutate(
      euro_d = ifelse(initial_euro_d == "no" & euro_d == "yes", "yes",
                      ifelse(initial_euro_d == "no" & euro_d == "no", "no", NA))
    ) %>%
    # Filter out rows where 'euro_d' is NA
    filter(!is.na(euro_d))
  
  # Return the modified dataset
  return(dataset)
}


##################################################################
# score_train_model ##############################################
##################################################################
#' Train a regression model based on the specified model type
#'
#' This function trains a regression model (rpart, randomForest, or glm) using the provided training data 
#' to predict the `euro_d` variable. The model type is selected through the `model_type` argument.
#'
#' @param model_type A string specifying the type of model to train ("rpart", "rf", or "glm").
#' @param train_data A data frame containing the training data. The target variable is `euro_d`.
#'
#' @return A trained model object based on the specified model type.
#'
#' @examples
#' # Train an rpart model:
#' model <- score_train_model("rpart", train_data)
score_train_model <- function(model_type, train_data) {
  # Train the model based on the specified model type
  model <- switch(model_type,
                  rpart = rpart(euro_d ~ ., data = train_data, method = "anova", control = rpart.control(minsplit = 5, minbucket = 5, xval=0)),   # Regression tree with rpart
                  rf = randomForest(euro_d ~ ., data = train_data),                # Random forest regression
                  glm = glm(euro_d ~ ., family = gaussian(), data = train_data),   # Linear regression with glm
                  stop("Model type not supported for regression"))  # Error handling for unsupported model types
  
  return(model)  # Return the trained model
}


##################################################################
# score_make_predictions #########################################
##################################################################
#' Make predictions using a trained model
#'
#' This function generates predictions based on the provided test data using a trained model.
#' The prediction method is determined by the model type.
#'
#' @param model A trained model object (either rpart, randomForest, or glm).
#' @param test_data A data frame containing the test data for prediction.
#' @param model_type A string specifying the type of model used for prediction ("rpart", "rf", or "glm").
#'
#' @return A numeric vector of predicted values.
#'
#' @examples
#' # Generate predictions using a trained random forest model:
#' predictions <- score_make_predictions(model, test_data, "rf")
score_make_predictions <- function(model, test_data, model_type) {
  # Make predictions based on the specified model type
  predictions <- switch(model_type,
                        rpart = predict(model, test_data),  # Continuous predictions for rpart
                        rf = predict(model, test_data),     # Continuous predictions for randomForest
                        glm = predict(model, test_data),    # Continuous predictions for glm
                        stop("Model type not supported for predictions"))  # Error handling for unsupported models
  
  return(predictions)  # Return the predictions
}


##################################################################
# train_model ####################################################
##################################################################
#' Train a classification model based on the specified model type
#'
#' This function trains a classification model (rpart, randomForest, or glm) using the provided training data 
#' to predict the `euro_d` variable. The target variable `euro_d` is converted to a factor to handle classification tasks.
#'
#' @param model_type A string specifying the type of model to train ("rpart", "rf", or "glm").
#' @param train_data A data frame containing the training data. The target variable is `euro_d`.
#'
#' @return A trained model object based on the specified model type.
#'
#' @examples
#' # Train a random forest model:
#' model <- train_model("rf", train_data)
train_model <- function(model_type, train_data) {
  # Convert the target variable 'euro_d' to a factor for classification
  train_data$euro_d <- as.factor(train_data$euro_d)
  
  # Train the model based on the specified model type
  model <- switch(model_type,
                  rpart = rpart(euro_d ~ ., data = train_data, method = "class", control = rpart.control(minsplit = 5, minbucket = 5, xval=0)),  # Classification tree with rpart
                  rf = randomForest(euro_d ~ ., data = train_data, type = "classification"),  # Random forest classification
                  glm = glm(euro_d ~ ., family = binomial(), data = train_data),  # Logistic regression with glm
                  stop("Model type not supported"))  # Error handling for unsupported model types
  
  return(model)  # Return the trained model
}


##################################################################
# make_predictions ###############################################
##################################################################
#' Make predictions using a trained classification model
#'
#' This function generates probability predictions for the positive class (e.g., "yes") using a trained model.
#' The prediction method is chosen based on the model type.
#'
#' @param model A trained model object (either rpart, randomForest, or glm).
#' @param test_data A data frame containing the test data for prediction.
#' @param model_type A string specifying the type of model used for prediction ("rpart", "rf", or "glm").
#'
#' @return A numeric vector of predicted probabilities for the positive class.
#'
#' @examples
#' # Generate predictions using a trained glm model:
#' predictions <- make_predictions(model, test_data, "glm")
make_predictions <- function(model, test_data, model_type) {
  # Make predictions based on the specified model type
  predictions <- switch(model_type,
                        rpart = predict(model, test_data, type = "prob")[, 2],  # Probability predictions for rpart
                        rf = predict(model, test_data, type = "prob")[, 2],     # Probability predictions for randomForest
                        glm = predict(model, test_data, type = "response"),     # Probability predictions for glm
                        stop("Model type not supported for predictions"))  # Error handling for unsupported models
  
  return(predictions)  # Return the predictions for the positive class
}

##################################################################
# add_noise ######################################################
##################################################################
#' Add noise to numeric columns in a dataset
#'
#' This function adds random Gaussian noise to the numeric columns of a dataset. The level of noise can be controlled 
#' by the `noise_level` parameter. The noise is drawn from a normal distribution with mean 0 and standard deviation 
#' equal to `noise_level`.
#'
#' @param data A data frame containing the dataset.
#' @param noise_level A numeric value specifying the standard deviation of the noise to be added (default is 0.01).
#'
#' @return A modified data frame where noise has been added to the numeric columns.
#'
#' @examples
#' # Example usage:
#' noisy_dataset <- add_noise(data, noise_level = 0.05)
add_noise <- function(data, noise_level = 0.01) {
  # Copy the original data to preserve the input
  noisy_data <- data
  
  # Identify the numeric columns in the dataset
  numeric_columns <- sapply(data, is.numeric)
  
  # Add Gaussian noise to the numeric columns
  noisy_data[, numeric_columns] <- data[, numeric_columns] + 
    matrix(rnorm(nrow(data) * sum(numeric_columns), 0, noise_level), nrow = nrow(data))
  
  # Return the modified dataset with added noise
  return(noisy_data)
}


##################################################################
# calculate_metrics ##############################################
##################################################################
#' Calculate classification metrics at a given threshold
#'
#' This function calculates several classification metrics, including sensitivity, specificity, 
#' positive predictive value (PPV), negative predictive value (NPV), and accuracy based on a given 
#' threshold applied to predicted probabilities. The metrics are calculated using a confusion matrix.
#'
#' @param threshold A numeric value representing the threshold for converting predicted probabilities to binary classes.
#' @param predictions A numeric vector of predicted probabilities (values between 0 and 1).
#' @param actuals A factor vector of actual binary outcomes ("yes" or "no").
#'
#' @return A named vector containing the following metrics:
#' \describe{
#'   \item{Population_below_threshold}{The percentage of predictions below the threshold.}
#'   \item{Sensitivity}{The proportion of true positives correctly identified.}
#'   \item{Specificity}{The proportion of true negatives correctly identified.}
#'   \item{PPV}{Positive predictive value, the proportion of positive predictions that are true positives.}
#'   \item{NPV}{Negative predictive value, the proportion of negative predictions that are true negatives.}
#'   \item{Accuracy}{The proportion of correct predictions (both positive and negative).}
#' }
#'
#' @examples
#' # Example usage:
#' metrics <- calculate_metrics(threshold = 0.5, predictions = predicted_probs, actuals = actual_labels)
calculate_metrics <- function(threshold, predictions, actuals) {
  # Convert predictions to binary classes based on the threshold
  binary_predictions <- ifelse(predictions > threshold, "yes", "no")
  
  # Calculate the confusion matrix
  cm <- confusionMatrix(factor(binary_predictions, levels = c("no", "yes")),
                        factor(actuals, levels = c("no", "yes")))
  
  # Calculate the percentage of the population with predictions below the threshold
  population_below_threshold <- mean(predictions <= threshold) * 100
  
  # Extract the relevant metrics from the confusion matrix
  sensitivity <- cm$byClass["Sensitivity"]
  specificity <- cm$byClass["Specificity"]
  ppv <- cm$byClass["Pos Pred Value"]
  npv <- cm$byClass["Neg Pred Value"]
  accuracy <- cm$overall["Accuracy"]
  
  # Return the calculated metrics as a named vector
  return(c(Population_below_threshold = population_below_threshold,
           Sensitivity = sensitivity,
           Specificity = specificity,
           PPV = ppv,
           NPV = npv,
           Accuracy = accuracy))
}


##################################################################
# balancing_dataset ##############################################
##################################################################
#' Balance the dataset using oversampling or undersampling
#'
#' This function balances the dataset by either oversampling the minority class or undersampling the majority class 
#' based on the specified method. It aims to handle imbalanced datasets where one class (e.g., "yes" or "no" in `euro_d`) 
#' is significantly more frequent than the other.
#'
#' @param train_data A data frame containing the training dataset. The target variable is `euro_d`.
#' @param method A string specifying the balancing method. Options are "over" for oversampling or "under" for undersampling (default is "under").
#'
#' @return A balanced data frame with equal or nearly equal representation of both classes in the `euro_d` variable.
#'
#' @examples
#' # Example usage for oversampling:
#' balanced_data <- balancing_dataset(train_data, method = "over")
#'
#' # Example usage for undersampling:
#' balanced_data <- balancing_dataset(train_data, method = "under")
balancing_dataset <- function(train_data, method = "under") {
  
  # Oversampling method
  if (method == "over") {
    # Separate the minority and majority classes
    minor_class <- subset(train_data, euro_d == "yes")
    major_class <- subset(train_data, euro_d == "no")
    
    # Check if we need to swap the classes (if "no" is the minority class)
    if (nrow(minor_class) > nrow(major_class)) {
      minor_class <- subset(train_data, euro_d == "no")
      major_class <- subset(train_data, euro_d == "yes")
    }
    
    # Calculate the number of synthetic samples needed to match the size of the majority class
    num_samples_to_generate <- nrow(major_class) - nrow(minor_class)
    
    # Generate synthetic samples by oversampling the minority class
    synthetic_samples <- minor_class[sample(nrow(minor_class), num_samples_to_generate, replace = TRUE), ]
    
    # Optionally, add noise to the synthetic samples
    synthetic_samples <- add_noise(synthetic_samples)
    
    # Combine the original dataset with the synthetic samples
    balanced_train_data <- rbind(train_data, synthetic_samples)
    
    # Undersampling method
  } else if (method == "under") {
    # Separate the minority and majority classes
    minor_class <- subset(train_data, euro_d == "yes")
    major_class <- subset(train_data, euro_d == "no")
    
    # Check if we need to swap the classes (if "no" is the minority class)
    if (nrow(minor_class) > nrow(major_class)) {
      minor_class <- subset(train_data, euro_d == "no")
      major_class <- subset(train_data, euro_d == "yes")
    }
    
    # Undersample the majority class to match the size of the minority class
    major_samples <- major_class[sample(nrow(major_class), nrow(minor_class)), ]
    
    # Combine the undersampled majority class with the minority class
    balanced_train_data <- rbind(minor_class, major_samples)
  }
  
  # Return the balanced dataset
  return(balanced_train_data)
}


##################################################################
# print_summary_results ##########################################
##################################################################
#' Print summary statistics for model results
#'
#' This function takes a list of results (data frames), combines them, and calculates summary statistics 
#' (min, max, mean, standard deviation, and variance) for each model and indicator. The summary is printed to the console.
#'
#' @param results A list of data frames where each contains model evaluation metrics (e.g., Model, Indicator, Value).
#'
#' @return This function does not return any value. It prints the summary statistics.
#'
#' @examples
#' # Example usage:
#' print_summary_results(results_list)
print_summary_results <- function(results) {
  # Combine the list of result data frames into a single data frame
  final_results <- bind_rows(results)
  
  # Calculate summary statistics for each Model and Indicator
  summary_stats <- final_results %>%
    group_by(Model, Indicator) %>%
    summarise(
      Min = min(Value),         # Minimum value
      Max = max(Value),         # Maximum value
      Mean = mean(Value),       # Mean value
      SD = sd(Value),           # Standard deviation
      Var = var(Value)          # Variance
    )
  
  # Print the summary statistics
  print(summary_stats)
}

##################################################################
# print_score_summary_results ####################################
##################################################################
#' Print score summary results for model predictions
#'
#' This function computes a variety of classification metrics (accuracy, sensitivity, specificity, PPV, NPV, and Kappa) 
#' for different models across multiple folds. It summarizes the metrics by calculating the maximum, mean, minimum, 
#' standard deviation, and variance for each metric per model and prints the results.
#'
#' @param predictions A data frame containing the predictions. It should include columns for `PredictedValues`, `TrueLabel`, 
#' and the model's `Model` and `Fold`.
#'
#' @return This function does not return a value. It prints summary statistics for each model and metric.
#'
#' @examples
#' # Example usage:
#' print_score_summary_results(predictions_df)
print_score_summary_results <- function(predictions) {
  # Classify predictions based on a threshold of 4
  all_predictions <- predictions %>%
    mutate(
      PredictedClass = ifelse(PredictedValues >= 4, "yes", "no"),  # Classify predicted values
      TrueClass = ifelse(TrueLabel >= 4, "yes", "no"),  # Classify true values
      CorrectPrediction = ifelse(PredictedClass == TrueClass, 1, 0)  # Mark correct predictions
    )
  
  # Compute metrics by model and fold using confusion matrix
  metrics_by_model_fold_df <- all_predictions %>%
    group_by(Model, Fold) %>%
    do({
      conf_mat <- confusionMatrix(
        factor(.$PredictedClass),  # Predicted class
        factor(.$TrueClass),       # True class
        positive = "yes"           # Positive class is "yes"
      )
      
      # Extract and store key metrics
      data.frame(
        Accuracy = conf_mat$overall['Accuracy'],
        Sensitivity = conf_mat$byClass['Sensitivity'],
        Specificity = conf_mat$byClass['Specificity'],
        PPV = conf_mat$byClass['Pos Pred Value'],
        NPV = conf_mat$byClass['Neg Pred Value'],
        Kappa = conf_mat$overall['Kappa']
      )
    }) %>%
    ungroup()
  
  # Reshape the data into long format for easier summary calculation
  metrics_long <- metrics_by_model_fold_df %>%
    pivot_longer(
      cols = c(Accuracy, Sensitivity, Specificity, PPV, NPV, Kappa),  # Metrics to pivot
      names_to = "Metric",   # New column for metric names
      values_to = "Value"    # New column for metric values
    )
  
  # Summarize the metrics: calculate max, mean, min, standard deviation, and variance
  summary_stats_df <- metrics_long %>%
    group_by(Model, Metric) %>%
    summarize(
      Mean = mean(Value, na.rm = TRUE),
      Min = min(Value, na.rm = TRUE),
      Max = max(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      VAR = var(Value, na.rm = TRUE),
      .groups = 'drop'  # Remove groupings after summarizing
    ) %>%
    arrange(Model, Metric)  # Arrange the results by model and metric
  
  # Print the summarized statistics
  print(summary_stats_df)
}