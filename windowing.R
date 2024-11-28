# Function to dynamically classify euro_d and initial_euro_d based on thresholds
# `dep_ones` and `not_dep_ones` and handle missing values.
dinamic_classification_euro_score_dataset <- function(data, dep_ones, not_dep_ones) {
  
  # Classify initial_euro_d as "yes" or "no" based on dep_ones and not_dep_ones
  data <- data %>%
    mutate(
      initial_euro_d = case_when(
        initial_euro_d >= dep_ones ~ "yes",  # Mark as "yes" if greater than or equal to dep_ones
        initial_euro_d < not_dep_ones & initial_euro_d != -1 ~ "no",  # Mark as "no" if less than not_dep_ones
        TRUE ~ NA_character_  # Assign NA if neither condition is met
      ),
      # Add labels to `initial_euro_d` for clarity
      initial_euro_d = labelled(initial_euro_d, labels = c("depressed" = "yes", "not depressed" = "no"), label = "Depression at BaseLine")
    ) 
  
  # Handle missing values for different data types
  data <- data %>%
    mutate(across(where(is.integer), ~if_else(is.na(.), as.integer(-1), .)),
           across(where(is.double), ~if_else(is.na(.), as.double(-1), .)),
           across(where(is.character), ~if_else(is.na(.), "-1", .)))
  
  # Classify euro_d as "yes" or "no" based on dep_ones and not_dep_ones
  data <- data %>%
    mutate(
      euro_d = case_when(
        euro_d >= dep_ones ~ "yes",  # Mark as "yes" if greater than or equal to dep_ones
        euro_d < not_dep_ones ~ "no",  # Mark as "no" if less than not_dep_ones
        TRUE ~ NA_character_  # Assign NA if neither condition is met
      ),
      # Add labels to `euro_d` for clarity
      euro_d = labelled(euro_d, labels = c("depressed" = "yes", "not depressed" = "no"), label = "Depression")
    )
  
  return(data)  # Return the processed dataset
}

# Initialize variables for storing results
final_results <- list()  # Store final results for each dep-not_dep combination
n_generations <- 50  # Number of generations for genetic algorithm
pop_size <- 50  # Population size for genetic algorithm

# Loop over combinations of dep_ones and not_dep_ones
for (dep in 2:12) {
  for (not_dep in 1:dep) { 
    # Dynamically classify the dataset
    data <- dinamic_classification_euro_score_dataset(dataset, dep_ones = dep, not_dep_ones = not_dep)
    data <- data %>% filter(!is.na(euro_d))  # Remove rows with NA in euro_d
    
    # Check criteria for minimum row count and class balance
    if (nrow(data) > 1500 && all(table(data$euro_d) >= 600)) {
      model_types <- c("rf")  # Define the model type (e.g., Random Forest)
      seed <- 123  # Set random seed for reproducibility
      N <- 5  # Number of iterations for cross-validation
      k_folds <- 10  # Number of folds for cross-validation
      balance <- "under"  # Define the balancing strategy
      set.seed(seed)  # Set seed
      
      results <- list()  # Store results for each fold
      best_N_accuracy <- 0  # Track the best accuracy across folds
      best_N_features <- NA  # Track the best features across folds
      
      for (n in 1:N) {  
        folds <- createFolds(data$euro_d, k = k_folds)  # Generate cross-validation folds
        
        for (model_type in model_types) {
          best_fold_accuracy <- 0  # Track best accuracy for the current fold
          best_fold_features <- NULL  # Track best features for the current fold
          
          for (fold_idx in seq_along(folds)) {
            all_features <- setdiff(names(data), 'euro_d')  # Exclude target variable
            train_index <- -folds[[fold_idx]]  # Training set indices
            test_index <- folds[[fold_idx]]  # Test set indices
            
            # Train genetic algorithm for feature selection
            ga_model <- ga(
              type = "binary",
              fitness = function(selected_features) {
                res <- fitness_function_bl_under(selected_features, data, train_index, test_index, model_type, fold_idx)
                return(res$accuracy)  # Fitness is based on accuracy
              },
              nBits = length(all_features),  # Number of bits for genetic algorithm
              maxiter = n_generations,  # Maximum number of generations
              popSize = pop_size,  # Population size
              names = all_features,  # Feature names
              run = 50,  # Maximum iterations without improvement
              keepBest = TRUE  # Keep the best solution
            )
            
            best_solution <- ga_model@solution[1, ]  # Extract the best solution
            selected_features <- all_features[which(best_solution == 1)]  # Get selected features
            
            # Evaluate the solution
            res <- fitness_function_bl_under(best_solution, data, train_index, test_index, model_type, fold_idx)
            accuracy <- res$accuracy
            sensitivity <- res$sensitivity
            specificity <- res$specificity
            
            # Store results for this fold
            results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Accuracy", Value = accuracy)))
            results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Sensitivity", Value = sensitivity)))
            results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Specificity", Value = specificity)))
          
            # Update the best fold results
            if (accuracy > best_fold_accuracy) {
              best_fold_accuracy <- accuracy
              best_fold_features <- selected_features
            }
          }
        }
        # Update the best results across all folds
        if (best_fold_accuracy > best_N_accuracy) {
          best_N_accuracy <- best_fold_accuracy
          best_N_features <- best_fold_features
        }
      }
      
      # Aggregate results and calculate averages
      combined_results <- do.call(rbind, results)
      avg_accuracy <- mean(combined_results$Value[combined_results$Indicator == "Accuracy"], na.rm = TRUE)
      avg_sensitivity <- mean(combined_results$Value[combined_results$Indicator == "Sensitivity"], na.rm = TRUE)
      avg_specificity <- mean(combined_results$Value[combined_results$Indicator == "Specificity"], na.rm = TRUE)
      
      # Save the results for the current dep-not_dep combination
      final_results[[paste0("dep_", dep, "_not_dep_", not_dep)]] <- list(
        Accuracy = avg_accuracy,
        Sensitivity = avg_sensitivity,
        Specificity = avg_specificity,
        SelectedFeatures = best_N_features
      )
      
    } else {
      # If criteria are not met, save empty results
      final_results[[paste0("dep_", dep, "_not_dep_", not_dep)]] <- list(
        Accuracy = NA,
        Sensitivity = NA,
        Specificity = NA,
        SelectedFeatures = NA
      )
    }
  }
}

# Convert final_results into a data frame with detailed descriptions
results_df <- do.call(rbind, lapply(names(final_results), function(name) {
  # Extract dep and not_dep values from the name
  dep_value <- as.numeric(sub(".*dep_(\\d+)_not_dep_.*", "\\1", name))
  not_dep_value <- as.numeric(sub(".*not_dep_(\\d+)", "\\1", name))
  
  # Create a readable description of the combination
  combination_description <- paste0("euro_d >= ", dep_value, " ~ yes, euro_d < ", not_dep_value, " ~ no")
  
  # Create a row with the combination and metrics
  data.frame(
    Combination = combination_description,
    Accuracy = final_results[[name]]$Accuracy,
    Sensitivity = final_results[[name]]$Sensitivity,
    Specificity = final_results[[name]]$Specificity
  )
}))

# Print the results in the new format
print(results_df)
