############################################################################
# GENETIC SELECTION ########################################################
############################################################################

############################################################################
# fitness_function #########################################################
############################################################################
# Fitness Function for Feature Selection
# This function evaluates the performance of a given set of selected features on a dataset by training a specified model type and calculating performance metrics.
#
# Arguments:
# - selected_features: A logical vector indicating which features are selected for model training. The length of this vector should match the number of features in the dataset.
# - dataset: A data frame containing the dataset, including the target variable 'euro_d'.
# - train_index: Indices of the training set samples.
# - test_index: Indices of the test set samples.
# - model_type: A string specifying the type of model to train. Supported types are "rpart" (Decision Tree), "rf" (Random Forest), and "glm" (Generalized Linear Model).
# - fold_idx: An integer indicating the fold index during cross-validation (for tracking purposes).
#
# Returns:
# - A list containing:
#   1. accuracy: The accuracy of the model on the test set.
#   2. sensitivity: Sensitivity (True Positive Rate) of the model on the test set.
#   3. specificity: Specificity (True Negative Rate) of the model on the test set.
#   4. selected_features: The logical vector indicating which features were selected.
#   5. true_label: The true labels of the test data.
#   6. predicted_prob: The predicted probabilities for the positive class ("yes").
#   7. observations: The observed factor of the test data, used to compare against predictions.
#   8. PPV: Positive Predictive Value (Precision) of the model on the test set.
#   9. NPV: Negative Predictive Value of the model on the test set.
#   10. kappa: The kappa statistic measuring the agreement between predicted and observed classifications.
#
# Details:
# - The function first checks if any features are selected; if none are selected, it returns an accuracy of 0 and NA for other metrics.
# - The selected features are used to create a new dataset subset for training and testing.
# - The model is trained using the `train_model` function based on the specified `model_type`.
# - Predictions are made on the test set using `make_predictions`.
# - The confusion matrix is calculated to derive metrics such as accuracy, sensitivity, specificity, PPV, NPV, and kappa.
# - Returns a list of performance metrics and additional details about the model's predictions.
#
# Notes:
# - This function is designed for use in feature selection and model evaluation contexts, such as genetic algorithms or cross-validation.
fitness_function <- function(selected_features, dataset, train_index, test_index, model_type, fold_idx) {
  
  selected_features <- as.logical(selected_features)
  if(sum(selected_features) == 0) 
    return(list(accuracy = 0, sensitivity = NA, specificity = NA, selected_features = selected_features))
  
  features <- names(dataset)[selected_features]
  current_data <- dataset[, c(features, 'euro_d')]
  train_data <- current_data[train_index, ]
  test_data <- current_data[test_index, ]
  
  model <- train_model(model_type, train_data)
  
  predictions <- make_predictions(model, test_data, model_type)
  observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
  cm <- confusionMatrix(factor(ifelse(predictions > 0.5, "yes", "no"), levels = c("no", "yes")), observed_factor)
  
  accuracy <- cm$overall['Accuracy']
  sensitivity <- cm$byClass['Sensitivity']
  specificity <- cm$byClass['Specificity']
  kappa <- cm$overall['Kappa']
  PPV <- cm$byClass['Pos Pred Value']
  NPV <- cm$byClass['Neg Pred Value']
  
  return(list(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity, selected_features = selected_features, true_label = test_data$euro_d, predicted_prob = predictions, observations = observed_factor, PPV = PPV, NPV = NPV, kappa = kappa))
}

############################################################################
# genetic_feature_selection ################################################
############################################################################
# Genetic Feature Selection using Genetic Algorithm
# This function applies a genetic algorithm (GA) to select the best subset of features for a specified model type in the dataset. It evaluates different subsets of features across generations to optimize a fitness function based on model accuracy.
#
# Arguments:
# - dataset: A data frame containing the dataset, including the target variable 'euro_d'.
# - model_types: A character vector of model types to use for training. Supported values include "rpart" (Decision Tree), "rf" (Random Forest), and "glm" (Generalized Linear Model). Default is c("rpart", "rf", "glm").
# - n_generations: The number of generations for the genetic algorithm. Default is 50.
# - pop_size: The population size for each generation in the genetic algorithm. Default is 50.
#
# Returns:
# - A list containing:
#   1. best_features: A list of the best features selected for each model type.
#   2. best_accuracy: A list of the best accuracy achieved for each model type.
#   3. results: A list of data frames containing performance metrics (Accuracy, Sensitivity, Specificity, Kappa, PPV, NPV) for each fold of cross-validation.
#   4. thresholds_results: A data frame with performance metrics calculated at different probability thresholds (from 0.05 to 0.95).
#   5. ga_summaries: A list of summaries for the genetic algorithm run for each fold, including the best solution, fitness value, and iteration details.
#   6. all_predictions: A data frame with true labels and predicted probabilities for each fold and model type.
#
# Details:
# - The function performs 10-fold cross-validation for each specified model type.
# - For each fold, it uses a genetic algorithm to find the best subset of features that maximizes model accuracy:
#   - Uses the `ga` function from the GA package to create a binary GA, where each bit represents the inclusion (1) or exclusion (0) of a feature.
#   - The fitness function (`fitness_function`) calculates the accuracy of the model on the test set.
# - After GA optimization, the best subset of features for each fold is determined, and the model's performance metrics are calculated.
# - The function tracks the best overall accuracy and selected features across all folds for each model type.
# - Threshold metrics are calculated for a range of probability thresholds (from 0.05 to 0.95) to analyze the model's performance.
# Notes:
# - This function is computationally intensive due to the nested cross-validation and genetic algorithm optimization.
# - Set a random seed for reproducibility (`set.seed(1)`).
# - The `fitness_function` is called within the genetic algorithm to evaluate the selected features based on model accuracy.
#
# Example Usage:
# ```
# result <- genetic_feature_selection(dataset, model_types = c("rpart", "rf"), n_generations = 100, pop_size = 30)
# ```
genetic_feature_selection <- function(dataset, model_types = c("rpart", "rf", "glm"), n_generations = 50, pop_size = 50) {
  
  set.seed(1)
  best_overall_accuracy <- list()
  best_overall_features <- list()
  results <- list()
  thresholds_results <- data.frame()
  ga_summaries <- list()
  all_predictions <- data.frame()
  folds <- createFolds(dataset$euro_d, k = 10)
  thresholds <- seq(0.05, 0.95, by = 0.05)
  
  for (model_type in model_types) {
    
    best_fold_accuracy <- 0
    best_fold_features <- NULL
    best_overall_accuracy[[model_type]] <- -Inf
    
    for (fold_idx in seq_along(folds)) {
      
      all_features <- setdiff(names(dataset), 'euro_d')
      train_index <- -folds[[fold_idx]]
      test_index <- folds[[fold_idx]]
      
      ga_model <- ga(type = "binary",
                      fitness = function(selected_features) {
                                res <- fitness_function(selected_features, dataset, train_index, test_index, model_type, fold_idx)
                                return(res$accuracy) # Fitness è basata solo sull'accuratezza
                     },
                     nBits = length(all_features),
                     maxiter = n_generations,
                     popSize = pop_size,
                     names = all_features,
                     run = 50,
                     keepBest = TRUE)
      
      best_solution <- ga_model@solution[1, ]
      selected_features <- all_features[which(best_solution == 1)]
      res <- fitness_function(best_solution, dataset, train_index, test_index, model_type, fold_idx)
      accuracy <- res$accuracy
      sensitivity <- res$sensitivity
      specificity <- res$specificity
      PPV <- res$PPV
      NPV <- res$NPV
      kappa <- res$kappa
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Accuracy", Value = accuracy)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Sensitivity", Value = sensitivity)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Specificity", Value = specificity)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Kappa", Value = kappa)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "PPV", Value = PPV)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "NPV", Value = NPV)))  
      
      fold_predictions <- data.frame(
        Fold = fold_idx,
        Model = model_type,
        TrueLabel = res$true_label,
        PredictedProb = res$predicted_prob
      )
      all_predictions <- rbind(all_predictions, fold_predictions)
      
     if (accuracy > best_fold_accuracy) {
        best_fold_accuracy <- accuracy
        best_fold_features <- selected_features
      }
      
      ga_summaries[[paste(model_type, fold_idx, sep = "_")]] <- list(
        solution = best_solution,
        fitnessValue = ga_model@fitnessValue,
        summary = ga_model@summary,
        iter = ga_model@iter,
        run = ga_model@run
      )
      
      for (threshold in thresholds) {
        metrics <- calculate_metrics(threshold, res$predicted_prob, res$observations)
        thresholds_results <- rbind(thresholds_results, data.frame(Model = model_type, Threshold = threshold * 100, t(metrics)))
      }
    }
    
    if (best_fold_accuracy > best_overall_accuracy[[model_type]]) {
      best_overall_accuracy[[model_type]] <- best_fold_accuracy
      best_overall_features[[model_type]] <- best_fold_features
    }
  }
  
  return(list(
    best_features = best_overall_features,
    best_accuracy = best_overall_accuracy,
    results = results,
    thresholds_results = thresholds_results, 
    ga_summaries = ga_summaries,
    all_predictions = all_predictions
  ))
}

############################################################################
# fitness_function_bl_under ################################################
############################################################################
# Fitness Function for Feature Selection with Undersampling
# This function is used in genetic algorithms to evaluate a subset of features for classification models. It performs undersampling of the majority class to handle class imbalance during model training and calculates various performance metrics.

# Arguments:
# - selected_features: A binary vector indicating which features are selected (1 for selected, 0 for not). The length of this vector should match the number of features in the dataset.
# - dataset: A data frame containing the dataset, including the target variable 'euro_d'.
# - train_index: A vector of row indices indicating the training set.
# - test_index: A vector of row indices indicating the test set.
# - model_type: A string specifying the model type to use for training. Supported values are "rpart" (Decision Tree), "rf" (Random Forest), and "glm" (Generalized Linear Model).
# - fold_idx: The current fold index, used for tracking during cross-validation.

# Returns:
# - A list containing:
#   1. accuracy: The accuracy of the model on the test set.
#   2. sensitivity: The sensitivity (recall) of the model on the test set.
#   3. specificity: The specificity of the model on the test set.
#   4. selected_features: A logical vector indicating which features were selected in this evaluation.
#   5. true_label: The actual labels of the test set.
#   6. predicted_prob: The predicted probabilities for the positive class ("yes") on the test set.
#   7. observations: A factor containing the actual labels for the test set, used for evaluation.
#   8. PPV: Positive Predictive Value (precision) of the model on the test set.
#   9. NPV: Negative Predictive Value of the model on the test set.
#   10. kappa: Cohen's Kappa, a measure of the agreement between observed and predicted classifications.

# Details:
# - This function first checks if any features have been selected. If none are selected, it returns default values.
# - It creates training and testing datasets based on the provided indices.
# - To handle class imbalance, the function performs undersampling of the majority class in the training dataset, ensuring that the number of samples in both classes ("yes" and "no") is equal.
# - After undersampling, the specified model is trained using the balanced training data.
# - Predictions are made on the test set, and a confusion matrix is generated to compute performance metrics, including accuracy, sensitivity, specificity, PPV, NPV, and Cohen's Kappa.
# - The selected features, predicted probabilities, true labels, and performance metrics are returned as a list.

# Notes:
# - This function is designed to be used within genetic algorithms for feature selection, where the fitness of a subset of features is evaluated.
# - The function performs undersampling to address class imbalance in the training dataset, which can be particularly useful in datasets with skewed class distributions.
# - The model type can be "rpart", "rf", or "glm", representing Decision Trees, Random Forests, or Generalized Linear Models, respectively.

# Example Usage:
# ```
# result <- fitness_function_bl_under(selected_features, dataset, train_index, test_index, model_type = "rf", fold_idx = 1)
# ```
fitness_function_bl_under <- function(selected_features, dataset, train_index, test_index, model_type, fold_idx) {
  
  selected_features <- as.logical(selected_features)
  if(sum(selected_features) == 0) 
    return(list(accuracy = 0, sensitivity = NA, specificity = NA, selected_features = selected_features))
  
  features <- names(dataset)[selected_features]
  current_data <- dataset[, c(features, 'euro_d')]
  train_data <- current_data[train_index, ]
  test_data <- current_data[test_index, ]
  
  minor_class <- subset(train_data, euro_d == "yes")
  major_class <- subset(train_data, euro_d == "no")
  
  if (nrow(minor_class) > nrow(major_class)) {
    minor_class <- subset(train_data, euro_d == "no")
    major_class <- subset(train_data, euro_d == "yes")
  }
  
  major_samples <- major_class[sample(nrow(major_class), nrow(minor_class)), ]
  train_data <- rbind(minor_class, major_samples)
  
  model <- train_model(model_type, train_data)
  predictions <- make_predictions(model, test_data, model_type)
  observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
  cm <- confusionMatrix(factor(ifelse(predictions > 0.5, "yes", "no"), levels = c("no", "yes")), observed_factor)
  
  accuracy <- cm$overall['Accuracy']
  sensitivity <- cm$byClass['Sensitivity']
  specificity <- cm$byClass['Specificity']
  kappa <- cm$overall['Kappa']
  PPV <- cm$byClass['Pos Pred Value']
  NPV <- cm$byClass['Neg Pred Value']
  
  return(list(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity, selected_features = selected_features, true_label = test_data$euro_d, predicted_prob = predictions, observations = observed_factor, PPV = PPV, NPV = NPV, kappa = kappa))
}

############################################################################
# fitness_function_bl_over #################################################
############################################################################
# Fitness Function for Feature Selection with Oversampling
# This function is used in genetic algorithms to evaluate a subset of features for classification models. It performs oversampling of the minority class to handle class imbalance during model training and calculates various performance metrics.

# Arguments:
# - selected_features: A binary vector indicating which features are selected (1 for selected, 0 for not). The length of this vector should match the number of features in the dataset.
# - dataset: A data frame containing the dataset, including the target variable 'euro_d'.
# - train_index: A vector of row indices indicating the training set.
# - test_index: A vector of row indices indicating the test set.
# - model_type: A string specifying the model type to use for training. Supported values are "rpart" (Decision Tree), "rf" (Random Forest), and "glm" (Generalized Linear Model).
# - fold_idx: The current fold index, used for tracking during cross-validation.

# Returns:
# - A list containing:
#   1. accuracy: The accuracy of the model on the test set.
#   2. sensitivity: The sensitivity (recall) of the model on the test set.
#   3. specificity: The specificity of the model on the test set.
#   4. selected_features: A logical vector indicating which features were selected in this evaluation.
#   5. true_label: The actual labels of the test set.
#   6. predicted_prob: The predicted probabilities for the positive class ("yes") on the test set.
#   7. observations: A factor containing the actual labels for the test set, used for evaluation.
#   8. PPV: Positive Predictive Value (precision) of the model on the test set.
#   9. NPV: Negative Predictive Value of the model on the test set.
#   10. kappa: Cohen's Kappa, a measure of the agreement between observed and predicted classifications.

# Details:
# - This function first checks if any features have been selected. If none are selected, it returns default values.
# - It creates training and testing datasets based on the provided indices.
# - To handle class imbalance, the function performs oversampling of the minority class in the training dataset. Synthetic samples are generated by randomly duplicating instances of the minority class and adding noise to them.
# - The specified model is trained using the balanced training data.
# - Predictions are made on the test set, and a confusion matrix is generated to compute performance metrics, including accuracy, sensitivity, specificity, PPV, NPV, and Cohen's Kappa.
# - The selected features, predicted probabilities, true labels, and performance metrics are returned as a list.

# Notes:
# - This function is designed to be used within genetic algorithms for feature selection, where the fitness of a subset of features is evaluated.
# - The function performs oversampling to address class imbalance in the training dataset, which is particularly useful in datasets with skewed class distributions.
# - The model type can be "rpart", "rf", or "glm", representing Decision Trees, Random Forests, or Generalized Linear Models, respectively.
# - The `add_noise` function is used to introduce slight variations to the duplicated synthetic samples to prevent overfitting.

# Example Usage:
# ```
# result <- fitness_function_bl_over(selected_features, dataset, train_index, test_index, model_type = "rf", fold_idx = 1)
# ```
fitness_function_bl_over <- function(selected_features, dataset, train_index, test_index, model_type, fold_idx) {
  
  selected_features <- as.logical(selected_features)
  if(sum(selected_features) == 0) 
    return(list(accuracy = 0, sensitivity = NA, specificity = NA, selected_features = selected_features))
  
  
  features <- names(dataset)[selected_features]
  current_data <- dataset[, c(features, 'euro_d')]
  train_data <- current_data[train_index, ]
  test_data <- current_data[test_index, ]
  
  minor_class <- subset(train_data, euro_d == "yes")
  major_class <- subset(train_data, euro_d == "no")
  
  if (nrow(minor_class) > nrow(major_class)) {
    minor_class <- subset(train_data, euro_d == "no")
    major_class <- subset(train_data, euro_d == "yes")
  }
  
  num_samples_to_generate <- nrow(major_class) - nrow(minor_class)
  synthetic_samples <- minor_class[sample(nrow(minor_class), num_samples_to_generate, replace = TRUE), ]
  synthetic_samples <- add_noise(synthetic_samples)
  train_data <- rbind(train_data, synthetic_samples)
  
  model <- train_model(model_type, train_data)
  predictions <- make_predictions(model, test_data, model_type)
  observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
  cm <- confusionMatrix(factor(ifelse(predictions > 0.5, "yes", "no"), levels = c("no", "yes")), observed_factor)
  
  accuracy <- cm$overall['Accuracy']
  sensitivity <- cm$byClass['Sensitivity']
  specificity <- cm$byClass['Specificity']
  kappa <- cm$overall['Kappa']
  PPV <- cm$byClass['Pos Pred Value']
  NPV <- cm$byClass['Neg Pred Value']
  
  return(list(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity, selected_features = selected_features, true_label = test_data$euro_d, predicted_prob = predictions, observations = observed_factor, PPV = PPV, NPV = NPV, kappa = kappa))
}

############################################################################
# genetic_feature_selection_bl #############################################
############################################################################
genetic_feature_selection_bl <- function(dataset, model_types = c("rpart", "rf", "glm"), n_generations = 50, pop_size = 50, balance = "over") {
  
  set.seed(1)
  best_overall_accuracy <- list()
  best_overall_features <- list()
  results <- list()
  thresholds_results <- data.frame()
  ga_summaries <- list()
  all_predictions <- data.frame()
  folds <- createFolds(dataset$euro_d, k = 10)
  thresholds <- seq(0.05, 0.95, by = 0.05)
  
  for(n in 1:5){
    for (model_type in model_types) {
      
      best_fold_accuracy <- 0
      best_fold_features <- NULL
      best_overall_accuracy[[model_type]] <- -Inf
      
      for (fold_idx in seq_along(folds)) {
        
        all_features <- setdiff(names(dataset), 'euro_d')
        train_index <- -folds[[fold_idx]]
        test_index <- folds[[fold_idx]]
        
        ga_model <- ga(type = "binary",
                       fitness = function(selected_features) {
                         if(balance == "over")
                            res <- fitness_function_bl_over(selected_features, dataset, train_index, test_index, model_type, fold_idx)
                         else
                           res <- fitness_function_bl_under(selected_features, dataset, train_index, test_index, model_type, fold_idx)
                         return(res$accuracy) # Fitness è basata solo sull'accuratezza
                       },
                       nBits = length(all_features),
                       maxiter = n_generations,
                       popSize = pop_size,
                       names = all_features,
                       run = 50,
                       keepBest = TRUE)
        
        best_solution <- ga_model@solution[1, ]
        selected_features <- all_features[which(best_solution == 1)]
        if(balance == "over")
          res <- fitness_function_bl_over(best_solution, dataset, train_index, test_index, model_type, fold_idx)
        else
          res <- fitness_function_bl_under(best_solution, dataset, train_index, test_index, model_type, fold_idx)
        accuracy <- res$accuracy
        sensitivity <- res$sensitivity
        specificity <- res$specificity
        PPV <- res$PPV
        NPV <- res$NPV
        kappa <- res$kappa
        
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Accuracy", Value = accuracy)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Sensitivity", Value = sensitivity)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Specificity", Value = specificity)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Kappa", Value = kappa)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "PPV", Value = PPV)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "NPV", Value = NPV)))  
        
        fold_predictions <- data.frame(
          Fold = fold_idx,
          Model = model_type,
          TrueLabel = res$true_label,
          PredictedProb = res$predicted_prob
        )
        all_predictions <- rbind(all_predictions, fold_predictions)
        
        if (accuracy > best_fold_accuracy) {
          best_fold_accuracy <- accuracy
          best_fold_features <- selected_features
        }
        
        ga_summaries[[paste(model_type, fold_idx, sep = "_")]] <- list(
          solution = best_solution,
          fitnessValue = ga_model@fitnessValue,
          summary = ga_model@summary,
          iter = ga_model@iter,
          run = ga_model@run
        )
        
        for (threshold in thresholds) {
          metrics <- calculate_metrics(threshold, res$predicted_prob, res$observations)
          thresholds_results <- rbind(thresholds_results, data.frame(Model = model_type, Threshold = threshold * 100, t(metrics)))
        }
      }
      
      if (best_fold_accuracy > best_overall_accuracy[[model_type]]) {
        best_overall_accuracy[[model_type]] <- best_fold_accuracy
        best_overall_features[[model_type]] <- best_fold_features
      }
    }
  }
 
  return(list(
    best_features = best_overall_features,
    best_accuracy = best_overall_accuracy,
    results = results,
    thresholds_results = thresholds_results, 
    ga_summaries = ga_summaries,
    all_predictions = all_predictions
  ))
}


score_fitness_function <- function(selected_features, dataset, train_index, test_index, model_type, fold_idx) {
  
  selected_features <- as.logical(selected_features)
  if(sum(selected_features) == 0) 
    return(list(MSE = Inf, MAE = NA, R2 = NA, selected_features = selected_features))
  
  features <- names(dataset)[selected_features]
  current_data <- dataset[, c(features, 'euro_d')]
  train_data <- current_data[train_index, ]
  test_data <- current_data[test_index, ]
  
  model <- score_train_model(model_type, train_data) # Modifica per includere modelli di regressione
  predictions <- score_make_predictions(model, test_data, model_type)
  
  # Calcolo delle metriche per la regressione
  observed_values <- test_data$euro_d
  MSE <- mean((predictions - observed_values)^2)
  MAE <- mean(abs(predictions - observed_values))
  R2 <- 1 - sum((predictions - observed_values)^2) / sum((observed_values - mean(observed_values))^2)
  
  return(list(MSE = MSE, MAE = MAE, R2 = R2, selected_features = selected_features, 
              true_label = observed_values, predicted_values = predictions))
}

score_genetic_feature_selection <- function(dataset, model_types = c("rpart", "rf", "glm"), n_generations = 50, pop_size = 50) {
  
  set.seed(1)
  best_overall_MSE <- list()
  best_overall_features <- list()
  results <- list()
  ga_summaries <- list()
  all_predictions <- data.frame()
  folds <- createFolds(dataset$euro_d, k = 10) # Creazione dei fold per la cross-validation
  
  for (model_type in model_types) {
    
    best_fold_MSE <- Inf
    best_fold_features <- NULL
    best_overall_MSE[[model_type]] <- Inf
    
    for (fold_idx in seq_along(folds)) {
      
      all_features <- setdiff(names(dataset), 'euro_d')
      train_index <- -folds[[fold_idx]]
      test_index <- folds[[fold_idx]]
      
      ga_model <- ga(type = "binary",
                     fitness = function(selected_features) {
                       res <- score_fitness_function(selected_features, dataset, train_index, test_index, model_type, fold_idx)
                       return(-res$MSE) # Fitness basata sull'errore quadratico medio negativo
                     },
                     nBits = length(all_features),
                     maxiter = n_generations,
                     popSize = pop_size,
                     names = all_features,
                     run = 50,
                     keepBest = TRUE)
      
      best_solution <- ga_model@solution[1, ]
      selected_features <- all_features[which(best_solution == 1)]
      res <- score_fitness_function(best_solution, dataset, train_index, test_index, model_type, fold_idx)
      MSE <- res$MSE
      MAE <- res$MAE
      R2 <- res$R2
      
      # Salva i risultati in un dataframe
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "MSE", Value = MSE)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "MAE", Value = MAE)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "R2", Value = R2)))
      
      fold_predictions <- data.frame(
        Fold = fold_idx,
        Model = model_type,
        TrueLabel = res$true_label,
        PredictedValues = res$predicted_values
      )
      all_predictions <- rbind(all_predictions, fold_predictions)
      
      if (MSE < best_fold_MSE) {
        best_fold_MSE <- MSE
        best_fold_features <- selected_features
      }
      
      ga_summaries[[paste(model_type, fold_idx, sep = "_")]] <- list(
        solution = best_solution,
        fitnessValue = ga_model@fitnessValue,
        summary = ga_model@summary,
        iter = ga_model@iter,
        run = ga_model@run
      )
    }
    
    if (best_fold_MSE < best_overall_MSE[[model_type]]) {
      best_overall_MSE[[model_type]] <- best_fold_MSE
      best_overall_features[[model_type]] <- best_fold_features
    }
  }
  
  return(list(
    best_features = best_overall_features,
    best_MSE = best_overall_MSE,
    results = results,
    ga_summaries = ga_summaries,
    all_pred_ga = all_predictions
  ))
}