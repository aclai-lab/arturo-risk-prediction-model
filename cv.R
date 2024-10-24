############################################################################
# CROSS VALIDATION #########################################################
############################################################################

############################################################################
# cv #######################################################################
############################################################################
# Cross-Validation Function
# This function performs cross-validation on the input dataset using different models (decision tree, random forest, logistic regression).
# It trains models, makes predictions, calculates performance metrics, and evaluates them across different thresholds.
#
# Arguments:
# - dataset: The input dataset for cross-validation. The dataset should contain the dependent variable 'euro_d'.
# - seed: An integer to set the random seed for reproducibility (default is 123).
# - k_folds: The number of folds to use in cross-validation (default is 10).
#
# Returns:
# - A list containing:
#   1. results: A list of data frames with the calculated metrics (accuracy, sensitivity, specificity, PPV, NPV, kappa) for each model and fold.
#   2. all_predictions: A data frame of all the predictions made during cross-validation, including the fold index, model type, true labels, and predicted probabilities.
#   3. thresholds_results: A data frame containing performance metrics calculated at different threshold levels for each model.
#
# Details:
# - The function first sets up cross-validation using the `createFolds` function.
# - It loops through the specified models ('rpart' for decision tree, 'rf' for random forest, and 'glm' for logistic regression).
# - For each model, it performs training and testing on each fold:
#   - Trains the model on the training data and makes predictions on the test data.
#   - Stores the predicted probabilities, true labels, and model information in `all_predictions`.
#   - Calculates the confusion matrix and extracts performance metrics (accuracy, sensitivity, specificity, kappa, PPV, NPV).
#   - Appends these metrics to the `results` list.
# - The function evaluates performance across a range of probability thresholds (0.05 to 0.95) using the `calculate_metrics` function, storing these results in `thresholds_results`.
cv <- function(dataset,model_types = c("rpart", "rf", "glm"), seed = 123, k_folds = 10) {
  set.seed(seed)
  results <- list()
  all_predictions <- data.frame()
  thresholds_results <- data.frame()
  thresholds <- seq(0.05, 0.95, by = 0.05)
  
  folds <- createFolds(dataset$euro_d, k = k_folds)
  
  for (model_type in model_types) {
    
    best_fold_accuracy <- 0
    best_fold_features <- NULL
    
    for (fold_idx in seq_along(folds)) {
      train_data <- dataset[-folds[[fold_idx]], ]
      test_data <- dataset[folds[[fold_idx]], ]
      
      model <- train_model(model_type, train_data)
      predictions <- make_predictions(model, test_data, model_type)
      
      fold_predictions <- data.frame(
        Fold = fold_idx,
        Model = model_type,
        TrueLabel = test_data$euro_d,
        PredictedProb = predictions
      )
      all_predictions <- rbind(all_predictions, fold_predictions)
      
      predicted_class <- ifelse(predictions > 0.5, "yes", "no")
      observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
      cm <- confusionMatrix(factor(predicted_class, levels = c("no", "yes")), observed_factor)
      
      accuracy <- cm$overall["Accuracy"]
      sensitivity <- cm$byClass["Sensitivity"]
      specificity <- cm$byClass["Specificity"]
      PPV <- cm$byClass["Pos Pred Value"]
      NPV <- cm$byClass["Neg Pred Value"]
      kappa <- cm$overall["Kappa"]
      
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "ACC", Value = accuracy)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "SENS", Value = sensitivity)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "SPEC", Value = specificity)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Kappa", Value = kappa)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "PPV", Value = PPV)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "NPV", Value = NPV)))
      
      for (threshold in thresholds) {
        metrics <- calculate_metrics(threshold, predictions, test_data$euro_d)
        thresholds_results <- rbind(thresholds_results, data.frame(Model = model_type, Threshold = threshold * 100, t(metrics)))
      }
    }
  }
  
  return(list(results = results, all_predictions = all_predictions, thresholds_results = thresholds_results))
}


############################################################################
# double_cv ################################################################
############################################################################
# Double Cross-Validation Function
# This function performs a nested cross-validation on the input dataset using different models (decision tree, random forest, logistic regression).
# It balances the training data, trains models, makes predictions, and calculates performance metrics across multiple runs.
#
# Arguments:
# - dataset: The input dataset for cross-validation. The dataset should contain the dependent variable 'euro_d'.
# - seed: An integer to set the random seed for reproducibility (default is 123).
# - N: The number of times the cross-validation process is repeated (default is 5).
# - k_folds: The number of folds to use in cross-validation (default is 10).
# - balance: The method for balancing the dataset ("under" for undersampling or "over" for oversampling; default is "under").
#
# Returns:
# - A list containing:
#   1. results: A list of data frames with the calculated metrics (accuracy, sensitivity, specificity, PPV, NPV, kappa) for each model, fold, and repetition.
#   2. all_predictions: A data frame of all predictions made during cross-validation, including the fold index, model type, true labels, and predicted probabilities.
#   3. thresholds_results: A data frame containing performance metrics calculated at different threshold levels for each model.
#
# Details:
# - The function sets up a nested cross-validation with `N` repetitions and `k_folds` splits.
# - It balances the training data in each fold using the specified method ('under' or 'over') by calling the `balancing_dataset` function.
# - For each repetition (`N`), it loops through the specified models ('rpart' for decision tree, 'rf' for random forest, 'glm' for logistic regression):
#   - Splits the data into `k_folds` using the `createFolds` function.
#   - For each fold, it trains the model on the balanced training data and makes predictions on the test data.
#   - Stores predicted probabilities, true labels, and model information in `all_predictions`.
#   - Calculates the confusion matrix and extracts performance metrics (accuracy, sensitivity, specificity, kappa, PPV, NPV).
#   - Appends these metrics to the `results` list.
# - The function evaluates performance across a range of probability thresholds (0.05 to 0.95) using the `calculate_metrics` function, storing these results in `thresholds_results`.
double_cv <- function(dataset, model_types = c("rpart", "rf", "glm"), seed = 123, N = 5, k_folds = 10, balance = "under") {
  set.seed(seed)
  results <- list()
  all_predictions <- data.frame()
  thresholds_results <- data.frame()
  thresholds <- seq(0.05, 0.95, by = 0.05)
  
  for (n in 1:N) {  
    folds <- createFolds(dataset$euro_d, k = k_folds)
    
    for (model_type in model_types) {
      for (fold_idx in seq_along(folds)) {
        train_data <- dataset[-folds[[fold_idx]], ]
        test_data <- dataset[folds[[fold_idx]], ]
        
        balanced_train_data <- balancing_dataset(train_data, method = balance)
      
        model <- train_model(model_type, balanced_train_data)
        predictions <- make_predictions(model, test_data, model_type)
        
        fold_predictions <- data.frame(
          Fold = fold_idx,
          Model = model_type,
          TrueLabel = test_data$euro_d,
          PredictedProb = predictions
        )
        all_predictions <- rbind(all_predictions, fold_predictions)
        
        predicted_class <- ifelse(predictions > 0.5, "yes", "no")
        observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
        cm <- confusionMatrix(factor(predicted_class, levels = c("no", "yes")), observed_factor)
        
        accuracy <- cm$overall["Accuracy"]
        sensitivity <- cm$byClass["Sensitivity"]
        specificity <- cm$byClass["Specificity"]
        PPV <- cm$byClass["Pos Pred Value"]
        NPV <- cm$byClass["Neg Pred Value"]
        kappa <- cm$overall["Kappa"]
        
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "ACC", Value = accuracy)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "SENS", Value = sensitivity)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "SPEC", Value = specificity)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Kappa", Value = kappa)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "PPV", Value = PPV)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "NPV", Value = NPV)))
      
        for (threshold in thresholds) {
          metrics <- calculate_metrics(threshold, predictions, test_data$euro_d)
          thresholds_results <- rbind(thresholds_results, data.frame(Model = model_type, Threshold = threshold * 100, t(metrics)))
        }
      }
    }
  }
  
  return(list(results = results, all_predictions = all_predictions, thresholds_results = thresholds_results))
}

############################################################################
# score_cross_validation ###################################################
############################################################################
# Score Cross-Validation Function
# This function performs a cross-validation on the input dataset using specified regression models and calculates evaluation metrics.
#
# Arguments:
# - dataset: The input dataset for cross-validation. It should contain the dependent variable 'euro_d'.
# - model_types: A vector specifying the types of models to use for training (default is c("rpart", "rf", "glm")).
#   - "rpart": Decision tree regression model.
#   - "rf": Random forest regression model.
#   - "glm": Linear regression model.
#
# Returns:
# - A list containing:
#   1. best_MSE: A list of the lowest Mean Squared Error (MSE) achieved for each model type across all folds.
#   2. results: A list of data frames containing performance metrics (MSE, MAE, R2) for each model type and fold.
#   3. all_predictions: A data frame with predictions for each fold, including the fold index, model type, true labels, and predicted values.
#
# Details:
# - The function sets a seed for reproducibility and initializes lists to store results and best MSEs for each model type.
# - The dataset is split into 10 folds using the `createFolds` function.
# - For each model type, the following steps are performed:
#   - The data is split into training and test sets based on the current fold.
#   - A model is trained using the `score_train_model` function on the training data.
#   - Predictions are made on the test data using the `score_make_predictions` function.
#   - Evaluation metrics are calculated:
#     - Mean Squared Error (MSE).
#     - Mean Absolute Error (MAE).
#     - R-squared (R2).
#   - The metrics are stored in the `results` list.
#   - Predictions for the current fold are added to `all_predictions`.
#   - The best MSE for the current fold is updated if the new MSE is lower.
# - The function returns the best MSE for each model, the complete results list, and all predictions.
score_cross_validation <- function(dataset, model_types = c("rpart", "rf", "glm")) {
  
  set.seed(123)
  best_overall_MSE <- list()
  results <- list()
  all_predictions <- data.frame()
  folds <- createFolds(dataset$euro_d, k = 10) 
  
  for (model_type in model_types) {
    
    best_fold_MSE <- Inf
    best_overall_MSE[[model_type]] <- Inf
    
    for (fold_idx in seq_along(folds)) {
      
      all_features <- setdiff(names(dataset), 'euro_d')
      train_index <- -folds[[fold_idx]]
      test_index <- folds[[fold_idx]]
      
      selected_features <- all_features
      
      current_data <- dataset[, c(selected_features, 'euro_d')]
      train_data <- current_data[train_index, ]
      test_data <- current_data[test_index, ]
      
      model <- score_train_model(model_type, train_data)
      
      predictions <- score_make_predictions(model, test_data, model_type)
      #valid_indices <- which(predictions <= 12)
      #predictions <- predictions[valid_indices]
      #observed_values <- test_data$euro_d[valid_indices]
      observed_values <- test_data$euro_d
      
      if(length(predictions)>0){
        MSE <- mean((predictions - observed_values)^2)
        MAE <- mean(abs(predictions - observed_values))
        R2 <- 1 - sum((predictions - observed_values)^2) / sum((observed_values - mean(observed_values))^2)
        
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "MSE", Value = MSE)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "MAE", Value = MAE)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "R2", Value = R2)))
        
        fold_predictions <- data.frame(
          Fold = fold_idx,
          Model = model_type,
          TrueLabel = observed_values,
          PredictedValues = predictions
        )
        all_predictions <- rbind(all_predictions, fold_predictions)
        
        if (MSE < best_fold_MSE) {
          best_fold_MSE <- MSE
        }
      }
    }
    
    if (best_fold_MSE < best_overall_MSE[[model_type]]) {
      best_overall_MSE[[model_type]] <- best_fold_MSE
    }
  }
  
  return(list(
    best_MSE = best_overall_MSE,
    results = results,
    all_predictions = all_predictions
  ))
}
