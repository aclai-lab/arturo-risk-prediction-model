############################################################################
# SEQUENTIAL BACKWARD SELECTION ############################################
############################################################################

############################################################################
# random_backward_selection_bl #############################################
############################################################################
# Random Backward Feature Selection with Balanced Dataset
# This function performs random backward feature selection for model training using cross-validation. The objective is to iteratively remove features that do not contribute to improving model accuracy, while using balanced training datasets for each fold.
#
# Arguments:
# - dataset: A data frame containing the dataset, which includes the dependent variable specified by `target_var`.
# - target_var: A string specifying the name of the target variable. The default is "euro_d".
# - N: The number of times the entire feature selection process is repeated. Default is 5.
# - k: The number of folds for cross-validation. Default is 10.
# - seed: A random seed for reproducibility. Default is 1.
# - balance: A string specifying the method for balancing the dataset, either "under" or "over". Default is "under".
#
# Returns:
# - A list containing:
#   1. results: A list of data frames storing the performance metrics (Accuracy, Sensitivity, Specificity, Kappa, PPV, NPV) for each model type and fold.
#   2. best_accuracy: The highest accuracy achieved across all models and folds during feature selection.
#   3. best_features: A vector of unique features that yielded the best accuracy after the feature selection process.
#   4. all_predictions: A data frame containing the predictions for each fold, including fold index, model type, true labels, and predicted probabilities.
#   5. thresholds_results: A data frame with metrics calculated for various thresholds on the predicted probabilities.
#
# Details:
# - The function sets a seed for reproducibility and initializes variables to store results, predictions, and threshold metrics.
# - Uses `createFolds` to split the dataset into training and test sets for k-fold cross-validation.
# - For each specified model type ("rpart", "rf", "glm"):
#   - Initializes the feature selection process by considering all available features.
#   - In each iteration, it randomly selects a feature to remove and evaluates the model's performance.
#   - Computes the following metrics: Accuracy, Sensitivity, Specificity, Positive Predictive Value (PPV), Negative Predictive Value (NPV), and Kappa.
#   - Removes the feature if the accuracy does not decrease, otherwise retains it.
#   - Records the best accuracy and selected features for each fold.
#   - Balances the training dataset using the specified balancing method ("under" or "over").
#   - Computes performance metrics at various thresholds (from 0.05 to 0.95) on the predicted probabilities.
# - Repeats the feature selection process N times to find the best set of features.
# - Returns the overall best accuracy, best features, and performance metrics.
#
# Notes:
# - The function includes console messages to indicate the progress of feature removal and its impact on accuracy.

random_backward_selection_bl <- function(dataset, target_var = "euro_d", N = 5, k = 10, seed = 1, balance = "under") {
  set.seed(seed)
  
  best_overall_accuracy <- 0
  best_overall_features <- NULL
  results <- list()
  folds <- createFolds(dataset[[target_var]], k = k)
  all_predictions <- data.frame()
  thresholds_results <- data.frame()
  thresholds <- seq(0.05, 0.95, by = 0.05)
  
  
  for (n in 1:N) { 
    for (model_type in c("rpart", "rf", "glm")) {
     
      best_fold_accuracy <- 0
      best_fold_features <- NULL
  
      for (fold_idx in seq_along(folds)) {
        all_features <- setdiff(names(dataset), target_var)
        selected_features <- all_features
       
        best_accuracy <- 0
        best_sensitivity <- NULL
        best_specificity <- NULL
        best_NPV <- NULL
        best_PPV <- NULL
        best_kappa <- NULL
        
        for (i in 1:length(selected_features)) {
          random_feature <- sample(selected_features, 1)
          current_features <- setdiff(selected_features, random_feature)
          current_data <- dataset[, c(current_features, target_var)]
          train_data <- current_data[-folds[[fold_idx]], ]
          test_data <- current_data[folds[[fold_idx]], ]
        
          train_data <- balancing_dataset(train_data, balance)
          
          model <- train_model(model_type, train_data)
          predictions <- make_predictions(model, test_data, model_type)
          
          predicted_class <- ifelse(predictions > 0.5, "yes", "no")
          observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
          cm <- confusionMatrix(factor(predicted_class, levels = c("no", "yes")), observed_factor)
          
          accuracy <- cm$overall['Accuracy']
          sensitivity <- cm$byClass['Sensitivity']
          specificity <- cm$byClass['Specificity']
          PPV <- cm$byClass["Pos Pred Value"]
          NPV <- cm$byClass["Neg Pred Value"]
          kappa <- cm$overall["Kappa"]
          
          if (accuracy >= best_accuracy) {
            best_accuracy <- accuracy
            best_sensitivity <- sensitivity
            best_specificity <- specificity
            best_NPV <- NPV
            best_PPV <- PPV
            best_kappa <- kappa
            last_predictions <- predictions 
            selected_features <- current_features
            cat("Iteration", i, ": removing feature =", random_feature, "get accuracy =", best_accuracy, "\n")
          } else {
            cat("Iteration", i, ": removing feature =", random_feature, "did not improve accuracy\n")
          }
        }
        
        fold_predictions <- data.frame(
          Fold = fold_idx,
          Model = model_type,
          TrueLabel = test_data$euro_d,
          PredictedProb = last_predictions
        )
        all_predictions <- rbind(all_predictions, fold_predictions)
        
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Accuracy", Value = best_accuracy)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Sensitivity", Value = best_sensitivity)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Specificity", Value = best_specificity)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Kappa", Value = best_kappa)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "PPV", Value = best_PPV)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "NPV", Value = best_NPV)))
        
        if (best_accuracy > best_fold_accuracy) {
          best_fold_accuracy <- best_accuracy
          best_fold_features <- selected_features
        }
        
        for (threshold in thresholds) {
          metrics <- calculate_metrics(threshold, results$predicted_prob, results$observations)
          thresholds_results <- rbind(thresholds_results, data.frame(Model = model_type, Threshold = threshold * 100, t(metrics)))
        }
      }
      if (best_fold_accuracy > best_overall_accuracy) {
        best_overall_accuracy <- best_fold_accuracy
        best_overall_features <- best_fold_features
      }
    }
  }
  
  return(list(results = results, best_accuracy = best_overall_accuracy, best_features = unique(best_overall_features), all_predictions = all_predictions, thresholds_results = thresholds_results))
}

############################################################################
# random_backward_selection ################################################
############################################################################
# Random Backward Feature Selection
# This function performs random backward feature selection using cross-validation to optimize model performance. It iteratively removes random features, evaluates model accuracy, and retains the features that contribute to the best accuracy.
#
# Arguments:
# - dataset: A data frame containing the dataset, which includes the dependent variable specified by `target_var`.
# - target_var: A string specifying the name of the target variable. The default is "euro_d".
# - N: The number of times the feature selection process is repeated. Default is 5.
# - k: The number of folds for cross-validation. Default is 10.
# - seed: An integer for setting the random seed to ensure reproducibility. Default is 1.
#
# Returns:
# - A list containing:
#   1. results: A list of data frames storing the performance metrics (Accuracy, Sensitivity, Specificity, Kappa, PPV, NPV) for each model type and fold.
#   2. best_accuracy: The highest accuracy achieved across all models and folds during the feature selection process.
#   3. best_features: A vector of unique features that yielded the best accuracy after the feature selection process.
#   4. all_predictions: A data frame containing the predictions for each fold, including fold index, model type, true labels, and predicted probabilities.
#   5. thresholds_results: A data frame with metrics calculated for various thresholds on the predicted probabilities.
#
# Details:
# - The function sets a seed for reproducibility and initializes variables to store results, predictions, and threshold metrics.
# - Uses `createFolds` to split the dataset into training and test sets for k-fold cross-validation.
# - For each specified model type ("rpart", "rf", "glm"):
#   - Initializes the feature selection process by considering all available features.
#   - Iteratively removes a randomly chosen feature, trains the model, and evaluates its performance using the following metrics: Accuracy, Sensitivity, Specificity, Positive Predictive Value (PPV), Negative Predictive Value (NPV), and Kappa.
#   - If removing a feature improves accuracy, the feature is excluded from the selected features for future iterations; otherwise, it is retained.
#   - Stores the predictions, performance metrics, and selected features for each fold.
#   - Computes performance metrics at various thresholds (from 0.05 to 0.95) on the predicted probabilities.
# - Returns the best accuracy, selected features, and performance metrics.
#
# Notes:
# - Includes console messages to indicate the progress of feature removal and its impact on accuracy.
# - The function does not balance the dataset; consider using `random_backward_selection_bl` if balancing is required.
random_backward_selection <- function(dataset, target_var = "euro_d", N = 5, k = 10, seed = 1) {
  set.seed(seed)
  
  best_overall_accuracy <- 0
  best_overall_features <- NULL
  results <- list()
  folds <- createFolds(dataset[[target_var]], k = k)
  all_predictions <- data.frame()
  thresholds_results <- data.frame()
  thresholds <- seq(0.05, 0.95, by = 0.05)
  
  
    for (model_type in c("rpart", "rf", "glm")) {
      
      best_fold_accuracy <- 0
      best_fold_features <- NULL
      
      for (fold_idx in seq_along(folds)) {
        all_features <- setdiff(names(dataset), target_var)
        selected_features <- all_features
        
        best_accuracy <- 0
        best_sensitivity <- NULL
        best_specificity <- NULL
        best_NPV <- NULL
        best_PPV <- NULL
        best_kappa <- NULL
        
        for (i in 1:length(selected_features)) {
          random_feature <- sample(selected_features, 1)
          current_features <- setdiff(selected_features, random_feature)
          current_data <- dataset[, c(current_features, target_var)]
          train_data <- current_data[-folds[[fold_idx]], ]
          test_data <- current_data[folds[[fold_idx]], ]
          
          model <- train_model(model_type, train_data)
          predictions <- make_predictions(model, test_data, model_type)
          
          predicted_class <- ifelse(predictions > 0.5, "yes", "no")
          observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
          cm <- confusionMatrix(factor(predicted_class, levels = c("no", "yes")), observed_factor)
          
          accuracy <- cm$overall['Accuracy']
          sensitivity <- cm$byClass['Sensitivity']
          specificity <- cm$byClass['Specificity']
          PPV <- cm$byClass["Pos Pred Value"]
          NPV <- cm$byClass["Neg Pred Value"]
          kappa <- cm$overall["Kappa"]
          
          if (accuracy >= best_accuracy) {
            best_accuracy <- accuracy
            best_sensitivity <- sensitivity
            best_specificity <- specificity
            best_NPV <- NPV
            best_PPV <- PPV
            best_kappa <- kappa
            last_predictions <- predictions 
            selected_features <- current_features
            cat("Iteration", i, ": removing feature =", random_feature, "get accuracy =", best_accuracy, "\n")
          } else {
            cat("Iteration", i, ": removing feature =", random_feature, "did not improve accuracy\n")
          }
        }
        
        fold_predictions <- data.frame(
          Fold = fold_idx,
          Model = model_type,
          TrueLabel = test_data$euro_d,
          PredictedProb = last_predictions
        )
        all_predictions <- rbind(all_predictions, fold_predictions)
        
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Accuracy", Value = best_accuracy)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Sensitivity", Value = best_sensitivity)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Specificity", Value = best_specificity)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Kappa", Value = best_kappa)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "PPV", Value = best_PPV)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "NPV", Value = best_NPV)))
        
        if (best_accuracy > best_fold_accuracy) {
          best_fold_accuracy <- best_accuracy
          best_fold_features <- selected_features
        }
        for (threshold in thresholds) {
          metrics <- calculate_metrics(threshold, results$predicted_prob, results$observations)
          thresholds_results <- rbind(thresholds_results, data.frame(Model = model_type, Threshold = threshold * 100, t(metrics)))
        }
      }
      if (best_fold_accuracy > best_overall_accuracy) {
        best_overall_accuracy <- best_fold_accuracy
        best_overall_features <- best_fold_features
      }
    }
  
  return(list(results = results, best_accuracy = best_overall_accuracy, best_features = unique(best_overall_features), all_predictions = all_predictions, thresholds_results = thresholds_results))
}