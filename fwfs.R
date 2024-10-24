############################################################################
# SEQUENTIAL FORWARD SELECTION #############################################
############################################################################

############################################################################
# random_forward_selection_bl #################################################
############################################################################
# Random Forward Selection with Balancing
# This function performs a forward selection of features for model training using random selection. It balances the dataset during training and evaluates multiple models using cross-validation.
#
# Arguments:
# - dataset: The input dataset containing the dependent variable 'euro_d' and various features.
# - balance: A string specifying the balancing method to use on the training dataset. Default is "under".
# - The function uses N = 5 for random feature selection iterations and 10-fold cross-validation.
#
# Returns:
# - A list containing:
#   1. results: A list of data frames that store performance metrics (Accuracy, Sensitivity, Specificity, Kappa, PPV, NPV) for each model type and fold.
#   2. best_accuracy: The highest accuracy obtained across all models and folds.
#   3. best_features: A vector containing the unique best features selected during the process.
#   4. all_predictions: A data frame with predictions for each fold, including fold index, model type, true labels, and predicted probabilities.
#   5. thresholds_results: A data frame containing metrics calculated for various thresholds on the predicted probabilities.
#
# Details:
# - The function sets a seed for reproducibility and initializes variables to store results, predictions, and threshold metrics.
# - For each iteration (N = 5):
#   - The dataset is split into 10 folds using `createFolds`.
#   - For each specified model type ("rpart", "rf", "glm"):
#     - Randomly selects 5 features from the dataset and performs forward selection by adding one feature at a time.
#     - Balances the training dataset using the specified `balance` method (e.g., "under").
#     - Trains the model using the selected features and evaluates it using the test data.
#     - Computes evaluation metrics: Accuracy, Sensitivity, Specificity, Positive Predictive Value (PPV), Negative Predictive Value (NPV), and Kappa.
#     - Updates the list of selected features if the newly added feature improves accuracy.
#   - Stores the best accuracy and features for each fold and model.
#   - Calculates performance metrics at different thresholds (0.05 to 0.95) on the predicted probabilities.
# - The function returns the overall best accuracy, best features, and performance metrics across all iterations.
random_forward_selection_bl <- function(dataset, balance = "under") {
  set.seed(1)  
  
  best_overall_accuracy <- 0
  best_overall_features <- NULL
  results <- list()
  all_predictions <- data.frame()
  thresholds_results <- data.frame()
  thresholds <- seq(0.05, 0.95, by = 0.05)
  
  N <- 5
  for(n in 1:N){
    
    folds <- createFolds(dataset$euro_d, k = 10)
    
    for (model_type in c("rpart", "rf", "glm")) {

      best_fold_accuracy <- 0
      best_fold_features <- NULL
      
      for (fold_idx in seq_along(folds)) {
        
        all_features <- setdiff(names(dataset), 'euro_d')
        
        if (length(all_features) < 5) {
          stop("Il dataset non ha abbastanza feature per selezionarne 5 casualmente")
        }
        selected_features <- sample(all_features, 5)
        
        best_accuracy <- 0
        best_sensitivity <- NULL
        best_specificity <- NULL
        best_NPV <- NULL
        best_PPV <- NULL
        best_kappa <- NULL
        
        for (i in seq_len(length(all_features) - length(selected_features))) {
          remaining_features <- setdiff(all_features, selected_features)
          if (length(remaining_features) == 0) break
          
          random_feature <- sample(remaining_features, 1)
          current_features <- c(selected_features, random_feature)
          current_data <- dataset[, c(current_features, 'euro_d')]
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
            selected_features <- c(selected_features, random_feature)
            last_predictions <- predictions 
            
            cat("Iteration", i, ": selected feature =", random_feature, "with accuracy =", best_accuracy, "\n")
          } else {
            cat("Iteration", i, ": random feature =", random_feature, "did not improve accuracy\n")
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
          metrics <- calculate_metrics(threshold, predictions, test_data$euro_d)
          thresholds_results <- rbind(thresholds_results, data.frame(Model = model_type, Threshold = threshold * 100, t(metrics)))
        }
      }
      if (best_fold_accuracy > best_overall_accuracy) {
        best_overall_accuracy <- best_fold_accuracy
        best_overall_features <-  best_fold_features
      }
    }
  }
  
  return(list(results = results, best_accuracy = best_overall_accuracy, best_features = unique(best_overall_features), all_predictions = all_predictions, thresholds_results = thresholds_results))
}

############################################################################
# random_forward_selection #################################################
############################################################################
# Random Forward Selection
# This function performs random forward feature selection for model training, aiming to identify a subset of features that maximize model performance. It uses cross-validation to evaluate the performance of selected features for different model types.
#
# Arguments:
# - dataset: The input dataset, which should include the dependent variable 'euro_d' and various features for selection.
#
# Returns:
# - A list containing:
#   1. results: A list of data frames storing performance metrics (Accuracy, Sensitivity, Specificity, Kappa, PPV, NPV) for each model type and fold.
#   2. best_accuracy: The highest accuracy obtained across all models and folds during feature selection.
#   3. best_features: A vector containing the unique best features selected during the process.
#   4. all_predictions: A data frame with predictions for each fold, including fold index, model type, true labels, and predicted probabilities.
#   5. thresholds_results: A data frame containing metrics calculated for various thresholds on the predicted probabilities.
#
# Details:
# - The function sets a seed for reproducibility and initializes variables to store results, predictions, and threshold metrics.
# - It uses 10-fold cross-validation (`createFolds`) to split the dataset into training and test sets for model evaluation.
# - For each specified model type ("rpart", "rf", "glm"):
#   - Randomly selects an initial set of 5 features from the dataset.
#   - Adds features one by one to the selected feature set through random selection, aiming to maximize accuracy at each step.
#   - Evaluates the model's performance using various metrics: Accuracy, Sensitivity, Specificity, Positive Predictive Value (PPV), Negative Predictive Value (NPV), and Kappa.
#   - Updates the selected feature set if the newly added feature improves the accuracy.
#   - Records the best accuracy and selected features for each fold.
#   - Calculates performance metrics at different thresholds (from 0.05 to 0.95) on the predicted probabilities.
# - Returns the overall best accuracy, best features, and performance metrics across all iterations.
#
# Notes:
# - The function stops with an error if the dataset has fewer than 5 features available for selection.
# - If no feature improves accuracy, it prints a message indicating that the selected random feature did not improve the accuracy.
random_forward_selection <- function(dataset) {
  set.seed(1)  
  
  best_overall_accuracy <- 0
  best_overall_features <- NULL
  results <- list()
  all_predictions <- data.frame()
  thresholds_results <- data.frame()
  thresholds <- seq(0.05, 0.95, by = 0.05)
    
    folds <- createFolds(dataset$euro_d, k = 10)
    
    for (model_type in c("rpart", "rf", "glm")) {
      
      best_fold_accuracy <- 0
      best_fold_features <- NULL
      
      for (fold_idx in seq_along(folds)) {
        
        all_features <- setdiff(names(dataset), 'euro_d')
        
        if (length(all_features) < 5) {
          stop("Il dataset non ha abbastanza feature per selezionarne 5 casualmente")
        }
        selected_features <- sample(all_features, 5)
        
        best_accuracy <- 0
        best_sensitivity <- NULL
        best_specificity <- NULL
        best_NPV <- NULL
        best_PPV <- NULL
        best_kappa <- NULL
        
        for (i in seq_len(length(all_features) - length(selected_features))) {
          remaining_features <- setdiff(all_features, selected_features)
          if (length(remaining_features) == 0) break
          
          random_feature <- sample(remaining_features, 1)
          current_features <- c(selected_features, random_feature)
          current_data <- dataset[, c(current_features, 'euro_d')]
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
            selected_features <- c(selected_features, random_feature)
            last_predictions <- predictions 
            
            cat("Iteration", i, ": selected feature =", random_feature, "with accuracy =", best_accuracy, "\n")
          } else {
            cat("Iteration", i, ": random feature =", random_feature, "did not improve accuracy\n")
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
          metrics <- calculate_metrics(threshold, predictions, test_data$euro_d)
          thresholds_results <- rbind(thresholds_results, data.frame(Model = model_type, Threshold = threshold * 100, t(metrics)))
        }
      }
      if (best_fold_accuracy > best_overall_accuracy) {
        best_overall_accuracy <- best_fold_accuracy
        best_overall_features <-  best_fold_features
      }
    }
  
  return(list(results = results, best_accuracy = best_overall_accuracy, best_features = unique(best_overall_features), all_predictions = all_predictions,  thresholds_results = thresholds_results))
}