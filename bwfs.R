random_backward_selection <- function(dataset, target_var = "euro_d", N = 5, k = 10, seed = 1) {
  set.seed(seed)
  
  best_overall_model <- NULL
  best_overall_accuracy <- 0
  best_overall_features <- NULL
  best_overall_sensitivity <- NULL
  best_overall_specificity <- NULL
  results <- list()
  
  folds <- createFolds(dataset[[target_var]], k = k)
  
  for (n in 1:N) { 
    for (model_type in c("rpart", "rf", "glm")) {
      for (fold_idx in seq_along(folds)) {
        all_features <- setdiff(names(dataset), target_var)
        selected_features <- all_features
        best_accuracy <- 0
        
        
        for (i in 1:length(selected_features)) {
          random_feature <- sample(selected_features, 1)
          current_features <- setdiff(selected_features, random_feature)
          current_data <- dataset[, c(current_features, target_var)]
          train_data <- current_data[-folds[[fold_idx]], ]
          test_data <- current_data[folds[[fold_idx]], ]
          
          model <- train_model(model_type, train_data)
          predictions <- make_predictions(model, test_data, model_type)
          observed_factor <- factor(test_data[[target_var]], levels = c("no", "yes"))
          cm <- confusionMatrix(predictions, observed_factor)
          
          accuracy <- cm$overall['Accuracy']
          sensitivity <- cm$byClass['Sensitivity']
          specificity <- cm$byClass['Specificity']
          
          if (accuracy >= best_accuracy) {
            best_accuracy <- accuracy
            selected_features <- current_features
            cat("Iteration", i, ": removing feature =", random_feature, "get accuracy =", best_accuracy, "\n")
          } else {
            cat("Iteration", i, ": removing feature =", random_feature, "did not improve accuracy\n")
          }
        }
        
        results <- append(results, list(data.frame(Model = model_type, Indicator = "Accuracy", Value = best_accuracy)))
        results <- append(results, list(data.frame(Model = model_type, Indicator = "Sensitivity", Value = sensitivity)))
        results <- append(results, list(data.frame(Model = model_type, Indicator = "Specificity", Value = specificity)))
        
        if (best_accuracy > best_overall_accuracy) {
          best_overall_accuracy <- best_accuracy
          best_overall_features <- selected_features
          best_overall_model <- model_type
          best_overall_sensitivity <- sensitivity
          best_overall_specificity <- specificity
        }
      }
    }
  }
  
  return(list(best_features = unique(best_overall_features), results = results, best_model = best_overall_model, best_sensitivity = best_overall_sensitivity, best_specificity = best_overall_specificity, best_accuracy = best_overall_accuracy))
}

random_backward_selection <- function(dataset, target_var = "euro_d", N = 5, k = 10, seed = 1) {
  set.seed(seed)
  
  best_overall_model <- NULL
  best_overall_accuracy <- 0
  best_overall_features <- NULL
  best_overall_sensitivity <- NULL
  best_overall_specificity <- NULL
  results <- list()
  
  folds <- createFolds(dataset[[target_var]], k = k)
  
  for (n in 1:N) { 
    for (model_type in c("rpart", "rf", "glm")) {
      for (fold_idx in seq_along(folds)) {
        all_features <- setdiff(names(dataset), target_var)
        selected_features <- all_features
        best_accuracy <- 0
        
        
        for (i in 1:length(selected_features)) {
          random_feature <- sample(selected_features, 1)
          current_features <- setdiff(selected_features, random_feature)
          current_data <- dataset[, c(current_features, target_var)]
          train_data <- current_data[-folds[[fold_idx]], ]
          test_data <- current_data[folds[[fold_idx]], ]
          
          # Bilanciamento dei dati di addestramento
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
          observed_factor <- factor(test_data[[target_var]], levels = c("no", "yes"))
          cm <- confusionMatrix(predictions, observed_factor)
          
          accuracy <- cm$overall['Accuracy']
          sensitivity <- cm$byClass['Sensitivity']
          specificity <- cm$byClass['Specificity']
          
          if (accuracy >= best_accuracy) {
            best_accuracy <- accuracy
            selected_features <- current_features
            cat("Iteration", i, ": selected feature =", random_feature, "with accuracy =", best_accuracy, "\n")
          } else {
            cat("Iteration", i, ": random feature =", random_feature, "did not improve accuracy\n")
          }
        }
        
        results <- append(results, list(data.frame(Model = model_type, Indicator = "Accuracy", Value = best_accuracy)))
        results <- append(results, list(data.frame(Model = model_type, Indicator = "Sensitivity", Value = sensitivity)))
        results <- append(results, list(data.frame(Model = model_type, Indicator = "Specificity", Value = specificity)))
        
        if (best_accuracy > best_overall_accuracy) {
          best_overall_accuracy <- best_accuracy
          best_overall_features <- selected_features
          best_overall_model <- model_type
          best_overall_sensitivity <- sensitivity
          best_overall_specificity <- specificity
        }
      }
    }
  }
  
  return(list(best_features = unique(best_overall_features), results = results, best_model = best_overall_model, best_sensitivity = best_overall_sensitivity, best_specificity = best_overall_specificity, best_accuracy = best_overall_accuracy))
}