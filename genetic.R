library(caret)
library(GA)

# Funzione di valutazione per l'algoritmo genetico con cross-validation
fitness_function <- function(selected_features, dataset, model_types , n_folds = 10) {
  
  selected_features <- as.logical(selected_features)
  if (sum(selected_features) == 0) {
    return(0) # Fitness è zero se nessuna feature è selezionata
  }
  
  features <- names(dataset)[selected_features]
  current_data <- dataset[, c(features, 'euro_d')]
  
  best_overall_accuracy <- 0
  best_overall_features <- NULL
  best_overall_model <- NULL
  
  for (model_type in model_types) {
    
    folds <- createFolds(current_data$euro_d, k = n_folds)
    best_fold_accuracy <- 0
    best_fold_features <- NULL

    all_predictions <- data.frame()
    results <- list()
  
    for (fold_idx in seq_along(folds)) {
      train_index <- -folds[[fold_idx]]
      test_index <- folds[[fold_idx]]
    
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

      if (accuracy > best_fold_accuracy) {
        best_fold_accuracy <- accuracy
        best_fold_features <- selected_features
      }

      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Accuracy", Value = accuracy)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Sensitivity", Value = sensitivity)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Specificity", Value = specificity)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "Kappa", Value = kappa)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "PPV", Value = PPV)))
      results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "NPV", Value = NPV)))
       
      fold_predictions <- data.frame(
        Fold = fold_idx,
        Model = model_type,
        TrueLabel = test_data$euro_d,
        PredictedProb = predictions
      )
      all_predictions <- rbind(all_predictions, fold_predictions)       
    }

    if (best_fold_accuracy > best_overall_accuracy) {
      best_overall_accuracy <- best_fold_accuracy
      best_overall_features <- best_fold_features
      best_overall_model <- model_type
    }
  }  

  return(list(
    best_features = unique(best_overall_features),
    best_model = best_overall_model,
    best_accuracy = best_overall_accuracy,
    result = results,
    ga_predictions = all_predictions
  ))
}

genetic_feature_selection <- function(dataset, model_types = c("rpart", "rf", "glm"), n_generations = 50, pop_size = 50, run = 50) {
  
  set.seed(1)  
  ga_summaries <- list()
  
  ga_model <- ga(
    type = "binary",
    fitness = function(selected_features) {
      res <- fitness_function(selected_features, dataset, model_types)
      return(res$best_accuracy) # Fitness è basata solo sull'accuratezza
    },
    nBits = length(setdiff(names(dataset), 'euro_d')),
    maxiter = n_generations,
    popSize = pop_size,
    run = run,
    keepBest = TRUE
  )

  best_solution <- ga_model@solution[1, ]
  selected_features <- names(dataset)[best_solution == 1]
  res <- fitness_function(best_solution, dataset, model_types)
  best_accuracy <- res@best_accuracy
  best_features <- res@best_features
  ga_model <- res@best_model
  results <- res@result
  ga_predictions <- res@ga_predictions
    
  ga_summaries[[model_type]] <- list(
    solution = best_solution,
    fitnessValue = ga_model@fitnessValue,
    summary = ga_model@summary,
    iter = ga_model@iter,
    run = ga_model@run
  )
   
  return(list(
    best_features = best_features,
    results = result,
    best_model = best_model,
    best_accuracy = best_accuracy,
    ga_summaries = ga_summaries,
    ga_predictions = ga_predictions
  ))
}

results <- genetic_feature_selection(dataset)
