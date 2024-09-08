  fitness_function <- function(selected_features, dataset, train_index, test_index, model_type, fold_idx) {
    selected_features <- as.logical(selected_features)
    if (sum(selected_features) == 0) return(list(accuracy = 0, sensitivity = NA, specificity = NA, selected_features = selected_features))
    
    features <- names(dataset)[selected_features]
    current_data <- dataset[, c(features, 'euro_d')]
    train_data <- current_data[train_index, ]
    test_data <- current_data[test_index, ]

    train_data <- current_data[train_index, ]
    test_data <- current_data[test_index, ]
        
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
    observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
    cm <- confusionMatrix(factor(ifelse(predictions > 0.5, "yes", "no"), levels = c("no", "yes")), observed_factor)
    
    accuracy <- cm$overall['Accuracy']
    sensitivity <- cm$byClass['Sensitivity']
    specificity <- cm$byClass['Specificity']
    kappa <- cm$overall['Kappa']
    PPV <- cm$byClass['Pos Pred Value']
    NPV <- cm$byClass['Neg Pred Value']

    return(list(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity, selected_features = selected_features, true_label = test_data$euro_d, predicted_prob = predictions, PPV = PPV, NPV = NPV, kappa = kappa))
  }
  
  genetic_feature_selection <- function(dataset, model_types = c("rpart", "rf", "glm"), n_generations = 50, pop_size = 50) {
    set.seed(1)
    
    best_overall_model <- NULL
    best_overall_accuracy <- 0
    best_overall_features <- NULL
    best_overall_sensitivity <- NULL
    best_overall_specificity <- NULL
    results <- list()
    ga_summaries <- list()
    all_predictions <- data.frame()
    for(n in 1:5){
    folds <- createFolds(dataset$euro_d, k = 10)
    for (model_type in model_types) {
      best_fold_accuracy <- 0
      best_fold_features <- NULL
      best_fold_sensitivity <- NULL
      best_fold_specificity <- NULL
      
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
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "PPV", Value = NPV)))
        results <- append(results, list(data.frame(Model = model_type, Fold = fold_idx, Indicator = "NPV", Value = PPV)))
        
        # Salvare le predizioni e le etichette effettive
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
          best_fold_sensitivity <- sensitivity
          best_fold_specificity <- specificity
        }
        
        # Aggiungi i dettagli del modello genetico al riepilogo
        ga_summaries[[paste(model_type, fold_idx, sep = "_")]] <- list(
          solution = best_solution,
          fitnessValue = ga_model@fitnessValue,
          summary = ga_model@summary,
          iter = ga_model@iter,
          run = ga_model@run
        )
      }
      
      if (best_fold_accuracy > best_overall_accuracy) {
        best_overall_accuracy <- best_fold_accuracy
        best_overall_features <- best_fold_features
        best_overall_model <- model_type
        best_overall_sensitivity <- best_fold_sensitivity
        best_overall_specificity <- best_fold_specificity
      }
    }
    }
    
    return(list(
      best_features = unique(best_overall_features),
      results = results,
      best_model = best_overall_model,
      best_sensitivity = best_overall_sensitivity,
      best_specificity = best_overall_specificity,
      best_accuracy = best_overall_accuracy,
      ga_summaries = ga_summaries,
      all_pred_ga = all_predictions
    ))
  }
  