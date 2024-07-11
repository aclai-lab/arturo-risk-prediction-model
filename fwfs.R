train_model <- function(model_type, train_data) {
  # Assicurarsi che la variabile target sia un fattore per modelli di classificazione
  train_data$euro_d <- as.factor(train_data$euro_d)
  
  # Scegliere e addestrare il modello in base al tipo specificato
  model <- switch(model_type,
                  rpart = rpart(euro_d ~ ., data = train_data, method = "class"),
                  rf = randomForest(euro_d ~ ., data = train_data, type = "classification"),
                  glm = glm(euro_d ~ ., family = binomial(), data = train_data),
                  stop("Model type not supported"))
  
  return(model)
}

# Funzione per fare previsioni con il modello
make_predictions <- function(model, test_data, model_type) {
  predictions <- switch(model_type,
                        rpart = predict(model, test_data, type = "class"),
                        rf = predict(model, test_data, type = "class"),
                        glm = ifelse(predict(model, test_data, type = "response") > 0.5, "yes", "no"),
                        stop("Model type not supported for predictions"))
  
  return(factor(predictions, levels = c("no", "yes")))
}

# Funzione per addestrare il modello
train_model <- function(model_type, train_data) {
  train_data$euro_d <- as.factor(train_data$euro_d)
  
  model <- switch(model_type,
                  rpart = rpart(euro_d ~ ., data = train_data, method = "class"),
                  rf = randomForest(euro_d ~ ., data = train_data, type = "classification"),
                  glm = glm(euro_d ~ ., family = binomial(), data = train_data),
                  stop("Model type not supported"))
  
  return(model)
}

# Funzione per fare previsioni con il modello
make_predictions <- function(model, test_data, model_type) {
  predictions <- switch(model_type,
                        rpart = predict(model, test_data, type = "prob")[, "yes"],
                        rf = predict(model, test_data, type = "prob")[, "yes"],
                        glm = predict(model, test_data, type = "response"),
                        stop("Model type not supported for predictions"))
  
  return(predictions)
}

random_forward_selection <- function(dataset, model_type) {
  set.seed(1)  
  
  all_features <- setdiff(names(dataset), 'euro_d')

  if (length(all_features) < 5) {
    stop("Il dataset non ha abbastanza feature per selezionarne 5 casualmente")
  }
  selected_features <- sample(all_features, 5)
  
  best_accuracy <- 0
  
  for (i in seq_len(length(all_features) - length(selected_features))) {
    remaining_features <- setdiff(all_features, selected_features)
    if (length(remaining_features) == 0) break
    
    random_feature <- sample(remaining_features, 1)
    current_features <- c(selected_features, random_feature)
    current_data <- dataset[, c(current_features, 'euro_d')]
    folds <- createFolds(current_data$euro_d, k = 10)
    accuracies <- numeric(length(folds))
    
    for (fold_idx in seq_along(folds)) {
      train_data <- current_data[-folds[[fold_idx]], ]
      test_data <- current_data[folds[[fold_idx]], ]
      model <- train_model(model_type, train_data)
      predictions <- make_predictions(model, test_data, model_type)
      observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
      cm <- confusionMatrix(predictions, observed_factor)
      accuracies[fold_idx] <- cm$overall['Accuracy']
    }
    
    mean_accuracy <- mean(accuracies)
    
    if (mean_accuracy > best_accuracy) {
      best_accuracy <- mean_accuracy
      selected_features <- c(selected_features, random_feature)
      cat("Iteration", i, ": selected feature =", random_feature, "with accuracy =", best_accuracy, "\n")
    } else {
      cat("Iteration", i, ": random feature =", random_feature, "did not improve accuracy\n")
    }
  }
  
  return(selected_features)
}

best_accuracy <- 0
best_model_type <- NULL
best_selected_features <- NULL
best_sensitivity <- NULL
best_recall <- NULL
results <- list()
all_predictions <- list()
all_observations <- list()

folds <- createFolds(dataset$euro_d, k = 10)

for (model_type in c("rpart", "rf", "glm")) {
  for (fold_idx in seq_along(folds)) {
    train_data <- dataset[-folds[[fold_idx]], ]
    test_data <- dataset[folds[[fold_idx]], ]
    
    selected_features <- random_forward_selection(train_data, model_type)
    
    train_data <- train_data[, c(selected_features, "euro_d")]
    test_data <- test_data[, c(selected_features, "euro_d")]
    
    model <- train_model(model_type, train_data) 
    predictions <- make_predictions(model, test_data, model_type)
    observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
    #cm <- confusionMatrix(factor(predictions, levels = c("no", "yes")),
                          #factor(observed_factor, levels = c("no", "yes")))
    cm <- confusionMatrix(factor(predictions, levels = c("no", "yes")),
                          observed_factor)
    
    #all_predictions[[paste0(model_type, "_fold_", fold_idx)]] <- predictions
    #all_observations[[paste0(model_type, "_fold_", fold_idx)]] <- as.numeric(observed_factor == "yes")
    accuracy <- cm$overall['Accuracy']
    sensitivity <- cm$byClass['Sensitivity']
    recall <- cm$byClass['Recall']
    
    # Aggiungi i risultati alle liste
    results <- append(results, list(data.frame(Model = model_type, Indicator = "Accuracy", Value = accuracy)))
    results <- append(results, list(data.frame(Model = model_type, Indicator = "Sensitivity", Value = sensitivity)))
    results <- append(results, list(data.frame(Model = model_type, Indicator = "Recall", Value = recall)))
   
     # Aggiornamento dei migliori risultati
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_model_type <- model_type
      best_selected_features <- selected_features
      best_sensitivity <- sensitivity
      best_recall <- recall
    }
  }
}

random_forward_selection <- function(dataset) {
  set.seed(1)  

  best_overall_model_type <- NULL
  best_overall_accuracy <- 0
  best_overall_features <- NULL
  best_overall_sensitivity <- NULL
  best_overall_specificity <- NULL
  results <- list()
   
  N <- 5
  for(n in 1:N){
    
    folds <- createFolds(dataset$euro_d, k = 10)
    
    for (model_type in c("rpart", "rf", "glm")) {
      
      for (fold_idx in seq_along(folds)) {
        
        all_features <- setdiff(names(dataset), 'euro_d')
        
        if (length(all_features) < 5) {
          stop("Il dataset non ha abbastanza feature per selezionarne 5 casualmente")
        }
        selected_features <- sample(all_features, 5)
        
        best_accuracy <- 0
        
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
          observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
          cm <- confusionMatrix(predictions, observed_factor)
          
          accuracy <- cm$overall['Accuracy']
          sensitivity <- cm$byClass['Sensitivity']
          specificity <- cm$byClass['Specificity']
          
          if (accuracy >= best_accuracy) {
            best_accuracy <- accuracy
            selected_features <- c(selected_features, random_feature)
            
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


random_forward_selection <- function(dataset) {
  set.seed(1)  
  
  best_overall_model_type <- NULL
  best_overall_accuracy <- 0
  best_overall_features <- NULL
  best_overall_sensitivity <- NULL
  best_overall_specificity <- NULL
  results <- list()
  
  N <- 5
  for(n in 1:N){
    
    folds <- createFolds(dataset$euro_d, k = 10)
    
    for (model_type in c("rpart", "rf", "glm")) {
      
      for (fold_idx in seq_along(folds)) {
        
        all_features <- setdiff(names(dataset), 'euro_d')
        
        if (length(all_features) < 5) {
          stop("Il dataset non ha abbastanza feature per selezionarne 5 casualmente")
        }
        selected_features <- sample(all_features, 5)
        
        best_accuracy <- 0
        
        for (i in seq_len(length(all_features) - length(selected_features))) {
          remaining_features <- setdiff(all_features, selected_features)
          if (length(remaining_features) == 0) break
          
          random_feature <- sample(remaining_features, 1)
          current_features <- c(selected_features, random_feature)
          current_data <- dataset[, c(current_features, 'euro_d')]
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
          observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
          cm <- confusionMatrix(predictions, observed_factor)
          
          accuracy <- cm$overall['Accuracy']
          sensitivity <- cm$byClass['Sensitivity']
          specificity <- cm$byClass['Specificity']
          
          if (accuracy >= best_accuracy) {
            best_accuracy <- accuracy
            selected_features <- c(selected_features, random_feature)
            
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

