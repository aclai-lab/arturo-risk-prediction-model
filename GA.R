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

select_features <- function(chromosome, dataset) {
  selected_vars <- which(chromosome == 1)
  selected_data <- dataset[, c(selected_vars, which(names(dataset) == "euro_d"))]
  return(selected_data)
}

# Funzione di fitness
fitness_function <- function(chromosome, dataset) {
  selected_data <- select_features(chromosome, dataset)
  
  set.seed(123)
  accuracies <- numeric(3)
  
  for (i in 1:3) {
    train_indices <- createDataPartition(selected_data$euro_d, p = 0.7, list = FALSE)
    train_data <- selected_data[train_indices, ]
    test_data <- selected_data[-train_indices, ]
    
    fold_accuracies <- numeric(3)
    for (model_type in c("rpart", "rf", "glm")) {
      model <- train_model(model_type, train_data)
      predictions <- make_predictions(model, test_data, model_type)
      observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
      cm <- confusionMatrix(predictions, observed_factor)
      fold_accuracies[which(c("rpart", "rf", "glm") == model_type)] <- cm$overall['Accuracy']
    }
    accuracies[i] <- mean(fold_accuracies)
  }
  
  return(mean(accuracies))
}

# Funzione per eseguire l'algoritmo genetico e selezionare le caratteristiche
perform_ga_and_cross_validation <- function(dataset) {
  numCores <- detectCores() - 1 
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  fitness_wrapper <- function(chromosome) {
    fitness_function(chromosome, dataset)
  }
  
  GA <- ga(type = "binary", 
           fitness = fitness_wrapper, 
           nBits = ncol(dataset) - 1, 
           popSize = 50, 
           maxiter = 100, 
           pmutation = 0.2, 
           seed = 123, 
           parallel = TRUE)
  
  stopCluster(cl)
  registerDoSEQ()
  
  best_subset <- GA@solution[1, ]
  selected_vars <- which(best_subset == 1)
  selected_features <- names(dataset)[selected_vars]
  print(selected_features)
  
  set.seed(123)
  N <- 5
  results <- list()
  
  for (n in 1:N) {
    minor_class <- subset(dataset[, c(selected_features, "euro_d")], euro_d == "yes")
    major_class <- subset(dataset[, c(selected_features, "euro_d")], euro_d == "no")
    
    major_samples <- major_class[sample(nrow(major_class), nrow(minor_class)), ]
    balanced_dataset <- rbind(minor_class, major_samples)
    
    folds <- createFolds(balanced_dataset$euro_d, k = 10, list = TRUE)
    
    for (model_type in c("rpart", "rf", "glm")) {
      for (fold_idx in seq_along(folds)) {
        train_data <- balanced_dataset[-folds[[fold_idx]], ]
        test_data <- balanced_dataset[folds[[fold_idx]], ]
        model <- train_model(model_type, train_data)
        predictions <- make_predictions(model, test_data, model_type)
        observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
        cm <- confusionMatrix(predictions, observed_factor)
        results <- append(results, list(data.frame(Model = model_type, Indicator = "Accuracy", Value = cm$overall['Accuracy'])))
        results <- append(results, list(data.frame(Model = model_type, Indicator = "Sensitivity", Value = cm$byClass['Sensitivity'])))
        results <- append(results, list(data.frame(Model = model_type, Indicator = "Recall", Value = cm$byClass['Recall'])))
      }
    }
  }
  
  final_results <- do.call(rbind, results)
  return(final_results)
}

final_results <- perform_ga_and_cross_validation(dataset)train_model <- function(model_type, train_data) {
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

select_features <- function(chromosome, dataset) {
  selected_vars <- which(chromosome == 1)
  selected_data <- dataset[, c(selected_vars, which(names(dataset) == "euro_d"))]
  return(selected_data)
}

# Funzione di fitness
fitness_function <- function(chromosome, dataset) {
  selected_data <- select_features(chromosome, dataset)
  
  set.seed(123)
  accuracies <- numeric(3)
  
  for (i in 1:3) {
    train_indices <- createDataPartition(selected_data$euro_d, p = 0.7, list = FALSE)
    train_data <- selected_data[train_indices, ]
    test_data <- selected_data[-train_indices, ]
    
    fold_accuracies <- numeric(3)
    for (model_type in c("rpart", "rf", "glm")) {
      model <- train_model(model_type, train_data)
      predictions <- make_predictions(model, test_data, model_type)
      observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
      cm <- confusionMatrix(predictions, observed_factor)
      fold_accuracies[which(c("rpart", "rf", "glm") == model_type)] <- cm$overall['Accuracy']
    }
    accuracies[i] <- mean(fold_accuracies)
  }
  
  return(mean(accuracies))
}

# Funzione per eseguire l'algoritmo genetico e selezionare le caratteristiche
perform_ga_and_cross_validation <- function(dataset) {
  numCores <- detectCores() - 1 
  cl <- makeCluster(numCores)
  registerDoParallel(cl)
  
  fitness_wrapper <- function(chromosome) {
    fitness_function(chromosome, dataset)
  }
  
  GA <- ga(type = "binary", 
           fitness = fitness_wrapper, 
           nBits = ncol(dataset) - 1, 
           popSize = 50, 
           maxiter = 100, 
           pmutation = 0.2, 
           seed = 123, 
           parallel = TRUE)
  
  stopCluster(cl)
  registerDoSEQ()
  
  best_subset <- GA@solution[1, ]
  selected_vars <- which(best_subset == 1)
  selected_features <- names(dataset)[selected_vars]
  print(selected_features)
  
  set.seed(123)
  N <- 5
  results <- list()
  
  for (n in 1:N) {
    minor_class <- subset(dataset[, c(selected_features, "euro_d")], euro_d == "yes")
    major_class <- subset(dataset[, c(selected_features, "euro_d")], euro_d == "no")
    
    major_samples <- major_class[sample(nrow(major_class), nrow(minor_class)), ]
    balanced_dataset <- rbind(minor_class, major_samples)
    
    folds <- createFolds(balanced_dataset$euro_d, k = 10, list = TRUE)
    
    for (model_type in c("rpart", "rf", "glm")) {
      for (fold_idx in seq_along(folds)) {
        train_data <- balanced_dataset[-folds[[fold_idx]], ]
        test_data <- balanced_dataset[folds[[fold_idx]], ]
        model <- train_model(model_type, train_data)
        predictions <- make_predictions(model, test_data, model_type)
        observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
        cm <- confusionMatrix(predictions, observed_factor)
        results <- append(results, list(data.frame(Model = model_type, Indicator = "Accuracy", Value = cm$overall['Accuracy'])))
        results <- append(results, list(data.frame(Model = model_type, Indicator = "Sensitivity", Value = cm$byClass['Sensitivity'])))
        results <- append(results, list(data.frame(Model = model_type, Indicator = "Recall", Value = cm$byClass['Recall'])))
      }
    }
  }
  
  final_results <- do.call(rbind, results)
  return(final_results)
}

final_results <- perform_ga_and_cross_validation(dataset)

set.seed(1)

# Caricamento del dataset
# Si assume che il dataset sia già caricato in una variabile chiamata `dataset`
# Convertiamo la colonna euro_d in fattore per la classificazione binaria
dataset$euro_d <- as.factor(dataset$euro_d)

# Calcola la percentuale di valori mancanti per ogni colonna
missing_perc <- colSums(is.na(dataset)) / nrow(dataset)

# Filtra le colonne con meno del 50% di valori mancanti
dataset_filtered <- dataset[, missing_perc < 0.5]

# Converti le variabili carattere in fattori
dataset<- dataset %>%
  mutate(across(where(is.character), as.factor))

# Gestisci i valori mancanti rimanenti, qui si usano medie e modali
preProcess_missingdata <- preProcess(dataset_filtered, method = 'medianImpute')
dataset_imputed <- predict(preProcess_missingdata, dataset_filtered)

# Dividi i dati in training e test set
set.seed(1)
trainIndex <- createDataPartition(dataset_imputed$euro_d, p = 0.7, list = FALSE)
train_data <- dataset_imputed[trainIndex, ]
test_data <- dataset_imputed[-trainIndex, ]

# Rimuovi la colonna euro_d dai predittori
train_x <- train_data[, setdiff(names(train_data), "euro_d")]
train_y <- train_data$euro_d

# Creazione del controllo GAFS con Random Forest
ctrl <- gafsControl(functions = rfGA, method = "cv", number = 3)

# Esecuzione della ricerca GAFS
rf_search <- gafs(
  x = train_x,
  y = train_y,
  iters = 3,
  gafsControl = ctrl
)

selected_features <- rf_search$optVariables
print(selected_features)

# Preparare i dati di test con le feature selezionate
test_x <- test_data[, selected_features]
test_y <- test_data$euro_d

# Visualizzazione dei risultati
print(rf_search)

##
##
##

N <- 5
results <- list()

for (n in 1:N) {  # N rappresenta il numero di ripetizioni
  folds <- createFolds(dataset$euro_d, k = 10)
  
  for (model_type in c("rpart", "rf", "glm")) {
    best_accuracy <- 0
    best_selected_features <- NULL
    for (fold_idx in seq_along(folds)) {
      train_data <- dataset[-folds[[fold_idx]], ]
      test_data <- dataset[folds[[fold_idx]], ]
      
      # Bilanciamento dei dati di addestramento
      minor_class <- subset(train_data, euro_d == "yes")
      major_class <- subset(train_data, euro_d == "no")
      
      if (nrow(minor_class) > nrow(major_class)) {
        minor_class <- subset(train_data, euro_d == "no")
        major_class <- subset(train_data, euro_d == "yes")
      }
      
      major_samples <- major_class[sample(nrow(major_class), nrow(minor_class)), ]
      balanced_train_data <- rbind(minor_class, major_samples)
      
      ctrl <- gafsControl(functions = rfGA, method = "cv", number = 3)
      rf_search <- gafs(
        x = balanced_train_data[, setdiff(names(balanced_train_data), "euro_d")],
        y = balanced_train_data$euro_d,
        iters = 3,
        gafsControl = ctrl
      )
      
      # Ottenere le feature selezionate
      selected_features <- rf_search$optVariables
      
      # Seleziona solo le caratteristiche selezionate per i dati di addestramento e test
      balanced_train_data <- balanced_train_data[, c(selected_features, "euro_d")]
      test_data <- test_data[, c(selected_features, "euro_d")]
      
      # Addestramento del modello
      model <- train_model(model_type, balanced_train_data)
      
      # Predizioni
      predictions <- make_predictions(model, test_data, model_type)
      
      # Confusion Matrix
      observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
      cm <- confusionMatrix(predictions, observed_factor)
      
      # Salvataggio dei risultati
      accuracy <- cm$overall['Accuracy']
      sensitivity <- cm$byClass['Sensitivity']
      recall <- cm$byClass['Recall']
      
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
}


results <- list()

folds <- createFolds(dataset$euro_d, k = 10)

for (model_type in c("rpart", "rf", "glm")) {
  best_accuracy <- 0
  best_selected_features <- NULL
  for (fold_idx in seq_along(folds)) {
    train_data <- dataset[-folds[[fold_idx]], ]
    test_data <- dataset[folds[[fold_idx]], ]
    
    ctrl <- gafsControl(
      functions = rfGA,
      method = "repeatedcv",
      number = 10,
      repeats = 3,
      allowParallel = TRUE,
      verbose = TRUE
    )
    
    rf_search <- gafs(
      x = train_data[, setdiff(names(train_data), "euro_d")],
      y = train_data$euro_d,
      iters = 50,
      popSize = 50,
      gafsControl = ctrl,
      pcrossover = 0.8,
      pmutation = 0.1
    )
    
    # Ottenere le feature selezionate
    selected_features <- rf_search$optVariables
    
    # Seleziona solo le caratteristiche selezionate per i dati di addestramento e test
    train_data <- train_data[, c(selected_features, "euro_d")]
    test_data <- test_data[, c(selected_features, "euro_d")]
    
    # Addestramento del modello
    model <- train_model(model_type, train_data)
    
    # Predizioni
    predictions <- make_predictions(model, test_data, model_type)
    
    # Confusion Matrix
    observed_factor <- factor(test_data$euro_d, levels = c("no", "yes"))
    cm <- confusionMatrix(predictions, observed_factor)
    
    # Salvataggio dei risultati
    accuracy <- cm$overall['Accuracy']
    sensitivity <- cm$byClass['Sensitivity']
    specificity <- cm$byClass['Specificity']
    
    results <- append(results, list(data.frame(Model = model_type, Indicator = "Accuracy", Value = accuracy)))
    results <- append(results, list(data.frame(Model = model_type, Indicator = "Sensitivity", Value = sensitivity)))
    results <- append(results, list(data.frame(Model = model_type, Indicator = "Specificity", Value = specificity)))
    
    # Aggiornamento dei migliori risultati
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_model_type <- model_type
      best_selected_features <- selected_features
      best_sensitivity <- sensitivity
      best_specificity <- specificity
    }
  }
}
stopCluster(cl)