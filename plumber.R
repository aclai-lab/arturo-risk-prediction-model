# plumber.R
library(plumber)
library(jsonlite)

model <- readRDS("glm_regression_with_ga.rds")
calculate_probability <- function(score, threshold = 4) {
  # Applica la funzione logistica per ottenere una probabilità
  prob <- 1 / (1 + exp(-(score - threshold)))
  return(prob)
}

#* @post /predict
function(req) {
  data <- fromJSON(req$postBody)
  # Ottieni la previsione dello score continuo
  prediction <- predict(model, newdata = as.data.frame(data))
  
  # Calcola la probabilità che lo score predetto sia >= 4
  predicted_prob <- calculate_probability(prediction, threshold = 4)
  
  # Restituisce sia la previsione dello score che la probabilità
  list(prediction = prediction, predicted_prob = predicted_prob)
}
