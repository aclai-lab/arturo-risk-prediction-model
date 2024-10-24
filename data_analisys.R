############################################################################
# DATA IMPORT ##############################################################
############################################################################

# Source custom scripts
source("install.R")       # Import R library
source("ddop_data.R")     # Import and Cleaning Data
source("ddop_utility.R")  # Utility functions for Data Analysis
source("ddop_plot.R")     # Plotting functions for Data Analysis
source("cv.R")            # Cross Validation
source("fwfs.R")          # Forward Feature Selection
source("bwfs.R")          # Backward Feature Selection
source("genetic.R")       # Feature Selection with Genetic Algorithm

############################################################################
# RUNNING MAIN FUNCTION ####################################################
############################################################################

rpm <- function(){
  
  dataset <- read_sav("data/dataset.sav")
  
  ############################################################################
  # FOR UNBALANCED DATASET ###################################################
  ############################################################################
 
  results_cv <- cv(dataset)
  print_summary_results(results_cv$results)
  calibration_curves_docx(results_cv$all_predictions, 
                          "calibration_curves_cv_no_balance.docx", 
                          "Calibration Curves for Cross Validation on unbalanced dataset")
  risk_table_docx(results_cv$thresholds_results, 
                  "risk_analysis_cv_no_balance.docx", 
                  "with Cross Validation on unbalanced dataset")
  
  results_fwfs <- random_forward_selection(dataset)
  print_summary_results(results_fwfs$results)
  calibration_curves_docx(results_fwfs$all_predictions,
                          "calibration_curves_fwfs_no_balance.docx", 
                          "Calibration Curves for Forward Selection on unbalanced dataset")
  selected_features_table_docx(dataset, results_fwfs$best_features, 
                               "selected_features_fwfs_no_balance.docx",
                               "Selected Features by Forward FS on unbalanced dataset")
  risk_table_docx(results_fwfs$thresholds_results, 
                  "risk_analysis_fwfs_no_balance.docx", 
                  "with Forward Selection on unbalanced dataset")
  
  results_bwfs <- random_backward_selection(dataset)
  print_summary_results(results_bwfs$results)
  calibration_curves_docx(results_bwfs$all_predictions, 
                          "calibration_curves_bwfs_no_balance.docx", 
                          "Calibration Curves for Backward Selection on unbalanced dataset")
  selected_features_table_docx(dataset, results_bwfs$best_features,
                               "selected_features_bwfs_no_balance.docx",
                               "Selected Features by Backword FS on unbalanced dataset")
  risk_table_docx(results_bwfs$thresholds_results, 
                  "risk_analysis_bwfs_no_balance.docx", 
                  "with Backward Selection on unbalanced dataset")
  
  results_ga <- genetic_feature_selection(dataset)
  print_summary_results(results_ga$results)
  calibration_curves_docx(results_ga$all_predictions, 
                          "calibration_curves_ga_no_balance.docx",
                          "Calibration Curves for Genetic Selection on unbalanced dataset")
  selected_features_table_docx(dataset, results_ga$best_features, 
                               "selected_features_ga_no_balance.docx",
                               "Selected Features by Genetic Selection on unbalanced dataset")
  risk_table_docx(results_ga$thresholds_results, 
                  "risk_analysis_ga_no_balance.docx", 
                  "with Genetic Selection on unbalanced dataset")
  
  roc_curves_docx(list(results_cv$all_predictions, results_fwfs$all_predictions, results_bwfs$all_predictions, results_ga$all_predictions), "roc_curves_no_balance.docx", "ROC Curves for unbalanced dataset",)
 
  ############################################################################
  # FOR UNDERSAMPLING DATASET ################################################
  ############################################################################
  
  results_cv_under <- double_cv(dataset, balance = "under")
  print_summary_results(results_cv_under$results)
  calibration_curves_docx(results_cv_under$all_predictions, 
                          "calibration_curves_cv_balance_under.docx", 
                          "Calibration Curves for Cross Validation on undersampling balanced dataset")
  risk_table_docx(results_cv_under$thresholds_results, 
                  "risk_analysis_cv_balance_under.docx", 
                  "with Cross Validation on undersampling balanced dataset")
  
  results_fwfs_bl_under <- random_forward_selection_bl(dataset, balance = "under")
  print_summary_results(results_fwfs_bl_under$results)
  calibration_curves_docx(results_fwfs_bl_under$all_predictions, 
                          "calibration_curves_fwfs_balance_under.docx", 
                          "Calibration Curves for Forward Selection on undersampling balanced dataset")
  selected_features_table_docx(dataset, results_fwfs_bl_under$best_features, 
                               "selected_features_fwfs_balance_under.docx",
                               "Selected Features by Forward FS on undersampling balanced dataset")
  risk_table_docx(results_fwfs_bl_under$thresholds_results, 
                  "risk_analysis_fwfs_balance_under.docx",
                  "with Forward Selection on undersampling balanced dataset")
  
  results_bwfs_bl_under<- random_backward_selection_bl(dataset, balance = "under")
  print_summary_results(results_bwfs_bl_under$results)
  calibration_curves_docx(results_bwfs_bl_under$all_predictions, 
                          "calibration_curves_bwfs_balance_under.docx", 
                          "Calibration Curves for Backward Selection on undersampling balanced dataset")
  selected_features_table_docx(dataset, results_bwfs_bl_under$best_features, 
                               "selected_features_bwfs_balance_under.docx",
                               "Selected Features by Backward FS on undersampling balanced dataset")
  risk_table_docx(results_bwfs_bl_under$thresholds_results, 
                  "risk_analysis_bwfs_balance_under.docx", 
                  "with Backward Selection on undersampling balanced dataset")
  
  results_ga_bl_under <- genetic_feature_selection_bl(dataset, balance = "under") 
  print_summary_results(results_ga_bl_under$results)
  calibration_curves_docx(results_ga_bl_under$all_predictions, 
                          "calibration_curves_ga_balance_under.docx",  
                          "Calibration Curves for Genetic Selection on undersampling balanced dataset")
  selected_features_table_docx(dataset, results_ga_bl_under$best_features, 
                               "selected_features_ga_balance_under.docx",
                               "Selected Features by Genetic Selection on undersampling balanced dataset")
  risk_table_docx(results_ga_bl_under$thresholds_results, 
                  "risk_analysis_ga_balance_under.docx", 
                  "with Genetic Selection on undersampling balanced dataset")
  
  roc_curves_docx(list(results_cv_under$all_predictions, results_fwfs_bl_under$all_predictions, results_bwfs_bl_under$all_predictions, results_ga_bl_under$all_predictions), "roc_curves_no_balance.docx", "ROC Curves for undersampling balanced dataset")
  
  ############################################################################
  # FOR OVERSAMPLING DATASET #################################################
  ############################################################################
  
  results_cv_over <- double_cv(dataset, balance = "over")
  print_summary_results(results_cv_over$results)
  calibration_curves_docx(results_cv_over$all_predictions, 
                          "calibration_curves_cv_balance_over.docx",  
                          "Calibration Curves for Cross Validation on oversampling balanced dataset")
  risk_table_docx(results_cv_over$thresholds_results, 
                  "risk_analysis_cv_balance_over.docx", 
                  "with Cross Validation on oversampling balanced dataset")
  
  results_fwfs_bl_over <- random_forward_selection_bl(dataset, balance = "over")
  print_summary_results(results_fwfs_bl_over$results)
  calibration_curves_docx(results_fwfs_bl_over$all_predictions, 
                          "calibration_curves_fwfs_balance_over.docx", 
                          "Calibration Curves for Forward Selection on oversampling balanced dataset")
  selected_features_table_docx(dataset, results_fwfs_bl_over$best_features,  
                               "selected_features_fwfs_balance_over.docx",
                               "Selected Features by Forward FS on oversampling balanced dataset")
  risk_table_docx(results_fwfs_bl_over$thresholds_results, 
                  "risk_analysis_fwfs_balance_over.docx", 
                  "with Forward Selection on oversampling balanced dataset")
  
  results_bwfs_bl_over <- random_backward_selection_bl(dataset, balance = "over")
  print_summary_results(results_bwfs_bl_over$results)
  calibration_curves_docx(results_bwfs_bl_over$all_predictions, 
                          "calibration_curves_bwfs_balance_over.docx", 
                          "Calibration Curves for Backward Selection on oversampling balanced dataset")
  selected_features_table_docx(dataset, results_bwfs_bl_over$best_features,  
                               "selected_features_bwfs_balance_over.docx",
                               "Selected Features by Backward FS on oversampling balanced dataset")
  risk_table_docx(results_bwfs_bl_over$thresholds_results, 
                  "risk_analysis_bwfs_balance_over.docx", 
                  "with Bacward Selection on oversampling balanced dataset")
  
  results_ga_bl_over <- genetic_feature_selection_bl(dataset, balance = "over") 
  print_summary_results(results_ga_bl_over$results)
  calibration_curves_docx(results_ga_bl_over$all_predictions, 
                          "calibration_curves_ga_balance_over.docx",  
                          "Calibration Curves for Genetic Selection on oversampling balanced dataset")
  selected_features_table_docx(dataset, results_ga_bl_over$best_features,  
                               "selected_features_ga_balance_over.docx",
                               "Selected Features by Genetic Selection on oversampling balanced dataset")
  risk_table_docx(results_ga_bl_over$thresholds_results, 
                  "risk_analysis_ga_balance_over.docx", 
                  "with Genetic Selection on oversampling balanced dataset")
  
  roc_curves_docx(list(results_cv_over$all_predictions, results_fwfs_bl_over$all_predictions, results_bwfs_bl_over$all_predictions, results_ga_bl_over$all_predictions), "roc_curves_balance_oversampling.docx", "ROC Curves for oversampling balanced dataset")
  ############################################################################
  # REGRESSION FROM GA #######################################################
  ############################################################################
  dataset <- read_sav("data/score_dataset.sav")
  dataset <- select(-c(results_ga_bl_under$selected_feature))
  results_cv_score_ga <- score_cross_validation(dataset)
  
  ############################################################################
  # REGRESSION ###############################################################
  ############################################################################
  
  dataset <- read_sav("data/score_dataset.sav")
  
  results_cv_score <- score_cross_validation(dataset)
  print_summary_results(results_cv_score$results)
  print_score_summary_results(results_cv_score$predictions)
  all_predictions<- results_cv_score$all_predictions %>%
    mutate(
      TrueLabel = ifelse(TrueLabel >= 4, "yes", "no"),
      TrueLabel = factor(TrueLabel, levels = c("no", "yes"))
    )
  regression_roc_curve_docx(all_predictions,
                  "roc_curves_cv_regression.docx", 
                  "ROC Curves for Linear Regression with Cross Validation")
  results_ga_score <- score_genetic_feature_selection(dataset)
  print_summary_results(results_ga_score$results)
  print_score_summary_results(results_ga_score$predictions)
  all_predictions<- results_ga_score$all_predictions %>%
    mutate(
      TrueLabel = ifelse(TrueLabel >= 4, "yes", "no"),
      TrueLabel = factor(TrueLabel, levels = c("no", "yes"))
    )
  regression_roc_curve_docx(all_predictions,
                  "roc_curves_ga_regression.docx", 
                  "ROC Curves for Linear Regression with Genetic Algorithm")
  selected_features_table_docx(dataset, results_ga_score$best_features,  
                               "selected_features_ga_score.docx",
                               "Selected Features by Genetic Selection in Logistic Regression")
}