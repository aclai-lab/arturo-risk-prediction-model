############################################################################
# FUNCTION #################################################################
############################################################################

# Function to find variable code by variable label
find_variable_by_label <- function(data, label) {
  for (var_name in names(data)) {
    # Obtein label
    var_label <- attr(data[[var_name]], "label")
    # Label Check
    if (!is.null(var_label) && any(var_label == label)) {
      return(var_name)
    }
  }
  return(NULL) 
}

# Function to find variable label by variable code
get_label_by_variable <- function(data, variable_name) {
  # check if variable exists
  if (!variable_name %in% names(data)) {
    stop("La variabile specificata non esiste nel dataframe.")
  }
  # Obtein variable
  variable_label <- attr(data[[variable_name]], "label")
  # Check on label
  if (is.null(variable_label)) {
    return("The variable does not have label attribute.")
  } 
  return(variable_label)
}

get_labels_by_variable <- function(dataset, var_name) {
  if (!var_name %in% names(dataset)) {
    stop("The variable name provided does not exist in the dataset.")
  }
  var <- dataset[[var_name]]
  if (is.null(attr(var, "labels"))) {
    return("The variable does not have labels attribute.")
  }
  labels <- attr(var, "labels")
  labels_df <- data.frame(value = as.numeric(labels), label = names(labels), stringsAsFactors = FALSE)
  return(labels_df)
}


# Function for find variable column domain
get_domain <- function(column) {
  if (is.labelled(column)){
    domain = val_labels(column)
    return(paste(domain, collapse = "/"))  
  }
  else{
    return("")
  }
}

# Function for find unlabelled variable column domain
get_unlabel_domain <- function(column) {
  unique_values <- sort(unique(na.omit(column)))
  return(paste(unique_values, collapse = "/"))
}

check_domain <- function(column) {
  unique_values <- sort(unique(na.omit(column)))
  if (is.numeric(unique_values)) {
    if (length(unique_values) > 1 && all(diff(unique_values) == 1)) {
      return(paste(range(unique_values), collapse = ":")) 
    } else {
      return(paste(unique_values, collapse = "/"))
    }
  } else {
    return(paste(unique_values, collapse = "/"))
  }
}

# Function to check if variables domain is sequential or not
is_sequential <- function(domain) {
  str_detect(domain, pattern = ":")
}

# Function to remove negative labels
update_labels <- function(column) {
  if (is.labelled(column)) { 
    labels <- attr(column, "labels")
    
    new_labels <- labels[labels >= 0]
    
    column <- labelled(column, labels = new_labels, label = attr(column, "label"))
  }
  return(column)
}

# function for count and list variable occurrencies in a dataset
create_var_occ <- function(dataset, type = "") {
  var_occ <- list()
  
  for (var in names(dataset)) {
    # Extract the domain for the variable
    if(type=="continue")
      var_domain <- get_unlabel_domain(dataset[[var]])
    else
      var_domain <- get_domain(dataset[[var]])
    
    if(var_domain == "") var_domain <- get_unlabel_domain(dataset[[var]])
    
    # Split the domain string into individual levels
    val_domain <- strsplit(var_domain, "/")[[1]]
    # Create a factor with the specified levels and compute the table
    occ <- table(factor(dataset[[var]], levels = val_domain))
    # Store the table in the list with the variable name as the key
    var_occ[[var]] <- occ
  }
  
  return(var_occ)
}

# Function to calculate basic statistics, variance, and standard deviation for each sequential variablee
# summary_stats <- calculate_statistics(seq_domain, combined_dataset)
calculate_statistics <- function(variables_df, data_df) {
  variables_df %>%
    pull(variable) %>%
    map_df(~{
      selected_col <- data_df[[.x]]
      
      # Check for empty column or columns with only NA values
      if (all(is.na(selected_col))) {
        return(tibble(
          Variable = .x,
          Min = NA,
          `1st Qu.` = NA,
          Median = NA,
          Mean = NA,
          `3rd Qu.` = NA,
          Max = NA,
          Variance = NA,
          St_Dev = NA
        ))
      }
      
      # Calculate basic statistics using the summary function
      base_stats <- summary(selected_col)
      
      # Calculate variance and standard deviation, excluding NA values
      variance <- var(selected_col, na.rm = TRUE)
      st_dev <- sd(selected_col, na.rm = TRUE)
      
      # Create a tibble with all calculated statistics
      tibble(
        Variable = .x,
        Min = base_stats[1],
        `1st Qu.` = base_stats[2],
        Median = base_stats[3],
        Mean = base_stats[4],
        `3rd Qu.` = base_stats[5],
        Max = base_stats[6],
        Variance = variance,
        St_Dev = st_dev
      )
    })
}

var_na_perc <- function(dataset){
  na_perc <- round(colSums(is.na(dataset)) / nrow(dataset) * 100, 2)
  
  wo_na <- tibble(
    perc = character(),
    remains = integer()
  )
  
  for (i in seq(from = 5, to = 95, by = 5)){
    var_na <- sum(na_perc >= i)
    wo_na <- wo_na %>%
      add_row(
        perc = paste(i, "%", sep=""),
        remains = (length(names(dataset)) - var_na)
      )
  }
  
  return(wo_na)
}

get_var_type <- function(
    dataset, 
    domain_mapping,
    var_filter = c("mergeid", "wave", "hhid6", "mergeidp6", "coupleid6", "hhid5", "mergeidp5", "coupleid5", "hhid7", "mergeidp7", "coupleid7", "hhsize", "hh017e"),
    unlabel_filter = c("ac012_", "ch001_", "ac012_"),
    var_euro = c("euro1", "euro2", "euro3", "euro4", "euro5", "euro6", "euro7", "euro8", "euro9", "euro10", "euro11", "euro12"),
    continue_filter = c("-9/-2/-1", "-2/-1", "-3/-2/-1", "-9999992/-9999991"),
    not_ord_filter = c("country", "language", "more2y_c", "ch001_", "ch021_", "ph054_", "ph066_", "ph071_1", "ph071_2", "ph071_3", "ph071_4", "ph072_3", "ph076_1", "ph076_2", "ph076_3", "ph076_4", "ph077_1",  "ph077_2", "ph077_3", "ph077_4", "hc029_", "ph075_1", "ph075_3", "ep005", "isced1997_r")
){
  
  var_continue <- domain_mapping %>%
    filter(domain %in% continue_filter) %>%
    pull(variable)
  var_continue <- setdiff(var_continue, c(var_filter, unlabel_filter))
  
  var_categ <- setdiff(names(dataset), c(var_continue, var_filter, var_euro))
  ds_categ <- dataset %>% select(all_of(var_categ))
  
  categ_domains <- sapply(ds_categ, get_unlabel_domain)
  
  domain_mapping <- tibble(
    variable = names(categ_domains),
    domain = categ_domains
  )%>%
    mutate(domain_size = sapply(strsplit(domain, "/"), length))
  
  var_ord <- filter(domain_mapping, domain_size > 2)  %>%
    pull(variable)
  var_ord <- setdiff(var_ord, not_ord_filter)
  
  var_not_ord <- setdiff(names(ds_categ), var_ord)
  
  return(list(
    var_continue = var_continue, 
    var_euro = var_euro, 
    var_ord = var_ord, 
    var_not_ord = var_not_ord
  ))
}

ipo_test <- function(dataset, pvar, type_test){
  
  res <- data.frame(variable = character(), value = numeric())
  
  if(type_test == "chi2"){
    for (var in names(dataset)){
      if(var != pvar){
        t <- table(dataset[[var]], dataset[[pvar]])
        if (all(t >= 5)) {
          tchi2 <- chisq.test(t)
          res <- rbind(res, data.frame(variable = var, pvalue = tchi2$p.value))
        }
      }
    }
  }else if(type_test == "mann_whitney"){
    for (var in names(dataset)) {
      if (var != pvar && is.numeric(dataset[[var]])) {  
        group1 <- dataset[[var]][dataset[[pvar]] == "yes"]
        group2 <- dataset[[var]][dataset[[pvar]] == "no"]
        if (length(group1) > 0 && length(group2) > 0) {
          test_mw <- wilcox.test(group1, group2, exact = FALSE)
          res <- rbind(res, data.frame(variable = var, pvalue = test_mw$p.value))
        }
      }
    }
  }
  
  return(res)
}
