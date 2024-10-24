############################################################################
# FUNCTIONS TO PLOT ANALYSIS ###############################################
############################################################################


############################################################################
# make_docx_table ##########################################################
############################################################################
#' Generate a Word Table with Variable Information
#'
#' This function creates a Word document containing a table with selected variables 
#' from a dataset, showing their labels and the percentage of missing values.
#'
#' @param dataset A data frame containing the data.
#' @param variables A character vector of variable names from the dataset.
#' @param path The file path where the Word document will be saved.
#'
#' @return None. The function generates a .docx file at the specified path.
#'
#' @examples
#' # Assuming 'data' is your dataset and 'vars' is the list of variables:
#' make_docx_table(data, vars, "output.docx")
#'
make_docx_table <- function(dataset, variables, path) { 
  # Retrieve labels for each variable in the dataset
  var_labels <- sapply(variables, function(var_name) get_label_by_variable(dataset, var_name))
  
  # Calculate the percentage of missing or negative values for the dataset
  neg_perc <- negative_perc(dataset)
  perc_minus1 <- neg_perc[[1]]
  
  # Create a data frame with variable names, labels, and percentage of missing values
  var_ds <- data.frame(
    variable = names(var_labels),       # Variable names
    label = var_labels,                 # Corresponding labels
    perc_missing = paste(perc_minus1, "%", sep="")  # Percentage of missing values
  ) %>% arrange(Na)   # Sort the data frame based on the 'Na' column (if present)
  
  # Convert the data frame to a flextable object for formatting
  ft <- flextable(var_ds)
  
  # Create a new Word document
  doc <- read_docx()
  
  # Add the flextable to the Word document
  doc <- body_add_flextable(doc, ft)
  
  # Save the Word document to the specified path
  print(doc, target = path)
}

############################################################################
# make_alluvial_plot #######################################################
############################################################################
#' Generate Alluvial Plots and Export to Word Document
#'
#' This function creates alluvial plots for different waves of data, showing 
#' changes in depression status from baseline (BL) to follow-up (FU), and 
#' exports them to a Word document.
#'
#' @param dataset A data frame containing the data. It must include variables 
#' 'wave', 'euro_d', and 'initial_euro_d'.
#' @param output_file A character string specifying the path where the Word document
#' will be saved. Default is "results/euro_alluvial_plot.docx".
#'
#' @return None. The function generates a .docx file with alluvial plots for each wave.
#'
#' @examples
#' # Assuming 'data' is your dataset:
#' make_alluvial_plot(data, "output.docx")
#'
make_alluvial_plot <- function(dataset, output_file = "results/euro_alluvial_plot.docx") {
  
  # Convert variables to factors
  dataset$wave <- as.factor(dataset$wave)
  dataset$euro_d <- as.factor(dataset$euro_d)
  dataset$initial_euro_d <- as.factor(dataset$initial_euro_d)
  
  # Filter dataset for each wave
  w5 <- dataset %>% filter(wave == 5)
  w6 <- dataset %>% filter(wave == 6)
  w7 <- dataset %>% filter(wave == 7)
  
  # Function to create alluvial plots with subtitle
  create_alluvial_plot <- function(data_wave, wave_number) {
    ggplot(data = data_wave, aes(axis1 = initial_euro_d, axis2 = euro_d)) +
      geom_alluvium(aes(fill = euro_d)) + 
      geom_stratum() +
      geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
      theme_minimal() +
      scale_x_discrete(limits = c("DEPRESSION BL", "DEPRESSION FU"), expand = c(.05, .05)) +
      labs(
        y = "", x = "",
        title = paste("Alluvial Plot BL vs FU Depression in wave", wave_number),
        subtitle = paste("To analyze changes in depression status from baseline to follow-up within Wave", wave_number)
      )
  }
  
  # Create plots for each wave
  alluvial_plot3 <- create_alluvial_plot(w5, 5)
  alluvial_plot4 <- create_alluvial_plot(w6, 6)
  alluvial_plot5 <- create_alluvial_plot(w7, 7)
  
  # Create a Word document
  doc <- read_docx()
  
  # Add the plots to the document
  doc <- doc %>%
    body_add_gg(value = alluvial_plot3, width = 6.2, height = 4) %>%
    body_add_par("") %>%
    body_add_gg(value = alluvial_plot4, width = 6.2, height = 4) %>%
    body_add_par("") %>%
    body_add_gg(value = alluvial_plot5, width = 6.2, height = 4)
  
  # Save the document
  print(doc, target = output_file)
}

############################################################################
# make_docx_variable_list ##################################################
############################################################################
#' Generate a Word Document Containing a Variable List
#'
#' This function generates a table in a Word document with a list of variables 
#' from a dataset, along with their types, domains, and percentage of missing or 
#' negative values. It also includes label descriptions for each variable.
#'
#' @param dataset A data frame containing the data.
#' @param domain_mapping A data frame or tibble mapping variables to their domains.
#' @param var_continue A vector of variable names that are considered continuous.
#' @param var_ord A vector of variable names that are considered ordinal.
#' @param path The file path where the Word document will be saved.
#'
#' @return None. The function generates a .docx file at the specified path.
#'
#' @examples
#' # Assuming 'data' is your dataset and 'domain_map' is the domain mapping:
#' make_docx_variable_list(data, domain_map, continuous_vars, ordinal_vars, "output.docx")
#'
make_docx_variable_list <- function(dataset, domain_mapping, var_continue, var_ord, path) {
  # Calculate the percentage of missing or negative values for each variable in the dataset
  neg_perc <- negative_perc(dataset)
  perc_minus1 <- neg_perc[[1]]
  
  # Create an empty tibble to store the variable information
  data <- tibble(
    label_variabile = character(),     # Variable label
    type = character(),                # Variable type (numeric, orderable, etc.)
    domain = character(),              # Domain associated with the variable
    labels_variabile = character(),    # Labels associated with the variable
    perc_minus1 = character()          # Percentage of missing/negative values
  )
  
  # Loop through each variable in the dataset
  for (var_name in names(dataset)) {
    print(var_name)  # Print the variable name for debugging purposes
    
    # Retrieve the label and the possible values (labels) of the variable
    label_var <- get_label_by_variable(dataset, var_name)
    labels_var <- get_labels_by_variable(dataset, var_name)
    
    # Convert the labels into a single string, separated by commas
    if(length(labels_var) > 1) {
      labels_var <- paste(labels_var$label, collapse = ", ")
    } else {
      labels_var <- as.character(labels_var$label)
    }
    
    # Convert the label to character format for uniformity
    label_var <- as.character(label_var)
    
    # Find the domain corresponding to the variable from the domain_mapping
    domain <- domain_mapping$domain[domain_mapping$variable == var_name]
    
    # Determine the type of variable (numeric, orderable, not orderable)
    if (var_name %in% var_continue) {
      type = "numeric with -1 categorial"
    } else if (var_name %in% var_ord) {
      type = "orderable"
    } else {
      type = "not orderable"
    }
    
    # Append the variable information to the tibble
    data <- bind_rows(data, tibble(
      label_variabile = label_var,
      type = type,
      domain = domain,
      labels_variabile = labels_var,
      perc_minus1 = paste(perc_minus1[[var_name]], "%", sep = "")
    ))
  }
  
  # Create a flextable object from the tibble for better formatting
  ft <- flextable(data)
  
  # Create a new Word document
  doc <- read_docx()
  
  # Add the flextable to the Word document
  doc <- body_add_flextable(doc, ft)
  
  # Save the Word document to the specified file path
  print(doc, target = path)
}


############################################################################
# make_wave_flag_table #####################################################
############################################################################
#' Generate a Summary Table for a Specific Wave
#'
#' This function creates a summary table for a given wave of data, displaying 
#' the occurrence count, average age, and gender distribution (male and female)
#' grouped by the 'flag_cfu' (Cancer FU) variable. The table is formatted using 
#' the `flextable` package and includes headers, borders, and aligned columns.
#'
#' @param dataset A data frame containing the data.
#' @param wave The wave number to filter the data.
#'
#' @return A formatted flextable object displaying the summary statistics.
#'
#' @examples
#' # Assuming 'data' is your dataset:
#' make_wave_flag_table(data, 5)
#'
make_wave_flag_table <- function(dataset, wave) {
  
  # Filter the dataset for the specified wave and group by 'flag_cfu'
  table <- dataset %>%
    filter(wave == !!wave) %>%                 # Filter data for the given wave
    group_by(flag_cfu) %>%                     # Group by 'flag_cfu' (Cancer FU)
    summarize(
      occ = n(),                               # Count the number of occurrences per group
      age = round(mean(age_int, na.rm = TRUE), 2),  # Calculate mean age and round to 2 decimal places
      male = sum(dn042_ == 0, na.rm = TRUE),   # Count the number of males (dn042_ == 0)
      female = sum(dn042_ == 1, na.rm = TRUE)  # Count the number of females (dn042_ == 1)
    ) %>%
    ungroup() %>%                              # Ungroup the data
    flextable() %>%                            # Convert to flextable for formatting
    set_header_labels(
      flag_cfu = "Cancer FU",                  # Set header label for 'flag_cfu'
      occ = "N",                               # Set header label for occurrences
      age = "Age",                             # Set header label for mean age
      male = "M",                              # Set header label for male count
      female = "F"                             # Set header label for female count
    ) %>%
    add_header_row(                            # Add a header row with the wave information
      values = paste("Wave", wave, "Table"),   # Dynamic header with wave number
      colwidths = 5                            # Span the header across 5 columns
    ) %>%
    align(align = "center", part = "header") %>%  # Center align the header text
    colformat_double(j = "age", digits = 2) %>%   # Format the 'age' column to display 2 decimal places
    border_outer(part = "all", border = fp_border(color = "black", width = 1, style = "solid")) %>%
    border_inner_h(part = "all", border = fp_border(color = "black", width = 1, style = "solid")) %>%
    border_inner_v(part = "all", border = fp_border(color = "black", width = 1, style = "solid"))  # Add borders around the table
  
  return(table)  # Return the formatted flextable
}

############################################################################
# make_wave_condition_table ################################################
############################################################################
#' Generate a Table Summarizing Conditions by Wave
#'
#' This function creates a summary table for a specified wave of data. The table 
#' contains counts of individuals in each condition group (condition_fu), along with
#' their average age and gender distribution (male and female).
#'
#' @param dataset A data frame containing the data. It must include the variables 
#' 'wave', 'condition_fu', 'age_int', and 'dn042_'.
#' @param wave A specific wave (e.g., 5, 6, or 7) to filter the data.
#'
#' @return A flextable object summarizing the condition, number of occurrences, 
#' average age, and the male/female count for the given wave.
#'
#' @examples
#' # Assuming 'data' is your dataset:
#' make_wave_condition_table(data, 5)
#'
make_wave_condition_table <- function(dataset, wave) {
  
  # Filter dataset for the specified wave and group by condition_fu
  table <- dataset %>%
    filter(wave == !!wave) %>%    # Filter for the selected wave
    group_by(condition_fu) %>%    # Group by condition
    summarize( 
      occ = n(),                            # Count occurrences in each condition
      age = round(mean(age_int, na.rm = TRUE), 2),  # Calculate mean age, removing NAs
      male = sum(dn042_ == 0, na.rm = TRUE),        # Count males (assuming dn042_ == 0 for males)
      female = sum(dn042_ == 1, na.rm = TRUE)       # Count females (assuming dn042_ == 1 for females)
    ) %>%
    ungroup() %>%
    
    # Create a flextable from the summary data
    flextable() %>%
    
    # Set custom column labels for the table
    set_header_labels(
      condition_fu = "Condition FU",  # Rename condition column
      occ = "N",                      # Rename count column
      age = "Age",                    # Rename age column
      male = "M",                     # Rename male count column
      female = "F"                    # Rename female count column
    ) %>%
    
    # Add a header row with the wave number
    add_header_row(
      values = paste("Wave", wave, "Table"),  # Title for the table header
      colwidths = 5                           # Span across 5 columns
    ) %>%
    
    # Center-align the header content
    align(align = "center", part = "header") %>%
    
    # Format the 'age' column to show only two decimal places
    colformat_double(j = "age", digits = 2) %>%
    
    # Add borders to the table
    border_outer(part = "all", border = fp_border(color = "black", width = 1, style = "solid")) %>%
    border_inner_h(part = "all", border = fp_border(color = "black", width = 1, style = "solid")) %>%
    border_inner_v(part = "all", border = fp_border(color = "black", width = 1, style = "solid"))
  
  return(table)  # Return the formatted flextable object
}

############################################################################
# make_wave_euro_table #####################################################
############################################################################
#' Generate a Summary Table of Euro Depression Status by Wave
#'
#' This function creates a summary table that displays the change in depression 
#' status from baseline (BL) to follow-up (FU) across a specific wave. It also 
#' calculates the count, average age, and gender distribution (male and female) 
#' for each depression status combination.
#'
#' @param dataset A data frame containing the data. It must include the variables 
#' 'wave', 'initial_euro_d', 'euro_d', 'age_int', and 'dn042_'.
#' @param wave The specific wave (e.g., 5, 6, or 7) to filter the data.
#'
#' @return A flextable object summarizing the baseline to follow-up depression 
#' combinations, number of occurrences, average age, and male/female count for the given wave.
#'
#' @examples
#' # Assuming 'data' is your dataset:
#' make_wave_euro_table(data, 5)
#'
make_wave_euro_table <- function(dataset, wave) {
  
  # Filter the dataset for the specified wave
  table <- dataset %>%
    filter(wave == !!wave) %>%  # Filter the dataset for the selected wave
    
    # Create a new column combining initial and follow-up depression status
    mutate(euro_combination = paste(initial_euro_d, euro_d, sep = "-")) %>%
    
    # Group the data by the combined depression status
    group_by(euro_combination) %>%
    
    # Summarize the data: calculate occurrences, average age, and gender counts
    summarize(
      occ = n(),  # Count the number of occurrences in each combination
      age = round(mean(age_int, na.rm = TRUE), 2),  # Calculate the mean age, rounded to 2 decimals
      male = sum(dn042_ == 0, na.rm = TRUE),  # Count the number of males
      female = sum(dn042_ == 1, na.rm = TRUE)  # Count the number of females
    ) %>%
    ungroup() %>%  # Remove the groupings
    
    # Convert the summarized data into a flextable for easy display
    flextable() %>%
    
    # Set custom column headers for the table
    set_header_labels(
      euro_combination = "BL->FU",  # Rename the combined depression status column
      occ = "N",                    # Rename the occurrences column
      age = "Age",                  # Rename the age column
      male = "M",                   # Rename the male count column
      female = "F"                  # Rename the female count column
    ) %>%
    
    # Add a header row with the wave number
    add_header_row(
      values = paste("Wave", wave),  # Title for the table header
      colwidths = 5                  # Span across 5 columns
    ) %>%
    
    # Center-align the content in the header row
    align(align = "center", part = "header") %>%
    
    # Format the 'age' column to display numbers with 2 decimal places
    colformat_double(j = "age", digits = 2) %>%
    
    # Add borders to the table
    border_outer(part = "all", border = fp_border(color = "black", width = 1, style = "solid")) %>%
    border_inner_h(part = "all", border = fp_border(color = "black", width = 1, style = "solid")) %>%
    border_inner_v(part = "all", border = fp_border(color = "black", width = 1, style = "solid"))
  
  # Return the formatted table
  return(table)
}


############################################################################
# make_wave_table_docx #####################################################
############################################################################
#' Generate a Word Document with Tables for Multiple Waves
#'
#' This function creates a Word document containing tables summarizing different aspects 
#' (e.g., euro depression status, flag status, or condition status) for each wave in 
#' the dataset. The user can specify which type of table to generate.
#'
#' @param dataset A data frame containing the data. It must include the variable 'wave'.
#' @param euro A logical value indicating whether to generate tables for euro depression status.
#' @param flag A logical value indicating whether to generate tables for flag status.
#' @param condition A logical value indicating whether to generate tables for condition status.
#' @param output_file A character string specifying the file path where the Word document
#' will be saved.
#'
#' @return None. The function generates a .docx file at the specified path.
#'
#' @examples
#' # Assuming 'data' is your dataset:
#' make_wave_table_docx(data, euro = TRUE, output_file = "euro_tables.docx")
#'
make_wave_table_docx <- function(dataset, euro = FALSE, flag = FALSE, condition = FALSE, output_file = "") {
  
  # Get the unique waves present in the dataset
  waves <- unique(dataset$wave)
  
  # Create a new Word document
  doc <- read_docx()
  
  # Loop through each wave and create the corresponding table
  for (w in waves) {
    
    # Determine which type of table to create based on the input parameters
    if (euro == TRUE) {
      wave_table <- make_wave_euro_table(dataset, w)  # Create euro depression table
    } else if (flag == TRUE) {
      wave_table <- make_wave_flag_table(dataset, w)  # Create flag status table
    } else if (condition == TRUE) {
      wave_table <- make_wave_condition_table(dataset, w)  # Create condition status table
    }
    
    # Add the table to the Word document
    doc <- body_add_flextable(doc, value = wave_table)
    
    # Add a blank paragraph (newline) between tables for readability
    doc <- body_add_par(doc, value = "\n", style = "Normal")
  } 
  
  # Save the Word document to the specified file path
  print(doc, target = output_file)
}

############################################################################
# make_distribution_pdf #####################################################
############################################################################
#' Generate PDF with Variable Distributions
#'
#' This function generates a PDF containing plots of distributions for a list of variables.
#' Variables can be of different types (continuous, ordinal, not ordinal, or euro-related), 
#' and different types of plots are created depending on the variable type. The layout of the plots 
#' is customizable, and multiple pages can be created in the PDF.
#'
#' @param dataset A data frame containing the dataset.
#' @param variables A vector of variable names for which to create distribution plots.
#' @param var_type A list containing vectors of variable names categorized by type (e.g., 'var_continue', 'var_ord', etc.).
#' @param pdf_name A string specifying the name of the output PDF file.
#' @param width The width of the PDF pages (default = 20).
#' @param height The height of the PDF pages (default = 17).
#' @param per_page The number of plots to include per page.
#' @param layout A vector specifying the layout of plots on each page (e.g., c(2, 2) for 2x2 grid).
#'
#' @return None. The function saves the generated PDF to the specified path.
#'
#' @examples
#' # Example usage:
#' make_distribution_pdf(dataset, variables = c("var1", "var2"), var_type = var_types, pdf_name = "output.pdf", per_page = 4, layout = c(2, 2))
#'
make_distribution_pdf <- function(dataset, variables, var_type, pdf_name, width = 20, height = 17, per_page, layout) {
  
  # Create a new PDF with the specified width, height, and name
  pdf(paste("plot/", pdf_name, sep=""), width = width, height = height)
  
  # Calculate the number of pages needed based on the number of variables and per_page
  num_pages <- ceiling(length(variables) / per_page)
  
  # Prepare necessary data for plotting
  var_occ <- create_var_occ(dataset)
  var_continue <- var_type$var_continue
  var_euro <- var_type$var_euro
  var_ord <- var_type$var_ord
  var_not_ord <- var_type$var_not_ord
  
  # Loop through the number of pages
  for (page in 1:num_pages) {
    
    plots_page <- list()
    start_index <- (page - 1) * per_page + 1
    end_index <- min(page * per_page, length(variables))
    
    # Loop through each variable to create its corresponding plot
    for (i in start_index:end_index) {
      var_name <- variables[i]
      neg_perc <- negative_perc(dataset)
      perc_minus1 <- neg_perc[[1]]
      
      if (!(var_name %in% var_continue)) {
        df_occ <- as.data.frame(var_occ[[var_name]])
        names(df_occ) <- c("Value", "Count")
        var_labels_df <- get_labels_by_variable(dataset, var_name)
        var_labels_df$label <- substr(var_labels_df$label, 1, 30)  # Shorten labels to 30 characters
      }
      
      # Create plot based on variable type
      if (var_name %in% var_euro) {
        # For euro variables, create a bar plot with value-labels
        df_occ$ValueLabel <- with(var_labels_df, paste(value, "-", label))
        df_occ$ValueLabel <- factor(df_occ$ValueLabel, levels = df_occ$ValueLabel)
        
        p <- ggplot(df_occ, aes(x = ValueLabel, y = Count)) +
          geom_bar(stat = "identity", fill = "skyblue", color = "black", alpha = 0.5) +
          labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n -1:", perc_minus1[[var_name]], "%"), y = "") +
          theme_minimal() +
          theme(legend.position = "none")
        
      } else if (var_name %in% var_ord) {
        # For ordinal variables, create a bar plot by euro status
        data_for_plot <- dataset %>%
          filter(!is.na(.data[[var_name]]), !is.na(euro_d)) %>%
          mutate(var_name = factor(.data[[var_name]]), euro_d = factor(euro_d))
        
        p <- ggplot(data_for_plot, aes(x = var_name, fill = euro_d)) +
          geom_bar(color = "black", alpha = 0.5) +
          scale_x_discrete(breaks = var_labels_df$value, labels = var_labels_df$label) +
          scale_fill_manual(values = c("yes" = "skyblue", "no" = "orange")) +
          theme_minimal() +
          labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n -1:", perc_minus1[[var_name]], "%"), y = "") +
          theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else if (var_name %in% var_not_ord) {
        # For non-ordinal variables, create a pie chart
        if (nrow(var_labels_df) > 0) {
          var_labels_df$legend_label <- paste(df_occ$Value, "->", var_labels_df$label)
          df_occ$ValueLabel <- with(var_labels_df, paste(value, "->", label))
          df_occ$ValueLabel <- factor(df_occ$ValueLabel, levels = df_occ$ValueLabel)
        } else {
          df_occ$ValueLabel <- as.factor(df_occ$Value)
        }
        
        colors <- colorRampPalette(RColorBrewer::brewer.pal(min(9, length(unique(df_occ$ValueLabel))), "Set1"))(length(unique(df_occ$ValueLabel)))
        
        p <- ggplot(df_occ, aes(x = "", y = Count, fill = ValueLabel)) +
          geom_bar(stat = "identity", color = "black") +
          coord_polar("y", start = 0) +
          theme_void() +
          scale_fill_manual(values = colors, labels = var_labels_df$legend_label, name = var_name) +
          labs(caption = paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n -1:", perc_minus1[[var_name]], "%"))
        
      } else if (var_name %in% var_continue) {
        # For continuous variables, create a density plot
        p <- ggplot(dataset, aes(x = !!sym(var_name))) +
          geom_density(aes(y = after_stat(count)), fill = "skyblue", alpha = 0.5) +
          labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n -1:", perc_minus1[[var_name]], "%"), y = "") +
          theme_minimal()
      }
      
      # Store the plot in the list
      plots_page[[i - start_index + 1]] <- p
    }
    
    # Arrange the plots on the page with the specified layout
    do.call(grid.arrange, c(plots_page, ncol = layout[1], nrow = layout[2], padding = unit(0.5, "lines")))
  }
  
  # Close the PDF device
  dev.off()
}


############################################################################
# pre_analysis_plot ########################################################
############################################################################
#' Generate Pre-Analysis Plots for Different Variable Types
#'
#' This function generates distribution plots for various types of variables in a dataset 
#' (continuous, euro, ordinal, non-ordinal). It creates separate PDFs for each type of variable, 
#' and organizes the plots within each PDF according to a specified layout.
#'
#' @param dataset A data frame containing the dataset.
#' @param var_type A list containing vectors of variable names categorized by type 
#' (e.g., 'var_continue', 'var_euro', 'var_ord', 'var_not_ord').
#' @param path A string specifying the directory where the generated PDF files will be saved.
#'
#' @return None. The function saves the generated PDFs to the specified directory.
#'
#' @examples
#' # Example usage:
#' pre_analysis_plot(dataset = data, var_type = var_types, path = "output/")
#'
pre_analysis_plot <- function(
    dataset, 
    var_type,
    path = ""
){
  
  var_continue <- var_type$var_continue
  var_euro <- var_type$var_euro
  var_ord <- var_type$var_ord
  var_not_ord <- var_type$var_not_ord
  
  # Generate distribution plots for continuous variables
  if(length(var_continue) > 0 ){
    make_distribution_pdf(dataset = dataset,
                          variables = var_continue,
                          var_type = var_type,
                          pdf_name = paste(path, "continue_distribution.pdf", sep=""), 
                          width = 20, 
                          height = 17, 
                          per_page = 12, 
                          layout = c(3, 4))
  }
  
  # Generate distribution plots for euro variables
  if(length(var_euro) > 0 ){
    make_distribution_pdf(dataset = dataset,
                          variables = var_euro,
                          var_type = var_type,
                          pdf_name = paste(path, "euro_distribution.pdf", sep=""),
                          width = 20, 
                          height = 17, 
                          per_page = 12, 
                          layout = c(3, 4))
  }
  
  # Generate distribution plots for ordinal variables
  if(length(var_ord) > 0 ){
    make_distribution_pdf(dataset = dataset, 
                          variables = var_ord,
                          var_type = var_type,
                          pdf_name = paste(path, "fill_ord_distribution.pdf", sep=""), 
                          width = 25, 
                          height = 22, 
                          per_page = 12,  
                          layout = c(3, 4))
  }
  
  # Generate distribution plots for non-ordinal variables
  if(length(var_not_ord) > 0 ){
    make_distribution_pdf(dataset = dataset,
                          variables = var_not_ord,
                          var_type = var_type,
                          pdf_name = paste(path, "not_ord_distribution.pdf", sep=""),
                          width = 25, 
                          height = 22, 
                          per_page = 12, 
                          layout = c(3, 4))
  }
}


############################################################################
# make_variancy_barplot_docx################################################
############################################################################
#' Generate Variance Barplots and Export to Word Document
#'
#' This function creates variance barplots for different types of variables 
#' (continuous, ordinal, not ordinal, and euro variables) and saves them to a Word document.
#' It computes the variance of each variable in the dataset, creates barplots, and adds them to the document.
#'
#' @param dataset A data frame containing the dataset.
#' @param var_continue A vector of names of continuous variables.
#' @param var_ord A vector of names of ordinal variables.
#' @param var_not_ord A vector of names of non-ordinal variables.
#' @param var_euro A vector of names of euro-related variables.
#' @param path A string specifying the file path where the Word document will be saved.
#' @param width The width of each plot in the document (default = 6).
#' @param height The height of each plot in the document (default = 1.7).
#'
#' @return None. The function generates a .docx file at the specified path.
#'
#' @examples
#' # Example usage:
#' make_variancy_barplot_docx(dataset = data, var_continue = cont_vars, var_ord = ord_vars, var_not_ord = not_ord_vars, var_euro = euro_vars, path = "output.docx")
#'
make_variancy_barplot_docx <- function(
    dataset, 
    var_continue,
    var_ord,
    var_not_ord,
    var_euro,
    path, 
    width = 7, 
    height = 5
){
  
  # Continuous variables: Compute variance and create barplot
  ds <- dataset %>% select(all_of(var_continue))
  variancy_ds <- sapply(ds, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA)
  var_names <- c()
  
  # Loop through each variable name in variancy_ds
  for (var in names(variancy_ds)) {
    # Call the function get_label_by_variable and store the result in var_name
    var_name <- get_label_by_variable(dataset, var)
    
    # Append the result to the vector
    var_names <- c(var_names, var_name)
  }
  
  variancy_ds <- data.frame(
    variable = var_names,
    value = variancy_ds
  ) %>% na.omit(variancy_ds)
  
  # Sort variables by variance in descending order
  variancy_ds$variable <- factor(variancy_ds$variable, levels = variancy_ds$variable)
  var_names_replace <- c("Age", "Children", "Grandchildren", "Teeth missing", "Weight loss (kg)", "BMI")
  variancy_ds$variable <- var_names_replace[variancy_ds$variable]
  variancy_ds$variable <- factor(variancy_ds$variable, levels = variancy_ds$variable[order(variancy_ds$value, decreasing = TRUE)])
  # Create barplot for continuous variables
  p1 <- ggplot(variancy_ds, aes(x = variable, y = value)) +
    geom_bar(stat = "identity", fill = "blue") +  # Set bars to blue
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "Variances")
  
  # Ordinal variables: Compute variance and create barplot
  ds <- dataset %>% select(all_of(var_ord))
  variancy_ds <- sapply(ds, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA)
  
  var_names <- c()
  
  # Loop through each variable name in variancy_ds
  for (var in names(variancy_ds)) {
    # Call the function get_label_by_variable and store the result in var_name
    var_name <- get_label_by_variable(dataset, var)
    
    # Append the result to the vector
    var_names <- c(var_names, var_name)
  }
  
  variancy_ds <- data.frame(
    variable = var_names,
    value = variancy_ds
  ) %>% na.omit(variancy_ds)
  

  var_names_replace <- c(
    "Born citizen",
    "Part of area",
    "Help in trouble",
    "Make ends meet",
    "Life satisfaction",
    "Age limits",
    "Out of control",
    "Feel left out",
    "Do what you want",
    "Family prevents",
    "Money shortage",
    "Look forward",
    "Life has meaning",
    "Happy looking back",
    "Full of energy",
    "Many opportunities",
    "Good future",
    "Computer skills",
    "BMI categories",
    "Health question 2",
    "Limited by health",
    "Distance eyesight",
    "Use hearing aid",
    "Hearing",
    "Pain level",
    "Replaced teeth",
    "Feels lonely",
    "Health insurance satisfaction",
    "Vigorous activities",
    "Moderate activities",
    "Baseline depression",
    "2+ years with cancer",
    "Condition duration",
    "Cancer duration"
  )
  variancy_ds$variable <- factor(variancy_ds$variable, levels = variancy_ds$variable)
  variancy_ds$variable <- var_names_replace[variancy_ds$variable]
  variancy_ds$variable <- factor(variancy_ds$variable, levels = variancy_ds$variable[order(variancy_ds$value, decreasing = TRUE)])
  # Create barplot for ordinal variables
  p2 <- ggplot(variancy_ds, aes(x = variable, y = value)) +
    geom_bar(stat = "identity", fill = "blue") +  # Set bars to blue
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "Variances") +
    theme(plot.margin = unit(c(1, 1, 1, 2), "cm"))  # Add more space to the left
  
  # Non-ordinal variables: Compute variance and create barplot
  ds <- dataset %>% select(all_of(var_not_ord))
  variancy_ds <- sapply(ds, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA)
  var_names <- c()
  
  # Loop through each variable name in variancy_ds
  for (var in names(variancy_ds)) {
    # Call the function get_label_by_variable and store the result in var_name
    var_name <- get_label_by_variable(dataset, var)
    
    # Append the result to the vector
    var_names <- c(var_names, var_name)
  }

  
  variancy_ds <- data.frame(
    variable = var_names,
    value = variancy_ds
  ) %>% na.omit(variancy_ds)
  
  # Sort variables by variance in descending order

  var_names_replace <- c(
    "Gender",
    "Marital status",
    "Ever had any siblings",
    "Building area",
    "Received external help",
    "Help given (12 months)",
    "Job situation",
    "Physical inactivity",
    "Country ID",
    "Long-term illness",
    "Wears glasses/lenses",
    "Eyesight reading" ,
    "Health limits work",
    "Lost weight check",
    "Reason lost weight",
    "Pain troubled",
    "Joint pain",
    "Bifocal/progressive lenses",
    "Natural teeth",
    "Hospital stay (12 months)",
    "No doctor due to cost",
    "No doctor due to wait",
    "Education level (ISCED-97)",
    "Depression",
    "Cancer in follow-up?",
    "Help activities" ,
    "5+ drugs/day",
    "Hearing aid",
    "Activities (last year)",
    "Condition",
    "Drugs for",
    "Difficulties BADL",
    "Difficulties IADL",
    "Frailty",
    "Use of aids",
    "Cancer in",
    "Pain location",
    "Glasses/lenses type"
  )
  variancy_ds$variable <- factor(variancy_ds$variable, levels = variancy_ds$variable)
  variancy_ds$variable <- var_names_replace[variancy_ds$variable]
  
  variancy_ds$variable <- factor(variancy_ds$variable, levels = variancy_ds$variable[order(variancy_ds$value, decreasing = TRUE)])
  # Create barplot for non-ordinal variables
  p3 <- ggplot(variancy_ds, aes(x = variable, y = value)) +
    geom_bar(stat = "identity", fill = "blue") +  # Set bars to blue
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "Variances")
  # Euro variables: Compute variance and create barplot
  ds <- dataset %>% select(all_of(var_euro))
  variancy_ds <- sapply(ds, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA)
  var_names <- c()
  
  # Loop through each variable name in variancy_ds
  for (var in names(variancy_ds)) {
    # Call the function get_label_by_variable and store the result in var_name
    var_name <- get_label_by_variable(dataset, var)
    
    # Append the result to the vector
    var_names <- c(var_names, var_name)
  }
  
  variancy_ds <- data.frame(
    variable = var_names,
    value = variancy_ds
  ) %>% na.omit(variancy_ds)
  
  # Sort variables by variance in descending order
  variancy_ds$variable <- factor(variancy_ds$variable, levels = variancy_ds$variable)
   var_names <- gsub("\\s*\\(.*\\)", "", var_names)
  variancy_ds$variable <- var_names_replace[variancy_ds$variable]
  variancy_ds$variable <- factor(variancy_ds$variable, levels = variancy_ds$variable[order(variancy_ds$value, decreasing = TRUE)])
  # Create barplot for euro variables
  p4 <- ggplot(variancy_ds, aes(x = variable, y = value)) +
    geom_bar(stat = "identity", fill = "blue") +  # Set bars to blue
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "", y = "Variances")
  # Create a new Word document
  doc <- read_docx()
  
  # Add the plots to the Word document with headings for each section
  doc <- doc %>%
    body_add_par("Continuous Variables", style = "heading 1") %>%
    body_add_gg(p1, width, height, style = "centered") %>%
    body_add_par("Ordinal Variables", style = "heading 1") %>%
    body_add_gg(p2, width, height, style = "centered") %>%
    body_add_par("Not ordinal Variables", style = "heading 1") %>%
    body_add_gg(p3, width, height, style = "centered") %>%
    body_add_par("Euro Variables", style = "heading 1") %>%
    body_add_gg(p4, width, height, style = "centered")
  
  # Save the Word document to the specified path
  print(doc, target = path)
}


############################################################################
# plot_variable_distribution ###############################################
############################################################################
#' Plot Distribution of a Variable
#'
#' This function generates a plot representing the distribution of a given variable from the dataset.
#' The type of plot depends on whether the variable is continuous, ordinal, non-ordinal, or euro-related.
#'
#' @param dataset A data frame containing the dataset.
#' @param var_name The name of the variable to plot.
#' @param domain_mapping A data frame or tibble mapping variables to their types or domains.
#' @param condition_label A label (optional) used for certain conditions (not used in this version but included for flexibility).
#'
#' @return A ggplot object representing the distribution of the variable.
#'
#' @examples
#' # Example usage:
#' plot_variable_distribution(dataset = data, var_name = "age", domain_mapping = domain_map)
#'
plot_variable_distribution <- function(dataset, var_name, domain_mapping, condition_label) {
  
  # Get variable types based on the domain mapping
  var_type <- get_var_type(dataset, domain_mapping)
  var_continue <- var_type$var_continue
  var_euro <- var_type$var_euro
  var_ord <- var_type$var_ord
  var_not_ord <- var_type$var_not_ord
  var_occ <- create_var_occ(dataset)
  
  p <- NULL  # Initialize plot variable
  
  # For non-continuous variables, prepare a dataframe of occurrences and labels
  if (!(var_name %in% var_continue)) {
    df_occ <- as.data.frame(var_occ[[var_name]])
    names(df_occ) <- c("Value", "Count")
    var_labels_df <- get_labels_by_variable(dataset, var_name)
    var_labels_df$label <- substr(var_labels_df$label, 1, 30)  # Truncate labels to 30 characters
  }
  
  # Create the plot based on the variable type
  if (var_name %in% var_ord) {
    # Ordinal variable: Create a bar plot
    data_for_plot <- dataset %>% mutate(var_name = factor(.data[[var_name]]))
    
    p <- ggplot(data_for_plot, aes(x = var_name)) +
      geom_bar(fill = "skyblue", color = "black", alpha = 0.5) +
      theme_minimal() +
      theme(axis.title = element_blank(), legend.position = "none")
  }
  else if(var_name %in% var_not_ord){
    # Non-ordinal variable: Create a bar plot
    data_for_plot <- dataset %>% mutate(var_name = factor(.data[[var_name]]))
    
    p <- ggplot(data_for_plot, aes(x = var_name)) +
      geom_bar(fill = "orange", color = "black", alpha = 0.5) +
      theme_minimal() +
      theme(axis.title = element_blank(), legend.position = "none")
  }
  else if(var_name %in% var_euro){
    # Euro-related variable: Create a bar plot
    data_for_plot <- dataset %>% mutate(var_name = factor(.data[[var_name]]))
    
    p <- ggplot(data_for_plot, aes(x = var_name)) +
      geom_bar(fill = "green", color = "black", alpha = 0.5) +
      theme_minimal() +
      theme(axis.title = element_blank(), legend.position = "none")
  }
  else if (var_name %in% var_continue) {
    # Continuous variable: Create a density plot
    p <- ggplot(dataset, aes(x = !!sym(var_name))) +
      geom_density(aes(y = after_stat(count)), fill = "skyblue", alpha = 0.5) +
      theme_minimal() +
      theme(axis.title = element_blank(), legend.position = "none")
  }
  
  # Return the plot
  return(p)
}


############################################################################
# create_descriptive_table_with_plots ######################################
############################################################################
#' Create a Descriptive Table with Plots and Export to Word Document
#'
#' This function generates a descriptive table with plots for a set of variables. The table 
#' includes variable names, labels, and plots depicting different categories (e.g., Alive, Death, Missing).
#' It saves the resulting table as a Word document.
#'
#' @param dataset A data frame containing the dataset.
#' @param domain_mapping A data frame or tibble mapping variables to their domains.
#' @param output_file A string specifying the path where the Word document will be saved.
#' @param dataset_death Optional. A dataset used to generate specific plots for death and missing categories.
#' @param column3 A label for the third column (default = "ALIVE").
#' @param column4 A label for the fourth column (default = "DEATH").
#' @param column5 A label for the fifth column (default = "MISSING").
#'
#' @return None. The function saves a .docx file at the specified path.
#'
#' @examples
#' # Example usage:
#' create_descriptive_table_with_plots(dataset = data, domain_mapping = domain_map, output_file = "output.docx")
#'
create_descriptive_table_with_plots <- function(dataset, domain_mapping, output_file, dataset_death = NULL, column3 = "ALIVE", column4 = "DEATH", column5 = "MISSING") { 
  temp_files <- list()  # List to store temporary files for the plots
  var_names <- setdiff(names(dataset), c("mergeid", "wave", "hhid6", "mergeidp6", "coupleid6", 
                                         "hhid5", "mergeidp5", "coupleid5", "hhid7", "mergeidp7", 
                                         "coupleid7", "hhsize", "language", "ph012_", "ph013_", 
                                         "euro_d", "initial_euro_d", "condition_fu", "hh017e"))  # Exclude specific variables
  
  # Get labels for the variables
  labels_list <- sapply(var_names, function(var_name) {
    var_labels_df <- get_labels_by_variable(dataset, var_name)
    if (nrow(var_labels_df) > 0) {
      paste0(var_labels_df$value, ":", var_labels_df$label, collapse = ",\n")
    } else {
      NA
    }
  }, USE.NAMES = TRUE)
  
  # Get label descriptions for the variables
  label_list <- sapply(var_names, function(var_name) {
    var_label_df <- get_label_by_variable(dataset, var_name)
  }, USE.NAMES = TRUE)
  
  # Create plots for each variable
  for (var_name in var_names) {
    # Create plots for 'Alive', 'Death', and 'Missing' categories
    alive_plot <- if (is.null(dataset_death))
      plot_variable_distribution(dataset, var_name, domain_mapping, column3)
    else 
      plot_variable_distribution(dataset_death %>% filter(condition_fu == 0), var_name, domain_mapping, column3)
    
    death_plot <- if (is.null(dataset_death)) 
      plot_variable_distribution(dataset %>% filter(euro_d == "yes"), var_name, domain_mapping, column4)
    else 
      plot_variable_distribution(dataset_death %>% filter(condition_fu == 1), var_name, domain_mapping, column4)
    
    missing_plot <- if (is.null(dataset_death)) 
      plot_variable_distribution(dataset %>% filter(euro_d == "no"), var_name, domain_mapping, column5)
    else 
      plot_variable_distribution(dataset_death %>% filter(condition_fu == 2), var_name, domain_mapping, column5)
    
    # Save plots as temporary image files
    temp_file_alive <- tempfile(fileext = ".png")
    temp_file_death <- tempfile(fileext = ".png")
    temp_file_missing <- tempfile(fileext = ".png")
    
    ggsave(temp_file_alive, alive_plot, width = 4, height = 1, dpi = 300)
    ggsave(temp_file_death, death_plot, width = 4, height = 1, dpi = 300)
    ggsave(temp_file_missing, missing_plot, width = 4, height = 1, dpi = 300)
    
    temp_files[[var_name]] <- list(alive = temp_file_alive, death = temp_file_death, missing = temp_file_missing)
  }
  
  # Create a flexible table with flextable
  if (is.null(dataset_death))
    ft <- flextable(data.frame(VARIABLE = label_list, LABELS = labels_list, SELECTED = "", DEPRESSED = "", NOTDEPRESSED = ""))
  else
    ft <- flextable(data.frame(VARIABLE = label_list, LABELS = labels_list, ALIVE = "", DEATH = "", MISSING = ""))
  
  # Add the plots to the table
  for (i in seq_along(var_names)) {
    var_name <- var_names[i]
    ft <- compose(ft, i, j = 3, value = as_paragraph(as_image(temp_files[[var_name]]$alive, width = 1, height = 0.25)))
    ft <- compose(ft, i, j = 4, value = as_paragraph(as_image(temp_files[[var_name]]$death, width = 1, height = 0.25)))
    ft <- compose(ft, i, j = 5, value = as_paragraph(as_image(temp_files[[var_name]]$missing, width = 1, height = 0.25)))
  }
  
  # Adjust the column widths
  ft <- width(ft, j = 1, width = 1)
  ft <- width(ft, j = 2, width = 1)
  ft <- width(ft, j = 3:5, width = 1.2)
  
  # Add borders
  ft <- border_outer(ft, part = "all", border = fp_border(color = "black", width = 1))
  ft <- border_inner_h(ft, border = fp_border(color = "black", width = 0.5), part = "all")
  ft <- border_inner_v(ft, border = fp_border(color = "black", width = 0.5), part = "all")
  
  ft <- line_spacing(ft, space = 1.0, part = "body")
  
  # Adjust text size and font
  ft <- fontsize(ft, size = 3, part = "all")
  ft <- font(ft, fontname = "Arial", part = "all")
  
  # Create a new Word document
  doc <- read_docx() %>% body_add_flextable(value = ft)
  
  # Add a legend to the document
  legend_paragraph <- fpar(
    ftext("LEGEND\n", prop = fp_text(bold = TRUE, font.size = 12)),
    ftext("Alive: ..., Death: ..., Missing: ...", prop = fp_text(font.size = 10))
  )
  doc <- body_add_fpar(doc, legend_paragraph, style = "Normal")
  
  # Save the Word document
  print(doc, target = output_file)
  
  # Remove the temporary files
  unlink(unlist(temp_files))
}


############################################################################
# risk_table_docx ##########################################################
############################################################################
#' Create a Risk Table and Export to Word Document
#'
#' This function creates a risk table summarizing the performance of models based on different 
#' thresholds. The table includes metrics such as sensitivity, specificity, positive predictive value (PPV), 
#' and negative predictive value (NPV). The results are exported as a Word document.
#'
#' @param thresholds_results A data frame containing the results for different models and thresholds.
#' @param output_path A string specifying the file path where the Word document will be saved.
#' @param algo A string representing the algorithm name to include in the document.
#'
#' @return None. The function generates a .docx file at the specified path.
#'
#' @examples
#' # Example usage:
#' risk_table_docx(thresholds_results = results_df, output_path = "risk_table.docx", algo = "Algorithm Name")
#'
risk_table_docx <- function(thresholds_results, output_path = "", algo = "") {
  
  # Remove extensions from column names
  colnames(thresholds_results) <- gsub("\\..*", "", colnames(thresholds_results))
  
  # Calculate mean values of the metrics for each Model and Threshold
  summary_stats <- thresholds_results %>% 
    group_by(Model, Threshold) %>%
    summarise(across(c(Population_below_threshold, Sensitivity, Specificity, PPV, NPV, Accuracy), mean))
  
  # Format the calculated metrics as percentages with two decimal places
  summary_stats <- summary_stats %>%
    mutate(
      Threshold = paste0(Threshold, "%"),
      Population_below_threshold = paste0(round(Population_below_threshold, 2), "%"),
      Sensitivity = paste0(round(Sensitivity * 100, 2), "%"),
      Specificity = paste0(round(Specificity * 100, 2), "%"),
      PPV = paste0(round(PPV * 100, 2), "%"),
      NPV = paste0(round(NPV * 100, 2), "%"),
      Accuracy = paste0(round(Accuracy * 100, 2), "%")
    )
  
  # Create a new Word document
  doc <- read_docx()
  
  # Function to add a table to the Word document with a title
  add_table_to_doc <- function(doc, data, title) {
    doc <- doc %>%
      body_add_par(title, style = "heading 1") %>%
      body_add_table(value = data, style = "table_template")
    return(doc)
  }
  
  # Get the list of unique models in the results
  models <- unique(summary_stats$Model)
  
  # Loop through each model, create a table, and add it to the Word document
  for (model in models) {
    model_data <- summary_stats %>% filter(Model == model)
    doc <- add_table_to_doc(doc, model_data, paste(model, algo))
    
    # Add a page break between tables for different models (except the last one)
    if (model != last(models)) {
      doc <- doc %>% body_add_break()
    }
  }
  
  # Save the Word document to the specified output path
  print(doc, target = output_path)
}

calibration_curve_docx <- function(predictions, plot_name = "calibration_curves.docx", table_par = "") {
  
  # Assicurati che i pacchetti necessari siano caricati
  library(dplyr)
  library(ggplot2)
  library(officer)
  library(scales)
  
  # Controlla che 'PredictedProb', 'TrueLabel' e 'Model' siano presenti
  if (!all(c("PredictedProb", "TrueLabel", "Model") %in% names(predictions))) {
    stop("Il dataframe delle previsioni deve contenere le colonne 'PredictedProb', 'TrueLabel' e 'Model'.")
  }
  
  # Converti TrueLabel in fattore se non lo è già
  predictions$TrueLabel <- as.factor(predictions$TrueLabel)
  
  # Estrai i tipi di modello unici
  model_types <- unique(predictions$Model)
  model_types <- c("dt", "rf", "glm")
  # Lista per memorizzare i dati di calibrazione per ogni modello
  calibration_data_list <- list()
  
  # Loop attraverso ogni tipo di modello per calcolare i dati di calibrazione
  for (model_type in model_types) {
    # Filtra le previsioni per il modello corrente
    model_data <- filter(predictions, Model == model_type)
    
    # Assicurati che PredictedProb sia numerico
    model_data$PredictedProb <- as.numeric(model_data$PredictedProb)
    
    # Calcola i dati di calibrazione usando il binning
    bins <- 10  # Numero di bin (puoi modificarlo)
    model_data <- model_data %>%
      mutate(
        bin = ntile(PredictedProb, bins)
      ) %>%
      group_by(bin) %>%
      summarise(
        mean_pred = mean(PredictedProb),
        obs_rate = mean(as.numeric(TrueLabel) - 1)  # Converti 'yes'/'no' in 1/0 se necessario
      )
    
    model_data$Model <- model_type
    calibration_data_list[[model_type]] <- model_data
  }
  
  # Combina i dati di calibrazione per tutti i modelli
  calibration_data <- bind_rows(calibration_data_list)
  
  # Crea il plot della curva di calibrazione
  calibration_plot <- ggplot(calibration_data, aes(x = mean_pred, y = obs_rate, color = Model)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    scale_x_continuous("Probabilità Predetta", limits = c(0, 1), labels = percent) +
    scale_y_continuous("Frequenza Osservata", limits = c(0, 1), labels = percent) +
    labs(title = table_par) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  # Salva il plot come file PNG temporaneo
  temp_plot_file <- tempfile(fileext = ".png")
  ggsave(temp_plot_file, calibration_plot, width = 8, height = 6)
  
  # Crea un nuovo documento Word e aggiungi il plot
  doc <- read_docx() %>%
    body_add_par(table_par, style = "heading 1") %>%
    body_add_img(src = temp_plot_file, width = 6, height = 6, style = "centered")
  
  # Salva il documento Word nel percorso specificato
  print(doc, target = plot_name)
  
  # Elimina il file temporaneo
  unlink(temp_plot_file)
}

calibration_curve_docx <- function(predictions, plot_name = "calibration_curves.docx", table_par = "") {

  predictions$TrueLabel <- as.factor(predictions$TrueLabel)
  
  # Estrai i tipi di modello unici
  model_types <- unique(predictions$Model)
  model_types <- c("dt", "rf", "glm")
  # Lista per memorizzare i dati di calibrazione per ogni modello
  calibration_data_list <- list()
  
  # Loop attraverso ogni tipo di modello per calcolare i dati di calibrazione
  for (model_type in model_types) {
    # Filtra le previsioni per il modello corrente
    model_data <- filter(predictions, Model == model_type)
    
    # Assicurati che PredictedProb sia numerico
    model_data$PredictedProb <- as.numeric(model_data$PredictedProb)
    
    # Calcola i dati di calibrazione usando il binning
    bins <- 10  # Numero di bin (puoi modificarlo)
    model_data <- model_data %>%
      mutate(
        bin = ntile(PredictedProb, bins)
      ) %>%
      group_by(bin) %>%
      summarise(
        mean_pred = mean(PredictedProb),
        obs_rate = mean(as.numeric(TrueLabel) - 1)  # Converti 'yes'/'no' in 1/0 se necessario
      )
    
    model_data$Model <- model_type
    calibration_data_list[[model_type]] <- model_data
  }
  
  # Combina i dati di calibrazione per tutti i modelli
  calibration_data <- bind_rows(calibration_data_list)
  
  # Crea il plot della curva di calibrazione
  calibration_plot <- ggplot(calibration_data, aes(x = mean_pred, y = obs_rate, color = Model)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
    scale_x_continuous("Probabilità Predetta", limits = c(0, 1), labels = percent) +
    scale_y_continuous("Frequenza Osservata", limits = c(0, 1), labels = percent) +
    labs(title = table_par) +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  # Salva il plot come file PNG temporaneo
  temp_plot_file <- tempfile(fileext = ".png")
  ggsave(temp_plot_file, calibration_plot, width = 8, height = 6)
  
  # Crea un nuovo documento Word e aggiungi il plot
  doc <- read_docx() %>%
    body_add_par(table_par, style = "heading 1") %>%
    body_add_img(src = temp_plot_file, width = 6, height = 6, style = "centered")
  
  # Salva il documento Word nel percorso specificato
  print(doc, target = plot_name)
  
  # Elimina il file temporaneo
  unlink(temp_plot_file)
}

############################################################################
# regression_roc_curves_docx ##########################################################
############################################################################
#' Generate ROC Curve Plot and Export to Word Document
#'
#' This function generates a Receiver Operating Characteristic (ROC) curve for multiple models, 
#' calculates the Area Under the Curve (AUC) for each model, and exports both the ROC curve plot 
#' and a table of AUC values to a Word document.
#'
#' @param predictions A data frame containing the predicted values, true labels, and model types.
#' @param plot_name A string specifying the name of the Word document to be saved (default: "roc_curves.docx").
#' @param table_par A string for the paragraph title in the Word document.
#'
#' @return None. The function generates a .docx file containing the ROC curve plot and AUC values.
#'
#' @examples
#' # Example usage:
#' roc_curve_docx(predictions = predictions_df, plot_name = "roc_report.docx", table_par = "ROC Curves for Models")
#'
roc_curve_docx <- function(predictions, plot_name = "roc_curves.docx", table_par = "") {
  
  # Extract the unique model types from the predictions
  model_types <- unique(predictions$Model)
  model_types <- c("dt", "rf", "glm")
  roc_data_list <- list()  # List to store ROC data for each model
  auc_values <- list()     # List to store AUC values for each model
  
  # Loop through each model type to calculate ROC and AUC
  for (model_type in model_types) {
    # Filter the predictions for the current model
    model_data <- filter(predictions, Model == model_type)
    
    # Create ROC object using the true labels and predicted values
    roc_obj <- roc(response = model_data$TrueLabel, predictor = model_data$PredictedProb)
    
    # Create a data frame with False Positive Rate, True Positive Rate, and Thresholds
    roc_data <- data.frame(
      False_Positive_Rate = 1 - roc_obj$specificities,
      True_Positive_Rate = roc_obj$sensitivities,
      Thresholds = roc_obj$thresholds,
      Model = model_type
    )
    
    # Store ROC data and AUC value
    roc_data_list[[model_type]] <- roc_data
    auc_values[[model_type]] <- auc(roc_obj)
  }
  
  # Combine ROC data for all models into a single data frame
  roc_data <- do.call(rbind, roc_data_list)
  
  # Create the ROC curve plot using ggplot2
  roc_plot <- ggplot(roc_data, aes(x = False_Positive_Rate, y = True_Positive_Rate, color = Model)) +
    geom_line(size = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
    labs(title = table_par, x = "False positive rate", y = "True positive rate") +
    theme_minimal() +
    theme(legend.title = element_blank())  # Remove legend title
  
  # Create a data frame to display AUC values for each model
  auc_table <- data.frame(
    Model = names(auc_values),
    AUC = round(unlist(auc_values), 4)
  )
  
  # Save the ROC curve plot as a PNG file
  ggsave("roc_curves.png", roc_plot, width = 8, height = 6)
  
  # Create a new Word document and add the ROC plot and AUC table
  doc <- read_docx() %>%
    body_add_par(table_par, style = "heading 1") %>%
    body_add_img(src = "roc_curves.png", width = 6, height = 6, style = "centered") %>%
    body_add_par("AUC Value", style = "heading 2") %>%
    body_add_table(auc_table, style = "table_template")
  
  # Save the Word document to the specified path
  print(doc, target = plot_name)
}


############################################################################
# calibration_data #########################################################
############################################################################
#' Generate Calibration Data for Calibration Curves
#'
#' This function computes calibration data (predicted vs observed probabilities) for a model by dividing
#' the predictions into bins and calculating the mean observed and predicted probabilities, along with
#' the standard error for each bin.
#'
#' @param pred A numeric vector of predicted probabilities.
#' @param obs A binary vector of observed outcomes (0 or 1).
#' @param bins An integer representing the number of bins to divide the predictions into (default = 10).
#'
#' @return A data frame containing the mean predicted probability, observed probability, and standard error for each bin.
#'
#' @examples
#' # Example usage:
#' calibration_data(pred = predictions, obs = true_labels, bins = 10)
#'
calibration_data <- function(pred, obs, bins = 10) {
  # Calculate the quantiles to bin the predicted probabilities
  quantiles <- unique(quantile(pred, probs = seq(0, 1, 1 / bins)))
  
  # Ensure there are enough bins, reduce if necessary
  while (length(quantiles) <= 1) {
    bins <- bins - 1
    quantiles <- unique(quantile(pred, probs = seq(0, 1, 1 / bins)))
    if (bins == 1) break
  }
  
  # Assign each prediction to a bin
  cuts <- cut(pred, breaks = quantiles, include.lowest = TRUE)
  
  # Calculate mean observed and predicted values for each bin
  obs_mean <- tapply(obs, cuts, mean)
  pred_mean <- tapply(pred, cuts, mean)
  
  # Calculate standard error for observed probabilities
  obs_se <- sqrt(obs_mean * (1 - obs_mean) / tapply(obs, cuts, length))
  
  # Return a data frame containing the predicted, observed probabilities and standard errors
  data.frame(Predicted = pred_mean, Observed = obs_mean, SE = obs_se)
}

############################################################################
# calibration_curves_docx ##################################################
############################################################################
#' Generate Calibration Curves and Export to Word Document
#'
#' This function generates calibration curves for different models, plotting the observed vs. predicted 
#' probabilities for each model, and exports the resulting plot to a Word document.
#'
#' @param predictions A data frame containing the predicted probabilities, true labels, and model types.
#' @param plot_name A string specifying the name of the Word document to be saved (default: "calibration_curves.docx").
#' @param table_par A string for the paragraph title in the Word document.
#'
#' @return None. The function generates a .docx file containing the calibration plot.
#'
#' @examples
#' # Example usage:
#' calibration_curves_docx(predictions = predictions_df, plot_name = "calibration_curves.docx", table_par = "Calibration Curves")
#'
calibration_curves_docx <- function(predictions, plot_name = "calibration_curves.docx", table_par) {
  
  # Extract unique model types from the predictions
  models <- unique(predictions$Model)
  
  # Generate calibration data for each model using the calibration_data function
  calib_list <- lapply(models, function(model) {
    model_data <- filter(predictions, Model == model)
    calibration_data(model_data$PredictedProb, model_data$TrueLabel == "yes")
  })
  
  # Combine calibration data for all models into one data frame
  calib_data <- do.call(rbind, Map(cbind, calib_list, Model = models))
  
  # Create calibration plot using ggplot2
  calibration_plot <- ggplot(calib_data, aes(x = Predicted, y = Observed, color = Model)) +
    geom_point(position = position_dodge(width = 0.05), size = 2) +  # Scatter plot of predicted vs observed probabilities
    geom_errorbar(aes(ymin = Observed - SE, ymax = Observed + SE), width = 0.02, position = position_dodge(width = 0.05)) +  # Add error bars
    geom_smooth(method = "loess", se = FALSE) +  # Loess curve
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +  # 45-degree reference line
    labs(title = table_par, x = "Predicted risk", y = "Observed risk") +  # Labels for the plot
    theme_minimal() +  # Minimal theme for the plot
    theme(legend.title = element_blank())  # Remove legend title
  
  # Create a Word document and add the calibration plot
  doc <- read_docx() %>%
    body_add_par(table_par, style = "heading 1") %>%
    body_add_gg(value = calibration_plot, style = "centered")  # Add the calibration plot to the document
  
  # Save the Word document
  print(doc, target = plot_name)
}


############################################################################
# selected_features_table_docx #############################################
############################################################################
#' Generate a Table of Selected Features for Different Models and Export to Word Document
#'
#' This function creates a table summarizing the selected features (predictors) for different models 
#' (e.g., Decision Tree (DT), Random Forest (RF), and Generalized Linear Model (GLM)), and then exports 
#' the table to a Word document.
#'
#' @param dataset A data frame containing the dataset.
#' @param features A list containing the selected features for each model (e.g., 'rpart' for DT, 'rf' for RF, 'glm' for GLM).
#' @param plot_name A string specifying the name of the Word document to be saved.
#' @param table_par A string for the paragraph title in the Word document.
#'
#' @return None. The function generates a .docx file containing the table of selected features.
#'
#' @examples
#' # Example usage:
#' selected_features_table_docx(dataset = data, features = list(rpart = dt_features, rf = rf_features, glm = glm_features), plot_name = "features.docx", table_par = "Selected Features")
#'
selected_features_table_docx <- function(dataset, features, plot_name = "", table_par = "") {
  
  # Create a data frame with the variable names from the dataset
  results <- data.frame(Variable = colnames(dataset), stringsAsFactors = FALSE)
  
  # Function to check if a variable is present in the selected features list
  check_presence <- function(variable, list) {
    ifelse(variable %in% list, "Present", "Absent")
  }
  
  # Add columns indicating presence of each variable in the lists for DT, RF, and GLM models
  results$DT <- sapply(results$Variable, check_presence, list = features$rpart)
  results$RF <- sapply(results$Variable, check_presence, list = features$rf)
  results$GLM <- sapply(results$Variable, check_presence, list = features$glm)
  
  # Replace variable names with their labels and convert to a character vector
  results$Variable <- sapply(results$Variable, function(var) get_label_by_variable(dataset, var))
  results$Variable <- as.character(results$Variable)
  
  # Count the number of selected features for each model
  dt_count <- length(features$rpart)
  rf_count <- length(features$rf)
  glm_count <- length(features$glm)
  
  # Rename columns to include model name and the count of selected features
  colnames(results) <- c("Predictor", paste0("DT (", dt_count, ")"), 
                         paste0("RF (", rf_count, ")"), paste0("GLM (", glm_count, ")"))
  
  # Create the flextable with specific formatting for each model's selected features
  table <- flextable(results) %>%
    bg(i = ~ DT == "Present", j = 2, bg = "yellow") %>%  # Highlight present features for DT in yellow
    bg(i = ~ RF == "Present", j = 3, bg = "red") %>%  # Highlight present features for RF in red
    bg(i = ~ GLM == "Present", j = 4, bg = "blue") %>%  # Highlight present features for GLM in blue
    hline_top(border = fp_border(color = "black", width = 1)) %>%  # Add top horizontal border
    hline(border = fp_border(color = "black", width = 1)) %>%  # Add horizontal lines between rows
    vline(border = fp_border(color = "black", width = 1)) %>%  # Add vertical lines between columns
    border_outer(border = fp_border(color = "black", width = 1)) %>%  # Add outer border
    padding(padding = 0) %>%  # Remove padding
    autofit() %>%  # Auto-fit the table to the content
    width(j = 1, width = 3) %>%  # Set width of the first column (Predictor)
    width(j = 2:4, width = 1.5)  # Set width of the remaining columns (DT, RF, GLM)
  
  # Create a new Word document
  doc <- read_docx()
  
  # Add a title to the Word document
  doc <- body_add_par(doc, table_par, style = "heading 1")
  
  # Add the flextable to the Word document
  doc <- body_add_flextable(doc, value = table)
  
  # Save the Word document to the specified path
  print(doc, target = plot_name)
}


##################################################################
# scatter_and_residuals_docx #####################################
##################################################################
#' Create scatter and residual plots and save them to a Word document
#'
#' This function generates a scatter plot of observed vs. predicted values and a histogram of residuals, 
#' then saves both plots to a Word document. The scatter plot compares the true values with predicted values, 
#' and the histogram shows the distribution of residuals for each model.
#'
#' @param plot_name A string representing the name of the output Word file (e.g., "model_plots.docx").
#' @param first_title A string representing the title of the scatter plot.
#' @param second_title A string representing the title of the residuals histogram.
#'
#' @return This function does not return any value. It saves the plots to a Word document.
#'
#' @examples
#' # Example usage:
#' scatter_and_residuals_docx("model_plots.docx", "Scatter Plot: Observed vs. Predicted", "Residuals Distribution")
scatter_and_residuals_docx <- function(predictions, plot_name, first_title, second_title) {
  # Create scatter plot of observed vs. predicted values
  scatter_plot <- ggplot(predictions, aes(x = TrueLabel, y = PredictedValues, color = Model)) +
    geom_point(alpha = 0.6) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    facet_wrap(~ Model) +
    theme_minimal() +
    labs(title = first_title,
         x = "Observed Values",
         y = "Predicted Values")
  
  # Calculate residuals
  predictions <- predictions %>%
    mutate(Residuals = TrueLabel - PredictedValues)
  
  # Create histogram plot of residuals
  histogram_plot <- ggplot(predictions, aes(x = Residuals, fill = Model)) +
    geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
    facet_wrap(~ Model) +
    theme_minimal() +
    labs(title = second_title,
         x = "Residuals",
         y = "Frequencies")
  
  # Create a new Word document and add the scatter plot and histogram plot
  doc <- read_docx()
  doc <- doc %>%
    body_add_gg(value = scatter_plot, width = 7, height = 3, style = "Normal")  # Add scatter plot
  doc <- doc %>%
    body_add_gg(value = histogram_plot, width = 7, height = 3, style = "Normal")  # Add histogram plot
  
  # Save the document
  print(doc, target = plot_name)
  
}

regression_roc_curve_docx <- function(predictions, plot_name = "roc_curves.docx", table_par = "") {
  
  # Extract the unique model types from the predictions
  model_types <- unique(predictions$Model)
  
  roc_data_list <- list()  # List to store ROC data for each model
  auc_values <- list()     # List to store AUC values for each model
  
  # Loop through each model type to calculate ROC and AUC
  for (model_type in model_types) {
    # Filter the predictions for the current model
    model_data <- filter(predictions, Model == model_type)
    
    # Create ROC object using the true labels and predicted values
    roc_obj <- roc(response = model_data$TrueLabel, predictor = model_data$PredictedValues)
    
    # Create a data frame with False Positive Rate, True Positive Rate, and Thresholds
    roc_data <- data.frame(
      False_Positive_Rate = 1 - roc_obj$specificities,
      True_Positive_Rate = roc_obj$sensitivities,
      Thresholds = roc_obj$thresholds,
      Model = model_type
    )
    
    # Store ROC data and AUC value
    roc_data_list[[model_type]] <- roc_data
    auc_values[[model_type]] <- auc(roc_obj)
  }
  
  # Combine ROC data for all models into a single data frame
  roc_data <- do.call(rbind, roc_data_list)
  
  # Create the ROC curve plot using ggplot2
  roc_plot <- ggplot(roc_data, aes(x = False_Positive_Rate, y = True_Positive_Rate, color = Model)) +
    geom_line(size = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey") +
    labs(title = table_par, x = "False positive rate", y = "True positive rate") +
    theme_minimal() +
    theme(legend.title = element_blank())  # Remove legend title
  
  # Create a data frame to display AUC values for each model
  auc_table <- data.frame(
    Model = names(auc_values),
    AUC = round(unlist(auc_values), 4)
  )
  
  # Save the ROC curve plot as a PNG file
  ggsave("roc_curves.png", roc_plot, width = 8, height = 6)
  
  # Create a new Word document and add the ROC plot and AUC table
  doc <- read_docx() %>%
    body_add_par(table_par, style = "heading 1") %>%
    body_add_img(src = "roc_curves.png", width = 6, height = 6, style = "centered") %>%
    body_add_par("AUC Value", style = "heading 2") %>%
    body_add_table(auc_table, style = "table_template")
  
  # Save the Word document to the specified path
  print(doc, target = plot_name)
}
