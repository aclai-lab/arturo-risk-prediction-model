make_docx_table <- function(dataset, variables, path){ 
  var_labels <- sapply(variables, function(var_name) get_label_by_variable(dataset, var_name))
  neg_perc <- negative_perc(dataset)
  perc_minus1 <- neg_perc[[1]]
  
  var_ds <- data.frame(
    variable = names(var_labels),
    label = var_labels,
    perc_missing = paste(perc_minus1, "%", sep="")
  ) %>% arrange(Na)
   
  ft <- flextable(var_ds)
  doc <- read_docx()
  doc <- body_add_flextable(doc, ft)
  print(doc, target = path)
}

make_alluvial_plot <- function(dataset, output_file = "results/euro_alluvial_plot.docx") {
  
  dataset$wave <- as.factor(dataset$wave)
  dataset$euro_d <- as.factor(dataset$euro_d)
  dataset$initial_euro_d <- as.factor(dataset$initial_euro_d)
  
  alluvial_plot1 <- ggplot(data = dataset, aes(axis1 = wave, axis2 = euro_d)) +
    geom_alluvium(aes(fill = initial_euro_d)) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_minimal() +
    scale_x_discrete(limits = c("WAVES", "DEPRESSION"), expand = c(.05, .05)) +
    labs(y = "", x = "",
         title = "Alluvial Plot of Depression Distribution in Base Line Across Waves")
  
  alluvial_plot2 <- ggplot(data = dataset, aes(axis1 = wave, axis2 = euro_d)) +
    geom_alluvium(aes(fill = euro_d)) +
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_minimal() +
    scale_x_discrete(limits = c("WAVES", "DEPRESSION"), expand = c(.05, .05)) +
    labs(y = "", x = "",
         title = "Alluvial Plot of Depression Distribution at Follow UP Across Waves")
  
  w5 <- dataset %>% filter(dataset$wave == 5)
  w6 <- dataset %>% filter(dataset$wave == 6)
  w7 <- dataset %>% filter(dataset$wave == 7)
  
  alluvial_plot3 <- ggplot(data = w5, aes(axis1 = initial_euro_d, axis2 = euro_d)) +
    geom_alluvium(aes(fill = euro_d)) + 
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_minimal() +
    scale_x_discrete(limits = c("DEPRESSION BL", "DEPRESSION FU"), expand = c(.05, .05)) +
    labs(y = "", x = "",
         title = "Alluvial Plot BL vs FU Depression in wave 5")
  
  alluvial_plot4 <- ggplot(data = w6, aes(axis1 = initial_euro_d, axis2 = euro_d)) +
    geom_alluvium(aes(fill = euro_d)) + 
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_minimal() +
    scale_x_discrete(limits = c("DEPRESSION BL", "DEPRESSION FU"), expand = c(.05, .05)) +
    labs(y = "", x = "",
         title = "Alluvial Plot BL vs FU Depression in wave 6")
  
  alluvial_plot5 <- ggplot(data = w7, aes(axis1 = initial_euro_d, axis2 = euro_d)) +
    geom_alluvium(aes(fill = euro_d)) + 
    geom_stratum() +
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_minimal() +
    scale_x_discrete(limits = c("DEPRESSION BL", "DEPRESSION FU"), expand = c(.05, .05)) +
    labs(y = "", x = "",
         title = "Alluvial Plot BL vs FU Depression in wave 7")
  
  # Create a Word document
  doc <- read_docx()
  
  # Add the first plot to the document
  doc <- doc %>%
    body_add_gg(value = alluvial_plot1, width = 6, height = 4) %>%
    body_add_gg(value = alluvial_plot2, width = 6, height = 4) %>%
    body_add_gg(value = alluvial_plot3, width = 6, height = 4) %>%
    body_add_gg(value = alluvial_plot4, width = 6, height = 4) %>%
    body_add_gg(value = alluvial_plot5, width = 6, height = 4)
 
  # Save the document
  print(doc, target = output_file)
} 

make_docx_variable_list <- function(dataset, domain_mapping, var_continue, var_ord, path){
  neg_perc <- negative_perc(dataset)
  perc_minus1 <- neg_perc[[1]]
  data<- tibble(label_variabile=character(), type=character(), domain=character(), labels_variabile=character(), perc_minus1=character())
  
  for (var_name in names(dataset)) {
    print(var_name)
    label_var <- get_label_by_variable(dataset, var_name) 
    labels_var <- get_labels_by_variable(dataset, var_name)
    if(length(labels_var) > 1) {
      labels_var <- paste(labels_var$label, collapse=", ")
    } else {
      labels_var <- as.character(labels_var$label)
    }
    
    label_var <- as.character(label_var)
    domain <- domain_mapping$domain[domain_mapping$variable == var_name]
    
    if(var_name %in% var_continue){
      type = "numeric with -1 categorial"
    }else if(var_name %in% var_ord){
      type = "orderable"
    }else{
      type = "not orderable"
    }
    
    data <- bind_rows(data, tibble(label_variabile=label_var, type=type, domain = domain, labels_variabile=labels_var, perc_minus1 = paste(perc_minus1[[var_name]], "%", sep="")) )
  }
  
  ft <- flextable(data)
  doc <- read_docx()
  doc <- body_add_flextable(doc, ft)
  print(doc, target = path)
}

make_wave_flag_table <- function(dataset, wave) {
  table <- dataset %>%
    filter(wave == !!wave) %>%
    group_by(flag_cfu) %>%
    summarize( 
      occ = n(),
      age = round(mean(age_int, na.rm = TRUE), 2),
      male = sum(dn042_ == 0, na.rm = TRUE),
      female = sum(dn042_ == 1, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    flextable() %>%
    set_header_labels(
      flag_cfu = "Cancer FU",
      occ = "N",
      age = "Age",
      male = "M",
      female = "F"
    ) %>%
    add_header_row(
      values = paste("Wave", wave, "Table"),
      colwidths = 5
    ) %>%
    align(align = "center", part = "header") %>%
    colformat_double(j = "age", digits = 2) %>%
    border_outer(part = "all", border = fp_border(color = "black", width = 1, style = "solid")) %>%
    border_inner_h(part = "all", border = fp_border(color = "black", width = 1, style = "solid")) %>%
    border_inner_v(part = "all", border = fp_border(color = "black", width = 1, style = "solid"))
  
  return(table)
}

make_wave_condition_table <- function(dataset, wave) {
  table <- dataset %>%
    filter(wave == !!wave) %>%
    group_by(condition_fu) %>%
    summarize( 
      occ = n(),
      age = round(mean(age_int, na.rm = TRUE), 2),
      male = sum(dn042_ == 0, na.rm = TRUE),
      female = sum(dn042_ == 1, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    flextable() %>%
    set_header_labels(
      condition_fu = "Condition FU",
      occ = "N",
      age = "Age",
      male = "M",
      female = "F"
    ) %>%
    add_header_row(
      values = paste("Wave", wave, "Table"),
      colwidths = 5
    ) %>%
    align(align = "center", part = "header") %>%
    colformat_double(j = "age", digits = 2) %>%
    border_outer(part = "all", border = fp_border(color = "black", width = 1, style = "solid")) %>%
    border_inner_h(part = "all", border = fp_border(color = "black", width = 1, style = "solid")) %>%
    border_inner_v(part = "all", border = fp_border(color = "black", width = 1, style = "solid"))
  
  return(table)
}

make_wave_euro_table <- function(dataset, wave) {
  table <- dataset %>%
    filter(wave == !!wave) %>%  # Filtra il dataset per l'onda specificata
    mutate(euro_combination = paste(initial_euro_d, euro_d, sep = "-")) %>%  # Crea una colonna combinata
    group_by(euro_combination) %>%  # Raggruppa per la combinazione euro
    summarize(
      occ = n(),  # Calcola il numero di occorrenze
      age = round(mean(age_int, na.rm = TRUE), 2),  # Calcola l'età media arrotondata a 2 decimali
      male = sum(dn042_ == 0, na.rm = TRUE),  # Conta il numero di maschi
      female = sum(dn042_ == 1, na.rm = TRUE)  # Conta il numero di femmine
    ) %>%
    ungroup() %>%  # Rimuove i raggruppamenti
    flextable() %>%  # Converte il risultato in una flextable
    set_header_labels(
      euro_combination = "BL->FU",
      occ = "N", 
      age = "Age",
      male = "M",
      female = "F"
    ) %>%
    add_header_row(
      values = paste("Wave", wave),
      colwidths = 5
    ) %>%
    align(align = "center", part = "header") %>%  # Centra il testo nella riga di intestazione
    colformat_double(j = "age", digits = 2) %>%
    border_outer(part = "all", border = fp_border(color = "black", width = 1, style = "solid")) %>%
    border_inner_h(part = "all", border = fp_border(color = "black", width = 1, style = "solid")) %>%
    border_inner_v(part = "all", border = fp_border(color = "black", width = 1, style = "solid"))
  
  return(table)
}

make_wave_table_docx <- function(dataset, euro = FALSE, flag = FALSE, condition = FALSE, output_file = ""){
  waves <- unique(dataset$wave)
  
  doc <- read_docx()
  
  for(w in waves) {
    
    if (euro == TRUE)
      wave_table <- make_wave_euro_table(dataset, w)
    else if (flag ==  TRUE)
      wave_table <- make_wave_flag_table(dataset, w)
    else if (condition == TRUE)
      wave_table <- make_wave_condition_table(dataset, w)
    
    doc <- body_add_flextable(doc, value = wave_table)
    doc <- body_add_par(doc, value = "\n", style = "Normal")
  } 
  
  print(doc, target = output_file)
}

make_distribution_pdf <- function(dataset, variables, var_type, pdf_name, width = 20, height = 17, per_page, layout) {
  pdf(paste("plot/",pdf_name,sep=""), width = width, height = height)
  
  num_pages <- ceiling(length(variables) / per_page)
  var_occ <- create_var_occ(dataset)
  var_continue <- var_type$var_continue
  var_euro <- var_type$var_euro
  var_ord <- var_type$var_ord
  var_not_ord<- var_type$var_not_ord
  
  for (page in 1:num_pages) {
    
    plots_page <- list()
    start_index <- (page - 1) * per_page + 1
    end_index <- min(page * per_page, length(variables))
    
    
    for (i in start_index:end_index) {
      var_name <- variables[i]
      neg_perc <- negative_perc(dataset)
      perc_minus1 <- neg_perc[[1]]
     
      if(!(var_name %in% var_continue)){
        df_occ <- as.data.frame(var_occ[[var_name]])
        names(df_occ) <- c("Value", "Count")
        var_labels_df <- get_labels_by_variable(dataset, var_name)
        var_labels_df$label <- substr(var_labels_df$label, 1, 30)
      }
      
      if(var_name %in% var_euro){
        df_occ$ValueLabel <- with(var_labels_df, paste(value, "-", label))
        df_occ$ValueLabel <- factor(df_occ$ValueLabel, levels = df_occ$ValueLabel)
        
        p <- ggplot(df_occ, aes(x = ValueLabel, y = Count)) +
          geom_bar(stat = "identity", fill = "skyblue", color = "black", alpha = 0.5) +
          labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n -1:", perc_minus1[[var_name]], "%"), y = "")+
          theme_minimal() +
          theme(legend.position = "none")
      }
      else if(var_name %in% var_ord){
        data_for_plot <- dataset %>%
          filter(!is.na(.data[[var_name]]), !is.na(euro_d))
        data_for_plot <- data_for_plot %>% mutate(var_name = factor(.data[[var_name]]), euro_d = factor(euro_d))
        
        p <- ggplot(data_for_plot, aes(x = var_name, fill = euro_d)) +
          geom_bar( color = "black", alpha = 0.5) +
          scale_x_discrete(breaks = var_labels_df$value,
                           labels = var_labels_df$label) +
          scale_fill_manual(values = c("yes" = "skyblue", "no" = "orange")) +
          theme_minimal() +
          labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n -1:", perc_minus1[[var_name]], "%"), y = "")+
          theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
      }
      else if(var_name %in% var_not_ord){
        
        if(nrow(var_labels_df) > 0){
          var_labels_df$legend_label <- paste(df_occ$Value, "->", var_labels_df$label)
          df_occ$ValueLabel <- with(var_labels_df, paste(value, "->", label))
          df_occ$ValueLabel <- factor(df_occ$ValueLabel, levels = df_occ$ValueLabel)
        }else{
          df_occ$ValueLabel <- as.factor(df_occ$Value)
          var_labels_df$legend_label <- as.character(var_labels_df$value)
        }
        
        colors <- colorRampPalette(RColorBrewer::brewer.pal(min(9, length(unique(df_occ$ValueLabel))), "Set1"))(length(unique(df_occ$ValueLabel)))
        
          if(var_name != "ch001_" & var_name != "ch021_"){
            p <- ggplot(df_occ, aes(x = "", y = Count, fill = ValueLabel)) +
              geom_bar(stat = "identity",  color = "black") +
              coord_polar("y", start = 0) +
              theme_void() +
              scale_fill_manual(values = colors, labels = var_labels_df$legend_label, name = var_name) +
              labs(caption= paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n -1:", perc_minus1[[var_name]], "%")) 
          }else{
            p <- ggplot(df_occ, aes(x = "", y = Count, fill = Value)) +
              geom_bar(stat = "identity",  color = "black") +
              coord_polar("y", start = 0) +
              theme_void() +
              labs(caption= paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n -1:", perc_minus1[[var_name]], "%"))  }
        
      }
      else if(var_name %in% var_continue){
        p <- ggplot(dataset, aes(x = !!sym(var_name))) +
          geom_density(aes(y = after_stat(count)), fill = "skyblue", alpha = 0.5) +
          labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n -1:", perc_minus1[[var_name]], "%"), y = "")+
          theme_minimal() 
      }
      
      
      plots_page[[i - start_index + 1]] <- p
    }
    
    
    do.call(grid.arrange, c(plots_page, ncol = layout[1], nrow = layout[2], padding = unit(0.5, "lines")))
  }
  
  dev.off()
}

pre_analysis_plot <- function(
    dataset, 
    var_type,
    path = ""
){
  
  var_continue <- var_type$var_continue
  var_euro <- var_type$var_euro
  var_ord <- var_type$var_ord
  var_not_ord<- var_type$var_not_ord
  
  if(length(var_continue) > 0 ){
    # continue distribution hist
    make_distribution_pdf(dataset = dataset,
                          variables = var_continue,
                          var_type = var_type,
                          pdf_name = paste(path,"continue_distribution.pdf", sep=""), 
                          width = 20, 
                          height = 17, 
                          per_page = 12, 
                          layout = c(3, 4))
  }
  
  if(length(var_euro) > 0 ){
    # euro distribution hist
    make_distribution_pdf(dataset = dataset,
                          variables = var_euro,
                          var_type = var_type,
                          pdf_name = paste(path,"euro_distribution.pdf", sep=""),
                          width = 20, 
                          height = 17, 
                          per_page = 12, 
                          layout = c(3, 4))
  }
  
  if(length(var_ord) > 0 ){
    # ordinable distribution hist
    make_distribution_pdf(dataset = dataset, 
                          variables = var_ord,
                          var_type = var_type,
                          pdf_name = paste(path, "fill_ord_distribution.pdf", sep=""), 
                          width = 25, 
                          height = 22, 
                          per_page = 12,  
                          layout = c(3, 4))
  }
  
  if(length(var_not_ord) > 0 ){
    # not ordinable distribution hist
    make_distribution_pdf(dataset = dataset,
                          variables = var_not_ord,
                          var_type = var_type,
                          pdf_name = paste(path,"not_ord_distribution.pdf", sep=""),
                          width = 25, 
                          height = 22, 
                          per_page = 12, 
                          layout = c(3, 4))
  }
}

make_variancy_barplot_docx <- function(
    dataset, 
    var_continue,
    var_ord,
    var_not_ord,
    var_euro,
    path, 
    width = 6, 
    height = 1.7
){
  
  ds <- dataset %>% select(all_of(var_continue))
  variancy_ds <- sapply(ds, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA)
  
  variancy_ds <- data.frame(
    variable = names(variancy_ds),
    value = variancy_ds
  ) %>% na.omit(variancy_ds)
  
  variancy_ds$variable <- factor(variancy_ds$variable, levels = variancy_ds$variable[order(variancy_ds$value, decreasing = TRUE)])
  
  p1 <- ggplot(variancy_ds, aes(x = variable, y = value)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Variables", y = "Variances")
  
  ds <- dataset %>% select(all_of(var_ord))
  variancy_ds <- sapply(ds, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA)
  
  variancy_ds <- data.frame(
    variable = names(variancy_ds),
    value = variancy_ds
  ) %>% na.omit(variancy_ds)
  
  variancy_ds$variable <- factor(variancy_ds$variable, levels = variancy_ds$variable[order(variancy_ds$value, decreasing = TRUE)])
  
  p2 <- ggplot(variancy_ds, aes(x = variable, y = value)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Variables", y = "Variances")
  
  ds <- dataset %>% select(all_of(var_not_ord))
  variancy_ds <- sapply(ds, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA)
  
  variancy_ds <- data.frame(
    variable = names(variancy_ds),
    value = variancy_ds
  ) %>% na.omit(variancy_ds)
  
  variancy_ds$variable <- factor(variancy_ds$variable, levels = variancy_ds$variable[order(variancy_ds$value, decreasing = TRUE)])
  
  p3 <- ggplot(variancy_ds, aes(x = variable, y = value)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Variables", y = "Variances")
  
  ds <- dataset %>% select(all_of(var_euro))
  variancy_ds <- sapply(ds, function(x) if(is.numeric(x)) var(x, na.rm = TRUE) else NA)
  
  variancy_ds <- data.frame(
    variable = names(variancy_ds),
    value = variancy_ds
  ) %>% na.omit(variancy_ds)
  
  variancy_ds$variable <- factor(variancy_ds$variable, levels = variancy_ds$variable[order(variancy_ds$value, decreasing = TRUE)])
  
  p4 <- ggplot(variancy_ds, aes(x = variable, y = value)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Variables", y = "Variances")
  
  # Create a new Word document
  doc <- read_docx()
  
  # Add the ggplot objects to the Word document in a 2x2 table
  doc <- doc %>%
    body_add_par("Continuous Variables", style = "heading 1") %>%
    body_add_gg(p1, width, height, style = "centered") %>%
    body_add_par("Ordinal Variables", style = "heading 1") %>%
    body_add_gg(p2, width, height, style = "centered") %>%
    body_add_par("Not ordinal Variables", style = "heading 1") %>%
    body_add_gg(p3, width, height, style = "centered") %>%
    body_add_par("Euro Variables", style = "heading 1") %>%
    body_add_gg(p4, width, height, style = "centered")
  
  # Save the Word document
  print(doc, target = path)
}


# Funzione per creare i grafici delle variabili
plot_variable_distribution <- function(dataset, var_name, domain_mapping, condition_label) {

  var_type <- get_var_type(dataset, domain_mapping)
  var_continue <- var_type$var_continue
  var_euro <- var_type$var_euro
  var_ord <- var_type$var_ord
  var_not_ord <- var_type$var_not_ord
  var_occ <- create_var_occ(dataset)
  
  p <- NULL
  
  if (!(var_name %in% var_continue)) {
    df_occ <- as.data.frame(var_occ[[var_name]])
    names(df_occ) <- c("Value", "Count")
    var_labels_df <- get_labels_by_variable(dataset, var_name)
    var_labels_df$label <- substr(var_labels_df$label, 1, 30)
  }
  
  if (var_name %in% var_ord ) {
    data_for_plot <- dataset %>% mutate(var_name = factor(.data[[var_name]]))
    
    p <- ggplot(data_for_plot, aes(x = var_name)) +
      geom_bar(fill = "skyblue", color = "black", alpha = 0.5) +
      theme_minimal() +
      theme(axis.title = element_blank(), legend.position = "none")
  }
  else if(var_name %in% var_not_ord){
    data_for_plot <- dataset %>% mutate(var_name = factor(.data[[var_name]]))
    
    p <- ggplot(data_for_plot, aes(x = var_name)) +
      geom_bar(fill = "orange", color = "black", alpha = 0.5) +
      theme_minimal() +
      theme(axis.title = element_blank(), legend.position = "none")
  }
  else if(var_name %in% var_euro ){
    data_for_plot <- dataset %>% mutate(var_name = factor(.data[[var_name]]))
    
    p <- ggplot(data_for_plot, aes(x = var_name)) +
      geom_bar(fill = "green", color = "black", alpha = 0.5) +
      theme_minimal() +
      theme(axis.title = element_blank(), legend.position = "none")
  }
  else if (var_name %in% var_continue) {
    p <- ggplot(dataset, aes(x = !!sym(var_name))) +
      geom_density(aes(y = after_stat(count)), fill = "skyblue", alpha = 0.5) +
      theme_minimal() +
      theme(axis.title = element_blank(), legend.position = "none")
  }
  
  return(p)
}

create_descriptive_table_with_plots <- function(dataset, domain_mapping, output_file, dataset_death = NULL, column3 = "ALIVE", column4 = "DEATH", column5 = "MISSING") { 
  temp_files <- list()
  var_names <- setdiff(names(dataset), c("mergeid", "wave", "hhid6", "mergeidp6", "coupleid6", "hhid5", "mergeidp5", "coupleid5", "hhid7", "mergeidp7", "coupleid7", "hhsize", "language", "ph012_", "ph013_", "euro_d", "initial_euro_d", "condition_fu", "hh017e"))
  
  labels_list <- sapply(var_names, function(var_name) {
    var_labels_df <- get_labels_by_variable(dataset, var_name)
    if (nrow(var_labels_df) > 0) {
      paste0(var_labels_df$value, ":", var_labels_df$label, collapse = ",\n")
    } else {
      NA
    }
  }, USE.NAMES = TRUE)
  
  label_list <- sapply(var_names, function(var_name) {
    var_label_df <- get_label_by_variable(dataset, var_name)
  }, USE.NAMES = TRUE)
  
  for (var_name in var_names) {
    alive_plot <- if(is.null(dataset_death))
                     plot_variable_distribution(dataset, var_name, domain_mapping, column3)
                  else 
                     plot_variable_distribution(dataset_death %>% filter(condition_fu == 0), var_name, domain_mapping, column3)
    death_plot <- if(is.null(dataset_death)) 
      plot_variable_distribution(dataset %>% filter(euro_d == "yes"), var_name, domain_mapping, column4)
    else 
      plot_variable_distribution(dataset_death %>% filter(condition_fu == 1), var_name, domain_mapping, column4)
    missing_plot <- if(is.null(dataset_death)) 
      plot_variable_distribution(dataset %>% filter(euro_d == "no"), var_name, domain_mapping, column5)
    else 
      plot_variable_distribution(dataset_death %>% filter(condition_fu == 2), var_name, domain_mapping, column5)
    
    temp_file_alive <- tempfile(fileext = ".png")
    temp_file_death <- tempfile(fileext = ".png")
    temp_file_missing <- tempfile(fileext = ".png")
    
    ggsave(temp_file_alive, alive_plot, width = 4, height = 1, dpi = 300)
    ggsave(temp_file_death, death_plot, width = 4, height = 1, dpi = 300)
    ggsave(temp_file_missing, missing_plot, width = 4, height = 1, dpi = 300)
    
    temp_files[[var_name]] <- list(alive = temp_file_alive, death = temp_file_death, missing = temp_file_missing)
  }
  
  # Creare una tabella flessibile con flextable
  if(is.null(dataset_death))
    ft <- flextable(data.frame(VARIABLE = label_list, LABELS = labels_list, SELECTED = "", DEPRESSED = "", NOTDEPRESSED = ""))
  else
    ft <- flextable(data.frame(VARIABLE = label_list, LABELS = labels_list, ALIVE = "", DEATH = "", MISSING = ""))
  
  
  for (i in seq_along(var_names)) {
    var_name <- var_names[i]
    ft <- compose(ft, i, j = 3, value = as_paragraph(as_image(temp_files[[var_name]]$alive, width = 1, height = 0.25)))
    ft <- compose(ft, i, j = 4, value = as_paragraph(as_image(temp_files[[var_name]]$death, width = 1, height = 0.25)))
    ft <- compose(ft, i, j = 5, value = as_paragraph(as_image(temp_files[[var_name]]$missing, width = 1, height = 0.25)))
  }
  
  # Allungare la larghezza della tabella
  ft <- width(ft, j = 1, width = 1)
  ft <- width(ft, j = 2, width = 1)
  ft <- width(ft, j = 3:5, width = 1.2)
  
  # Aggiungere bordi
  ft <- border_outer(ft, part = "all", border = fp_border(color = "black", width = 1))
  ft <- border_inner_h(ft, border = fp_border(color = "black", width = 0.5), part = "all")
  ft <- border_inner_v(ft, border = fp_border(color = "black", width = 0.5), part = "all")
  
  ft <- line_spacing(ft, space = 1.0, part = "body")
  
  # Ridurre la dimensione del testo e gli spazi tra i caratteri
  ft <- fontsize(ft, size = 3, part = "all")
  ft <- font(ft, fontname = "Arial", part = "all")
  
  
  # Crea un nuovo documento Word
  doc <- read_docx() %>% body_add_flextable(value = ft)
  
  legend_paragraph <- fpar(
    ftext("LEGEND\n", prop = fp_text(bold = TRUE, font.size = 12)),
    ftext("Alive: ..., Death: ..., Missing: ...", prop = fp_text(font.size = 10))
  )
  doc <- body_add_fpar(doc, legend_paragraph, style = "Normal")
  
  
  # Salva il documento Word
  print(doc, target = output_file)
  
  # Rimuovi i file temporanei
  unlink(unlist(temp_files))
}

