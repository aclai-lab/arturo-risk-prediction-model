
make_docx_table <- function(dataset, variables, path){
  var_labels <- sapply(variables, function(var_name) get_label_by_variable(dataset, var_name))
  var_perc <- round(colSums(is.na(dataset %>% select(all_of(variables)))) / nrow(dataset %>% select(all_of(variables))) * 100, 2)
  
  var_ds <- data.frame(
    variable = names(var_labels),
    label = var_labels,
    Na = paste(var_perc, "%", sep="")
  ) %>% arrange(Na)
  
  ft <- flextable(var_ds)
  doc <- read_docx()
  doc <- body_add_flextable(doc, ft)
  print(doc, target = path)
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
      na_perc =  round(sum(is.na(dataset[[var_name]])) / nrow(dataset) * 100, 2)
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
          labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n NA:", na_perc, "%"), y = "") +
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
          labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n NA:", na_perc, "%"), y = "")+
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
              labs(caption= paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n NA:", na_perc, "%")) 
          }else{
            p <- ggplot(df_occ, aes(x = "", y = Count, fill = Value)) +
              geom_bar(stat = "identity",  color = "black") +
              coord_polar("y", start = 0) +
              theme_void() +
              labs(caption= paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n NA:", na_perc, "%")) 
          }
        
      }
      else if(var_name %in% var_continue){
        p <- ggplot(dataset, aes(x = !!sym(var_name))) +
          geom_density(aes(y = after_stat(count)), fill = "skyblue", alpha = 0.5) +
          labs(x = paste(var_name, "\n", get_label_by_variable(dataset, var_name), "\n NA:", na_perc, "%"), y = "") +
          theme_minimal()  +
          theme(plot.margin = margin(0, 0, 0, 0)) 
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

make_barplot <- function(var_ds, path, xlabs = "", ylabs = "", title = "", width = 11, height = 8){

  var_ds$variable <- factor(var_ds$variable, levels = var_ds$variable[order(var_ds$value, decreasing = TRUE)])
  
  p<-ggplot(var_ds, aes(x = variable, y = value)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = xlabs, y = ylabs, title = title)
  
  ggsave(path, plot = p, device = "pdf", width = width, height = height, units = "in")
}
