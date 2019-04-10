## New function for volcano plot
#library(dplyr)
plot_volcano_new <- function(dep, contrast, label_size = 3,
                         add_names = TRUE, adjusted = FALSE, plot = TRUE) {
  # Show error if inputs are not the required classes
  if(is.integer(label_size)) label_size <- as.numeric(label_size)
  assertthat::assert_that(inherits(dep, "SummarizedExperiment"),
                          is.character(contrast),
                          length(contrast) == 1,
                          is.numeric(label_size),
                          length(label_size) == 1,
                          is.logical(add_names),
                          length(add_names) == 1,
                          is.logical(adjusted),
                          length(adjusted) == 1,
                          is.logical(plot),
                          length(plot) == 1)
  
  row_data <- rowData(dep, use.names = FALSE)
  
  # Show error if inputs do not contain required columns
  if(any(!c("name", "ID") %in% colnames(row_data))) {
    stop(paste0("'name' and/or 'ID' columns are not present in '",
                deparse(substitute(dep)),
                "'.\nRun make_unique() to obtain required columns."),
         call. = FALSE)
  }
  if(length(grep("_p.adj|_diff", colnames(row_data))) < 1) {
    stop(paste0("'[contrast]_diff' and '[contrast]_p.adj' columns are not present in '",
                deparse(substitute(dep)),
                "'.\nRun test_diff() to obtain the required columns."),
         call. = FALSE)
  }
  if(length(grep("_significant", colnames(row_data))) < 1) {
    stop(paste0("'[contrast]_significant' columns are not present in '",
                deparse(substitute(dep)),
                "'.\nRun add_rejections() to obtain the required columns."),
         call. = FALSE)
  }
  
  # Show error if an unvalid contrast is given
<<<<<<< HEAD
  if (length(grep(paste("^",contrast,"_diff", sep = ""),
=======
  if (length(grep(paste(contrast, "_diff", sep = ""),
>>>>>>> 5cb2d667e1353bae926eaac55a636303de8acc23
                  colnames(row_data))) == 0) {
    valid_cntrsts <- row_data %>%
      data.frame() %>%
      select(ends_with("_diff")) %>%
      colnames(.) %>%
      gsub("_diff", "", .)
    valid_cntrsts_msg <- paste0("Valid contrasts are: '",
                                paste0(valid_cntrsts, collapse = "', '"),
                                "'")
    stop("Not a valid contrast, please run `plot_volcano()` with a valid contrast as argument\n",
         valid_cntrsts_msg,
         call. = FALSE)
  }
  
  # Generate a data.frame containing all info for the volcano plot
<<<<<<< HEAD
  diff <- grep(paste("^",contrast,"_diff", sep = ""),
               colnames(row_data))
  if(adjusted) {
    p_values <- grep(paste("^",contrast, "_p.adj", sep = ""),
                     colnames(row_data))
  } else {
    p_values <- grep(paste("^",contrast, "_p.val", sep = ""),
                     colnames(row_data))
  }
  signif <- grep(paste("^",contrast, "_significant", sep = ""),
=======
  diff <- grep(paste(contrast, "_diff", sep = ""),
               colnames(row_data))
  if(adjusted) {
    p_values <- grep(paste(contrast, "_p.adj", sep = ""),
                     colnames(row_data))
  } else {
    p_values <- grep(paste(contrast, "_p.val", sep = ""),
                     colnames(row_data))
  }
  signif <- grep(paste(contrast, "_significant", sep = ""),
>>>>>>> 5cb2d667e1353bae926eaac55a636303de8acc23
                 colnames(row_data))
  df_tmp <- data.frame(diff = row_data[, diff],
                   p_values = -log10(row_data[, p_values]),
                   signif = row_data[, signif],
                   name = row_data$name)
   df<- df_tmp %>% data.frame() %>% filter(!is.na(signif)) %>%
    arrange(signif)
  
  name1 <- gsub("_vs_.*", "", contrast)
  name2 <- gsub(".*_vs_", "", contrast)
  #return(df)
  # Plot volcano with or without labels
  p <- ggplot(df, aes(diff, p_values)) +
    geom_vline(xintercept = 0) +
    geom_point(aes(col = signif)) +
    geom_text(data = data.frame(), aes(x = c(Inf, -Inf),
                                       y = c(-Inf, -Inf),
                                       hjust = c(1, 0),
                                       vjust = c(-1, -1),
                                       label = c(name1, name2),
                                       size = 5,
                                       fontface = "bold")) +
    labs(title = contrast,
         x = expression(log[2]~"Fold change")) +
    theme_DEP1() +
    theme(legend.position = "none") +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey"))
  if (add_names) {
    p <- p + ggrepel::geom_text_repel(data = filter(df, signif),
                                      aes(label = name),
                                      size = label_size,
                                      box.padding = unit(0.1, 'lines'),
                                      point.padding = unit(0.1, 'lines'),
                                      segment.size = 0.5)
  }
  if(adjusted) {
    p <- p + labs(y = expression(-log[10]~"Adjusted p-value"))
  } else {
    p <- p + labs(y = expression(-log[10]~"P-value"))
  }
  if(plot) {
   # return(list(p, df))
   # return(df)
    return(p)
  } else {
    df <- df %>%
      select(name, diff, p_value, signif) %>%
      arrange(desc(x))
    colnames(df)[c(1,2,3)] <- c("protein", "log2_fold_change", "p_value_-log10")
    if(adjusted) {
      colnames(df)[3] <- "adjusted_p_value_-log10"
    }
    return(df)
  }
}


#####====== get_volcano_df =======#######
get_volcano_df <- function(dep, contrast, adjusted = FALSE) {
  # Show error if inputs are not the required classes
  assertthat::assert_that(inherits(dep, "SummarizedExperiment"),
                          is.character(contrast),
                          length(contrast) == 1)
  
  row_data <- rowData(dep, use.names = FALSE)
  
  # Show error if inputs do not contain required columns
  if(any(!c("name", "ID") %in% colnames(row_data))) {
    stop(paste0("'name' and/or 'ID' columns are not present in '",
                deparse(substitute(dep)),
                "'.\nRun make_unique() to obtain required columns."),
         call. = FALSE)
  }
  if(length(grep("_p.adj|_diff", colnames(row_data))) < 1) {
    stop(paste0("'[contrast]_diff' and '[contrast]_p.adj' columns are not present in '",
                deparse(substitute(dep)),
                "'.\nRun test_diff() to obtain the required columns."),
         call. = FALSE)
  }
  if(length(grep("_significant", colnames(row_data))) < 1) {
    stop(paste0("'[contrast]_significant' columns are not present in '",
                deparse(substitute(dep)),
                "'.\nRun add_rejections() to obtain the required columns."),
         call. = FALSE)
  }
  
  # Show error if an unvalid contrast is given
  if (length(grep(paste(contrast, "_diff", sep = ""),
                  colnames(row_data))) == 0) {
    valid_cntrsts <- row_data %>%
      data.frame() %>%
      select(ends_with("_diff")) %>%
      colnames(.) %>%
      gsub("_diff", "", .)
    valid_cntrsts_msg <- paste0("Valid contrasts are: '",
                                paste0(valid_cntrsts, collapse = "', '"),
                                "'")
    stop("Not a valid contrast, please run `plot_volcano()` with a valid contrast as argument\n",
         valid_cntrsts_msg,
         call. = FALSE)
  }
  
  # Generate a data.frame containing all info for the volcano plot
  diff <- grep(paste(contrast, "_diff", sep = ""),
               colnames(row_data))
  if(adjusted) {
    p_values <- grep(paste(contrast, "_p.adj", sep = ""),
                     colnames(row_data))
  } else {
    p_values <- grep(paste(contrast, "_p.val", sep = ""),
                     colnames(row_data))
  }
  signif <- grep(paste(contrast, "_significant", sep = ""),
                 colnames(row_data))
  df_tmp <- data.frame(diff = row_data[, diff],
                       p_values = -log10(row_data[, p_values]),
                       signif = row_data[, signif],
                       name = row_data$name)
  df<- df_tmp %>% data.frame() %>% filter(!is.na(signif)) %>%
    arrange(signif)
  
 return(df)
}

### Function to plot intensities of individual proteins
plot_protein<-function(dep, protein, type){
  assertthat::assert_that(inherits(dep, "SummarizedExperiment"),
                          is.character(protein),
                          is.character(type))
  subset<-dep[protein]
  
  df_reps <- data.frame(assay(subset)) %>%
    rownames_to_column() %>%
    gather(ID, val, -rowname) %>%
    left_join(., data.frame(colData(subset)), by = "ID")
  df_reps$rowname <- parse_factor(df_reps$rowname, levels = protein)
  
  df_CI<- df_reps %>%
    group_by(condition, rowname) %>%
    summarize(mean = mean(val, na.rm = TRUE),
              sd = sd(val, na.rm = TRUE),
              n = n()) %>%
    mutate(error = qnorm(0.975) * sd / sqrt(n),
           CI.L = mean - error,
           CI.R = mean + error) %>%
    as.data.frame()
  df_CI$rowname <- parse_factor(df_CI$rowname, levels = protein)
  
  if(type=="violin"){
    p<-ggplot(df_reps, aes(condition, val))+
      geom_violin(fill="grey90", scale = "width",
                  draw_quantiles = 0.5,
                  trim =TRUE) +
      geom_jitter(aes(color = factor(replicate)),
                  size = 3, position = position_dodge(width=0.3)) +
      labs(
        y = expression(log[2]~"Intensity"),
        col = "Replicates") +
      facet_wrap(~rowname) +
      scale_color_brewer(palette = "Dark2")+
      theme_DEP1()+
      theme(axis.title.x = element_blank())
    }
  
  if(type=="boxplot"){
    p<-ggplot(df_reps, aes(condition, val))+
      geom_boxplot()+
      geom_jitter(aes(color = factor(replicate)),
                  size = 3, position = position_dodge(width=0.3)) +
      labs(
        y = expression(log[2]~"Intensity"),
        col = "Replicates") +
      facet_wrap(~rowname) +
      scale_color_brewer(palette = "Dark2")+
      theme_DEP1() +
      theme(axis.title.x = element_blank())
  }
  
  if(type=="interaction"){
    p<-ggplot(df_reps, aes(condition, val))+
      geom_point(aes(color = factor(replicate)),
                 size = 3) +
      geom_line(aes(group= factor(replicate), color= factor(replicate)))+
      labs(
        y = expression(log[2]~"Intensity"),
        col = "Replicates") +
      facet_wrap(~rowname) +
      scale_color_brewer(palette = "Dark2")+
      theme_DEP1()+
      theme(axis.title.x = element_blank())
  }
  
  if(type=="dot"){
    p<-ggplot(df_CI, aes(condition, mean))+
      geom_point(data=df_reps, aes(x=condition, y=val, color = factor(replicate)),
                 size = 3, position= position_dodge(width = 0.2)) +
      geom_errorbar(aes(ymin = CI.L, ymax = CI.R), width = 0.2)+
      labs(
        y = expression(log[2]~"Intensity"~"(\u00B195% CI)"),
        col = "Replicates") +
      facet_wrap(~rowname) +
      scale_color_brewer(palette = "Dark2")+
      theme_DEP1() +
      theme(axis.title.x = element_blank())
  }
  
  return(p)
}
<<<<<<< HEAD

plot_volcano_mod <- function(dep, contrast, label_size = 3,
                             add_names = TRUE, adjusted = FALSE, plot = TRUE) {
  # Show error if inputs are not the required classes
  if(is.integer(label_size)) label_size <- as.numeric(label_size)
  assertthat::assert_that(inherits(dep, "SummarizedExperiment"),
                          is.character(contrast),
                          length(contrast) == 1,
                          is.numeric(label_size),
                          length(label_size) == 1,
                          is.logical(add_names),
                          length(add_names) == 1,
                          is.logical(adjusted),
                          length(adjusted) == 1,
                          is.logical(plot),
                          length(plot) == 1)
  
  row_data <- rowData(dep, use.names = FALSE)
  
  # Show error if inputs do not contain required columns
  if(any(!c("name", "ID") %in% colnames(row_data))) {
    stop(paste0("'name' and/or 'ID' columns are not present in '",
                deparse(substitute(dep)),
                "'.\nRun make_unique() to obtain required columns."),
         call. = FALSE)
  }
  if(length(grep("_p.adj|_diff", colnames(row_data))) < 1) {
    stop(paste0("'[contrast]_diff' and '[contrast]_p.adj' columns are not present in '",
                deparse(substitute(dep)),
                "'.\nRun test_diff() to obtain the required columns."),
         call. = FALSE)
  }
  if(length(grep("_significant", colnames(row_data))) < 1) {
    stop(paste0("'[contrast]_significant' columns are not present in '",
                deparse(substitute(dep)),
                "'.\nRun add_rejections() to obtain the required columns."),
         call. = FALSE)
  }
  
  # Show error if an unvalid contrast is given
  if (length(grep(paste("^",contrast, "_diff", sep = ""),
                  colnames(row_data))) == 0) {
    valid_cntrsts <- row_data %>%
      data.frame() %>%
      select(ends_with("_diff")) %>%
      colnames(.) %>%
      gsub("_diff", "", .)
    valid_cntrsts_msg <- paste0("Valid contrasts are: '",
                                paste0(valid_cntrsts, collapse = "', '"),
                                "'")
    stop("Not a valid contrast, please run `plot_volcano()` with a valid contrast as argument\n",
         valid_cntrsts_msg,
         call. = FALSE)
  }
  
  # Generate a data.frame containing all info for the volcano plot
  diff <- grep(paste("^",contrast, "_diff", sep = ""),
               colnames(row_data))
  if(adjusted) {
    p_values <- grep(paste("^",contrast, "_p.adj", sep = ""),
                     colnames(row_data))
  } else {
    p_values <- grep(paste("^", contrast, "_p.val", sep = ""),
                     colnames(row_data))
  }
  signif <- grep(paste("^",contrast, "_significant", sep = ""),
                 colnames(row_data))
  df <- data.frame(x = row_data[, diff],
                   y = -log10(row_data[, p_values]),
                   significant = row_data[, signif],
                   name = row_data$name) %>%
    filter(!is.na(significant)) %>%
    arrange(significant)
  
  name1 <- gsub("_vs_.*", "", contrast)
  name2 <- gsub(".*_vs_", "", contrast)
  
  # Plot volcano with or without labels
  p <- ggplot(df, aes(x, y)) +
    geom_vline(xintercept = 0) +
    geom_point(aes(col = significant)) +
    geom_text(data = data.frame(), aes(x = c(Inf, -Inf),
                                       y = c(-Inf, -Inf),
                                       hjust = c(1, 0),
                                       vjust = c(-1, -1),
                                       label = c(name1, name2),
                                       size = 5,
                                       fontface = "bold")) +
    labs(title = contrast,
         x = expression(log[2]~"Fold change")) +
    theme_DEP1() +
    theme(legend.position = "none") +
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "grey"))
  if (add_names) {
    p <- p + ggrepel::geom_text_repel(data = filter(df, significant),
                                      aes(label = name),
                                      size = label_size,
                                      box.padding = unit(0.1, 'lines'),
                                      point.padding = unit(0.1, 'lines'),
                                      segment.size = 0.5)
  }
  if(adjusted) {
    p <- p + labs(y = expression(-log[10]~"Adjusted p-value"))
  } else {
    p <- p + labs(y = expression(-log[10]~"P-value"))
  }
  if(plot) {
    return(p)
  } else {
    df <- df %>%
      select(name, x, y, significant) %>%
      arrange(desc(x))
    colnames(df)[c(1,2,3)] <- c("protein", "log2_fold_change", "p_value_-log10")
    if(adjusted) {
      colnames(df)[3] <- "adjusted_p_value_-log10"
    }
    return(df)
  }
}

=======
>>>>>>> 5cb2d667e1353bae926eaac55a636303de8acc23
