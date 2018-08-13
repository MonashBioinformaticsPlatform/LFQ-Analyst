#Define server logic to read selected file ----
server <- function(input, output) {
  options(shiny.maxRequestSize=100*1024^2)  ## Set maximum upload size to 100MB
 
#  Show elements on clicking Start analysis button
   observeEvent(input$analyze,{
    shinyjs::show("downloadbox")
    })
   
   observeEvent(input$analyze,{
     shinyjs::show("results_tab")
   })
   
   observeEvent(input$analyze,{
     shinyjs::show("qc_tab")
   })
   
   # observeEvent(input$analyze,{
   #   shinyjs::hide("howto")
   # })
   
  # observe({
  #   if(input$body=="info"){
      # output$howto<-renderUI({
      #   box(width=12,
      #       includeMarkdown("www/Info.Rmd")
      #   )
      # })
      # 
      # observeEvent(input$analyze,{
      #   output$howto<-renderUI({NULL})
      # })
    #   }
    # else{
    #   output$howto<-renderUI({NULL})
    #     }
    # })
 
   ## Shinyalert
   observeEvent(input$analyze,{
     shinyalert("In Progress!", "Data analysis has started, wait until table and plots
                appear in the background", type="info",
                closeOnClickOutside = TRUE,
                closeOnEsc = TRUE,
                timer = 10000) # timer in miliseconds (10 sec)
   })
   
 
   
   ####======= Render Functions
   
   output$volcano_cntrst <- renderUI({
     if (!is.null(comparisons())) {
       df <- SummarizedExperiment::rowData(dep())
       cols <- grep("_significant$",colnames(df))
       selectizeInput("volcano_cntrst",
                      "Comparison",
                      choices = gsub("_significant", "", colnames(df)[cols]))
     }
   })
   
   output$downloadTable <- renderUI({
     if(!is.null(dep())){
     selectizeInput("dataset",
                    "Choose a dataset to save" ,
                    c("Results","Original_matrix",
                      "Imputed_matrix",
                      "Full_dataset"))
     }
    })
   
   output$downloadButton <- renderUI({
     if(!is.null(dep())){
     downloadButton('downloadData', 'Save')
     }
   })
   
   output$downloadZip <- renderUI({
     if(!is.null(dep())){
     downloadButton('downloadZip1', 'Download result plots')
     }
   })
    output$downloadreport <- renderUI({
      if(!is.null(dep())){
     downloadButton('downloadReport', 'Download Report')
      }
    })
   
    output$downloadPlots <- renderUI({
      if(!is.null(dep())){
      downloadButton('downloadPlots1', 'Download Plots')
      }
    })
    
    ## Read input files on shiny server
    ## NOTE: have to use reactive framework, otherwise throws out error
    maxquant_data<-eventReactive(input$analyze,{
      inFile<-input$file1
      if(is.null(inFile))
        return(NULL)
      read.table(inFile$datapath,
                 header = TRUE,
                 fill= TRUE, # to fill any missing data
                 sep = "\t"
      )
    })
    
    exp_design<-eventReactive(input$analyze,{
      inFile<-input$file2
      if (is.null(inFile))
        return(NULL)
      temp_df<-read.table(inFile$datapath,
                          header = TRUE,
                          sep="\t",
                          stringsAsFactors = FALSE)
      temp_df$label<-as.character(temp_df$label)
      return(temp_df)
    })    
    
  
### Reactive components
   processed_data<- reactive({
     if(grepl('+',maxquant_data()$Reverse)){
     filtered_data<-dplyr::filter(maxquant_data(),Reverse!="+")
     }
     else{filtered_data<-maxquant_data()}
     if(grepl('+',filtered_data$Potential.contaminant)){
       filtered_data<-dplyr::filter(filtered_data,Potential.contaminant!="+")
     }
     if(grepl('+',filtered_data$Only.identified.by.site)){
       filtered_data<-dplyr::filter(filtered_data,Only.identified.by.site!="+", Razor...unique.peptides>=2)
     }
     #else{filtered_data<-maxquant_data()}
     # filter<-dplyr::filter(maxquant_data(),Reverse!="+", Potential.contaminant!="+",
     #                       Only.identified.by.site!="+", Razor...unique.peptides>=2)
     data_unique<- DEP::make_unique(filtered_data,"Gene.names","Protein.IDs",delim=";")
     lfq_columns<-grep("LFQ.", colnames(data_unique))
     data_se<-DEP:::make_se(data_unique,lfq_columns,exp_design())
  
     # Check number of replicates
     if(max(exp_design()$replicate)<3){
       threshold<-0
     }
     else if (max(exp_design()$replicate)==3 | max(exp_design()$replicate)<6 ){
       threshold<-1
     }
     else {
       threshold<-2
     }
     
     filter_missval(data_se,thr = threshold)
   })
   
   unimputed_table<-reactive({
     temp<-assay(processed_data())
     temp1<-2^(temp)
     colnames(temp1)<-paste(colnames(temp1),"original_intensity",sep="_")
     temp1<-cbind(ProteinID=rownames(temp1),temp1) #temp1$ProteinID<-rownames(temp1)
     return(as.data.frame(temp1))
   })
   
   normalised_data<-reactive({
     normalize_vsn(processed_data())
   })
   
   imputed_data<-reactive({
     DEP::impute(processed_data(),input$imputation)
   })
   
   imputed_table<-reactive({
     temp<-assay(imputed_data())
     #tibble::rownames_to_column(temp,var = "ProteinID")
     temp1<-2^(temp)
     colnames(temp1)<-paste(colnames(temp1),"imputed_intensity",sep="_")
     temp1<-cbind(ProteinID=rownames(temp1),temp1) #temp1$ProteinID<-rownames(temp1)
     return(as.data.frame(temp1))
   })
   
   diff_all<-reactive({
     test_diff(imputed_data(),type = 'all')
   })

   dep<-reactive({
     if(input$fdr_correction=="BH"){
       diff_all<-test_limma(imputed_data(),type='all')
       add_rejections(diff_all,alpha = input$p, lfc= input$lfc)
     }
     else{
       diff_all<-test_diff(imputed_data(),type='all')
       add_rejections(diff_all,alpha = input$p, lfc= input$lfc)
     }
     
   })
   
   comparisons<-reactive ({
  temp<-capture.output(DEP::test_diff(imputed_data(),type='all'),type = "message")
    gsub(".*: ","",temp)
   ## Split conditions into character vector
    unlist(strsplit(temp,","))
   ## Remove leading and trailing spaces
      trimws(temp)
   })
   
   ## Select point on volcano plot
   # protein_graph_selected<- reactive({
   #   protein_row<-nearPoints(data_result(), input$protein_click,
   #                           maxpoints = 1)
   #  # as.character(protein_row$name)
   # })
   # 
   
   ## Results plot inputs
   
   ## PCA Plot
   pca_input<-eventReactive(input$analyze,{
     if (num_total()<=500){
       if(length(levels(as.factor(colData(dep())$replicate))) <= 6){
       DEP::plot_pca(dep(), n=num_total(), point_size = 4)
       }
       else{
         DEP::plot_pca(dep(), n=num_total(), point_size = 4, indicate = "condition") 
       }
     }
     else{
       if(length(levels(as.factor(colData(dep())$replicate))) <= 6){
       DEP::plot_pca(dep(), point_size = 4)
       }
       else{
         DEP::plot_pca(dep(), point_size = 4, indicate = "condition")
       }
     }
     
   })
   
   ### Heatmap Differentially expressed proteins
   heatmap_input<-eventReactive(input$analyze,{
     get_cluster_heatmap(dep(),
                         type="centered",kmeans = TRUE,
                         k=6, col_limit = 6,
                         indicate = c("condition", "replicate"))
   })
   
   ### Volcano Plot
    volcano_input <- reactive({
      if(!is.null(input$volcano_cntrst)) {
                    plot_volcano_new(dep(),
                    input$volcano_cntrst,
                    input$fontsize,
                    input$check_names,
                    input$p_adj)
    
      }
    })
    
    volcano_df<- reactive({
      if(!is.null(input$volcano_cntrst)) {
        get_volcano_df(dep(),
                         input$volcano_cntrst)
        
      }
    })

    
    volcano_input_selected<-reactive({
      if(!is.null(input$volcano_cntrst)){
        proteins_selected<-data_result()[c(input$contents_rows_selected),] ## get all rows selected
       ## convert contrast to x and padj to y
       diff_proteins <- grep(paste(input$volcano_cntrst, "_ratio", sep = ""),
                    colnames(proteins_selected))
        if(input$p_adj=="FALSE"){
       padj_proteins <- grep(paste(input$volcano_cntrst, "_p.val", sep = ""),
                                     colnames(proteins_selected))
        }
       else{
         padj_proteins <- grep(paste(input$volcano_cntrst, "_p.adj", sep = ""),
                               colnames(proteins_selected))
       }

       df_protein <- data.frame(x = proteins_selected[, diff_proteins],
                        y = -log10(as.numeric(proteins_selected[, padj_proteins])),#)#,
                        name = proteins_selected$name)

       p<-plot_volcano(dep(),
                    input$volcano_cntrst,
                    input$fontsize,
                    input$check_names,
                    input$p_adj)
       
       p + geom_point(data = df_protein, color = "maroon", size= 3) +
         ggrepel::geom_text_repel(data = df_protein,
                                  aes(label = name),
                                  size = 4,
                                  box.padding = unit(0.1, 'lines'),
                                  point.padding = unit(0.1, 'lines'),
                                  segment.size = 0.5)## use the dataframe to plot points

       }
    })
     
   ## QC Inputs
   norm_input <- reactive({
     plot_normalization(processed_data(),
                        normalised_data())
   })
   
   missval_input <- reactive({
     plot_missval(processed_data())
   })
   
   detect_input <- reactive({
     plot_detect(processed_data())
   })
   
   imputation_input <- reactive({
     plot_imputation(normalised_data(),
                     diff_all())
   })
   
   p_hist_input <- reactive({
     plot_p_hist(dep())
   })
   
   numbers_input <- reactive({
     plot_numbers(normalised_data())
   })
   
   coverage_input <- reactive({
     plot_coverage(normalised_data())
   })
   
   correlation_input<-reactive({
     plot_cor(dep())
   })
   
   cvs_input<-reactive({
     plot_cvs(dep())
   })
   
   num_total<-reactive({
     dep() %>%
       nrow()
   }) 

   
#### Interactive UI
   output$significantBox <- renderInfoBox({
     num_total <- dep() %>%
       nrow()
     num_signif <- dep() %>%
       .[SummarizedExperiment::rowData(.)$significant, ] %>%
       nrow()
     frac <- num_signif / num_total
     
     if(frac > 0.2) {
       info_box <- infoBox("Significant proteins",
                           paste0(num_signif,
                                  " out of ",
                                  num_total),
                           paste0("Too large fraction (",
                                  signif(frac * 100, digits = 3),
                                  "%) of proteins differentially expressed across all conditions"),
                           icon = icon("star-half-o", lib = "glyphicon"),
                           color = "orange",
                          # fill = TRUE,
                           width = 4)
     }
     if(frac == 0) {
       info_box <- infoBox("Significant proteins",
                           paste0(num_signif,
                                  " out of ",
                                  num_total),
                           "No proteins differentially expressed across all conditions",
                           icon = icon("star-o", lib = "glyphicon"),
                           color = "red",
                        #   fill = TRUE,
                           width = 4)
     }
     if(frac > 0 & frac <= 0.2) {
       info_box <- 		infoBox("Significant proteins",
                             paste0(num_signif,
                                    " out of ",
                                    num_total),
                             paste0(signif(frac * 100, digits = 3),
                                    "% of proteins differentially expressed across all conditions"),
                             icon = icon("star", lib = "glyphicon"),
                             color = "green",
                            # fill = TRUE,
                             width = 4)
     }
     info_box
   })

  ##### Get results dataframe from Summarizedexperiment object
    data_result<-reactive({
      get_results_proteins(dep())
      #get_results(dep())
    })
    
    
  #### Data table
  output$contents <- DT::renderDataTable({
    df<- data_result()
    return(df)
  },
  options = list(scrollX = TRUE)
  )
  
  ## Deselect all rows button
  proxy <- dataTableProxy("contents")
  
  observeEvent(input$clear,{
    proxy %>% selectRows(NULL)
  })
  
  observeEvent(input$original,{
    output$contents <- DT::renderDataTable({
      df<- data_result()
      return(df)
    },
    options = list(scrollX = TRUE)
    )
  })

   protein_name<- reactive({
     #protein_tmp<-nearPoints(volcano_df(), input$protein_click, maxpoints = 1)
     protein_tmp<-brushedPoints(volcano_df(), input$protein_brush, 
                                xvar = "diff", yvar = "p_values")
     protein_selected<-protein_tmp$name
     }) 
 #   observeEvent(input$protein_brush,{
 # output$protein_info<-renderPrint({
 # #  protein_selected()
 #   #nearPoints(rowData(dep()), input$protein_click, maxpoints = 1)
 #   brushedPoints(volcano_df(), input$protein_brush, 
 #               xvar = "diff", yvar = "p_values")
 #  # head(volcano_df())
 #   #input$protein_click
 #  # str(input$protein_hover)
 # })
 #  })
  
  ## Select rows dynamically
 observeEvent(input$protein_brush,{
    output$contents <- DT::renderDataTable({
      df<- data_result()[data_result()[["name"]] %in% protein_name(), ]
      return(df)
    },
    options = list(scrollX= TRUE)
    )
  })

 
  ## Render Result Plots
  output$pca_plot<-renderPlot({
    pca_input()
  })
  output$heatmap<-renderPlot({
   heatmap_input()
  })
 
  output$volcano <- renderPlot({
    if(is.null(input$contents_rows_selected)){
   volcano_input()
     }
    else if(!is.null(input$volcano_cntrst)){
      volcano_input_selected()
      } # else close
  })
 
 
  ### QC Outputs
  output$sample_corr <-renderPlot({
    correlation_input()
  })
  
  output$sample_cvs <- renderPlot({
    cvs_input()
  })
  
  output$norm <- renderPlot({
    norm_input()
  })
  
  output$missval <- renderPlot({
    missval_input()
  })
  
  output$detect <- renderPlot({
    detect_input()
  })
  
  output$imputation <- renderPlot({
    imputation_input()
  })
  
  output$p_hist <- renderPlot({
    p_hist_input()
  })
  
  output$numbers <- renderPlot({
    numbers_input()
  })
  
  output$coverage <- renderPlot({
    coverage_input()
  })
  
  
  ##### Download Functions
  datasetInput <- reactive({
    switch(input$dataset,
           "Results" = get_results(dep()),
           "Original_matrix"= unimputed_table(),
           # "significant_proteins" = get_results(dep()) %>%
           #   filter(significant) %>%
           #   select(-significant),
           "Imputed_matrix" = imputed_table(),
           "Full_dataset" = get_df_wide(dep()))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$dataset, ".csv", sep = "") }, ## use = instead of <-
    content = function(file) {
      write.table(datasetInput(),
                  file,
                  col.names = TRUE,
                  row.names = FALSE,
                  sep =",") }
  )
  
  ### === Cluster Download ==== ####
  
  individual_cluster <- reactive({
      cluster_number <- input$cluster_number
      cluster_all <- heatmap_input()
      data_result()[cluster_all[[cluster_number]],]
    })
  
  # output$text1 <- renderPrint({
  #   paste(individual_cluster())
  # })
  
  output$downloadCluster <- downloadHandler(
    filename = function() { paste("Cluster_info_",input$cluster_number, ".csv", sep = "") }, ## use = instead of <-
    content = function(file) {
      write.table(individual_cluster(),
                  file,
                  col.names = TRUE,
                  row.names = FALSE,
                  sep =",") }
  )
  
  output$downloadVolcano <- downloadHandler(
    filename = function() {
      paste0("Volcano_", input$volcano_cntrst, ".pdf")
    },
    content = function(file) {
      pdf(file)
      print(volcano_input_selected())
      dev.off()
    }
  )
  
 output$downloadZip1<-downloadHandler(
   # filename = function() {
   #   "Result_plots.pdf"
   # },
   # content = function(file) {
   #   pdf(file)
   #   print( pca_input() )
   #   print( heatmap_input() )
    # print( volcano_input() )

     # for(i in input$volcano_cntrst){
     #   print(plot_volcano(dep(),contrast = i,label_size = 2,add_names = T, adjusted=T))
     # }
    # print( age_plot() )
   #   dev.off()
   # }
 filename = function() {
   paste("output", "zip", sep=".")
 },
 content= function(fname){
   fs<-c()
   tmpdir<-tempdir()
   setwd(tempdir())
   for (i in c("pca_input","heatmap_input")){
     path<-paste0(i,".png",sep="")
     fs<- c(fs,path)
     png(paste0(i,".png",sep=""))
     print(i())
     dev.off()
   }
   print(fs)
   zip(zipfile = fname, files = fs)
 },
 contentType = "applications/zip"
 )
  
#####===== Download Report =====#####
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.docx",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "LFQ_report.Rmd")
      file.copy("LFQ_report.Rmd", tempReport, overwrite = TRUE)
      
      sig_proteins<-dep() %>%
        .[SummarizedExperiment::rowData(.)$significant, ] %>%
        nrow()
      
      tested_contrasts<- gsub("_p.adj", "", 
                              colnames(SummarizedExperiment::rowData(dep()))[grep("p.adj", colnames(SummarizedExperiment::rowData(dep())))])
      pg_width<- ncol(imputed_data()) / 2.5
      # Set up parameters to pass to Rmd document
      params <- list(data = processed_data,
                     alpha = input$p,
                     lfc = input$lfc,
                     #data_filter = imputed_data(),
                     num_signif= sig_proteins,
                     tested_contrasts= tested_contrasts,
                     pg_width = pg_width,
                     numbers_input= numbers_input,
                     pca_input = pca_input,
                     coverage_input= coverage_input,
                     correlation_input =correlation_input,
                     heatmap_input = heatmap_input,
                    # num_total = num_total,
                     #comparisons = comparisons,
                     dep = dep
                     )
      
      # Knit the document, passing in the `params` list
       rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
### ===== Download Plots ===== #####
  output$downloadPlots1 <- downloadHandler(
    filename = "Plots.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "Plots.Rmd")
      file.copy("Plots.Rmd", tempReport, overwrite = TRUE)
      
      tested_contrasts<- gsub("_p.adj", "", 
                              colnames(SummarizedExperiment::rowData(dep()))[grep("p.adj", 
                                                                                  colnames(SummarizedExperiment::rowData(dep())))])
      pg_width<- ncol(imputed_data()) / 2.5
      
      # Set up parameters to pass to Rmd document
      params <- list(tested_contrasts= tested_contrasts,
                     pg_width = pg_width,
                     numbers_input= numbers_input,
                     detect_input = detect_input,
                     imputation_input = imputation_input,
                     missval_input = missval_input,
                     p_hist_input = p_hist_input,
                     pca_input = pca_input,
                     coverage_input= coverage_input,
                     correlation_input =correlation_input,
                     heatmap_input = heatmap_input,
                     cvs_input= cvs_input,
                     dep = dep
      )
      
      # Knit the document, passing in the `params` list
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}
