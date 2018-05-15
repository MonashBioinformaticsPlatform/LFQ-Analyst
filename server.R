#Define server logic to read selected file ----
server <- function(input, output) {
  options(shiny.maxRequestSize=100*1024^2)  ## Set maximum upload size to 100MB
  ## Read input files in R
  ## NOTE: have to use reactive framework, otherwise throws out error
  ##
  # maxquant_data<-reactive({
  #   inFile <- input$file1
  #   if (is.null(inFile))
  #     return(NULL)
  #   read.table(inFile$datapath,
  #              header=TRUE,
  #              sep="\t")
  #   })
  
  ### Use of event reactive to use action button
  
  
  
  
  maxquant_data<-eventReactive(input$analyze,{
    inFile<-input$file1
    if(is.null(inFile))
      retun(NULL)
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
     selectizeInput("dataset",
                    "Choose a dataset to save" ,
                    c("Results","input_matrix",
                      "imputed_matrix",
                      "full_dataset"))
   })
   
   output$downloadButton <- renderUI({
     downloadButton('downloadData', 'Save')
   })
   
   # output$downloadZip <- renderUI({
   #   downloadButton('downloadZip1', 'Download result plots')
   # })
    output$downloadreport <- renderUI({
     downloadButton('downloadReport', 'Download report')
    })
   
    output$downloadPlots <- renderUI({
      downloadButton('downloadPlots1', 'Download Plots')
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
   
   normalised_data<-reactive({
     normalize_vsn(processed_data())
   })
   
   imputed_data<-reactive({
     DEP::impute(normalised_data(),input$imputation)
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
     diff_all<-test_diff(imputed_data(),type='all')
     add_rejections(diff_all,alpha = input$p, lfc= input$lfc)
   })
   
   comparisons<-reactive ({
  temp<-capture.output(DEP::test_diff(imputed_data(),type='all'),type = "message")
    gsub(".*: ","",temp)
   ## Split conditions into character vector
    unlist(strsplit(temp,","))
   ## Remove leading and trailing spaces
      trimws(temp)
   })
   
   volcano_input <- reactive({
     if(!is.null(input$volcano_cntrst)) {
       plot_volcano(dep(),
                    input$volcano_cntrst,
                    input$fontsize,
                    input$check_names,
                    input$p_adj)
     }
   })
   
   ## QC Inputs
   norm_input <- reactive({
     plot_normalization(processed_data(),
                        normalised_data())
   })
   
   missval_input <- reactive({
     plot_missval(normalised_data())
   })
   
   detect_input <- reactive({
     plot_detect(normalised_data())
   })
   
   imputation_input <- reactive({
     plot_imputation(normalised_data(),
                     diff_all())
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
                             width = 4)
     }
     info_box
   })
   
  #  lfq<-new.env()
  # # Result Table
  #    dep<-eventReactive(input$analyze,{
  #    LFQ_wrapper(maxquant_data(),exp_design())
  #   #attach("data/lfq_results.RData")
  #  })
  # load("data/lfq_results.RData", envir = lfq)
  #results<-reactiveFileReader(1000, "data/lfq_results.RData",load)
 # results<-reactiveFileReader(1000, "data/lfq_results.RData")
  # 
  # dep<-reactive({
  #   results[['dep']]
  # })
   ## Load RDATA file
   
   
   # data_result<-eventReactive(input$analyze,{
   #   #results_lfq()$data_result
   #   lfq.env()$data_result
   # })
   ## "()" is important
   
    # data_filter<-reactive({
    #   results_lfq()$data_filter
    # })
   
    # dep<-reactive({
    #   results_lfq()$dep
    # })
    # typeof(dep)
   
   
   # #### Frequency Plot
   # protein_frequency_input<-eventReactive(input$analyze,{
   #   plot_numbers(processed_data())
   # })

   ## PCA Plot
    pca_input<-eventReactive(input$analyze,{
      if (num_total()<=500){
        DEP::plot_pca(dep(), n=num_total())
      }
      else{
        DEP::plot_pca(dep())
      }
      
   })
    
    ### Heatmap Differentially expressed proteins
    heatmap_input<-eventReactive(input$analyze,{
      get_cluster_heatmap(dep(),
                        type="centered",kmeans = TRUE,
                        k=6, col_limit = 6,
                        indicate = c("condition", "replicate"))
    })
  #str(lfq_results)
    data_result<-reactive({
      get_results(dep())
    })
  #### Data table
  output$contents <- DT::renderDataTable({
    # req(input$file1)
    
    # df <- read.table(input$file1$datapath,
    #                header = TRUE,
    #                sep = "\t"
    #                )
   # df<- imputed_table()
    df<- data_result()
    return(df)
  },
  options = list(scrollX = TRUE)
  )
  
  ## Render Plots
  #
  # output$protein_frequency<-renderPlot({
  #   protein_frequency_input()
  #  })
  output$pca_plot<-renderPlot({
    pca_input()
  })
  output$heatmap<-renderPlot({
   heatmap_input()
  })
  output$volcano <- renderPlot({
    volcano_input()
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
           "input_matrix"=maxquant_data(),
           # "significant_proteins" = get_results(dep()) %>%
           #   filter(significant) %>%
           #   select(-significant),
           "imputed_matrix" = imputed_table(),
           "full_dataset" = get_df_wide(dep()))
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
  
  # output$downloadZip1<-downloadHandler(
  #   filename = function() {
  #     "Result_plots.pdf"
  #   },
  #   content = function(file) {
  #     pdf(file)
  #     print( pca_input() )
  #     print( heatmap_input() )
  #    # print( volcano_input() )
  # 
  #     # for(i in input$volcano_cntrst){
  #     #   print(plot_volcano(dep(),contrast = i,label_size = 2,add_names = T, adjusted=T))
  #     # }
  #    # print( age_plot() )
  #     dev.off()
  #   }
    # filename = function() {
    #   paste("output", "zip", sep=".")
    # },
    # content= function(fname){
    #   fs<-c()
    #   tmpdir<-tempdir()
    #   setwd(tempdir())
    #   for (i in c(pca_input,heatmap_input)){
    #     path<-paste0(deparse(substitute(i())),".pdf")
    #     fs<- c(fs,path)
    #     pdf(paste0(deparse(substitute(i())),".pdf",sep=""))
    #     print(i())
    #     dev.off()
    #   }
    #   print(fs)
    #   zip(zipfile = fname, files = fs)
    # },
    # contentType = "applications/zip"
 # )
  
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
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
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
                     pca_input = pca_input,
                     coverage_input= coverage_input,
                     correlation_input =correlation_input,
                     heatmap_input = heatmap_input,
                     cvs_input= cvs_input,
                     dep = dep
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}
