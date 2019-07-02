#Define server logic to read selected file ----
server <- function(input, output) {
  options(shiny.maxRequestSize=100*1024^2)  ## Set maximum upload size to 100MB
 # source("demo.R")
#  Show elements on clicking Start analysis button
   observeEvent(input$analyze ,{ 
     if(input$analyze==0){
       return()
     }
    shinyjs::show("downloadbox")
    })
   
   observeEvent(input$analyze ,{ 
       if(input$analyze==0 ){
         return()
       }
     shinyjs::show("results_tab")
   })
   
   observeEvent(input$analyze ,{ 
     if(input$analyze==0 ){
       return()
     }
     shinyjs::show("qc_tab")
   })
   
   observeEvent(input$analyze ,{ 
     if(input$analyze==0 ){
       return()
     }
     shinyjs::show("enrichment_tab")
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
   observeEvent(input$analyze ,{ 
     if(input$analyze==0 ){
       return()
     }
     
     shinyalert("In Progress!", "Data analysis has started, wait until table and plots
                appear on the screen", type="info",
                closeOnClickOutside = TRUE,
                closeOnEsc = TRUE,
                timer = 10000) # timer in miliseconds (10 sec)
   })
   
   observe({
   if (input$tabs_selected=="demo"){
     shinyalert("Demo results loading!...", "Wait until table and plots
                appear on the screen", type="info",
                closeOnClickOutside = TRUE,
                closeOnEsc = TRUE,
                timer = 6000)
   }
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
   
   ##comparisons
   output$contrast <- renderUI({
     if (!is.null(comparisons())) {
       df <- SummarizedExperiment::rowData(dep())
       cols <- grep("_significant$",colnames(df))
       selectizeInput("contrast",
                      "Comparison",
                      choices = gsub("_significant", "", colnames(df)[cols]))
     }
   })
   
   output$contrast_1 <- renderUI({
     if (!is.null(comparisons())) {
       df <- SummarizedExperiment::rowData(dep())
       cols <- grep("_significant$",colnames(df))
       selectizeInput("contrast",
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
    # observeEvent(input$analyze,{
    #   maxquant_data<-reactive({
    #     inFile<-input$file1
    #     if(is.null(inFile))
    #       return(NULL)
    #     read.table(inFile$datapath,
    #                header = TRUE,
    #                fill= TRUE, # to fill any missing data
    #                sep = "\t"
    #     )
    #   })
    # })
    
    ## make reactive elements
    maxquant_data_input<-reactive({NULL})
    exp_design_input<-reactive({NULL})
    exp_design_example<-reactive({NULL})
    maxquant_data_example<-reactive({NULL})
    
    maxquant_data_input<-eventReactive(input$analyze,{
      inFile<-input$file1
      if(is.null(inFile))
        return(NULL)
      temp_data<-read.table(inFile$datapath,
                 header = TRUE,
                 fill= TRUE, # to fill any missing data
                 sep = "\t"
      )
      validate(maxquant_input_test(temp_data))
      return(temp_data)
    })
   
    # observeEvent(input$analyze,{
    #   exp_design<-reactive({
    #     inFile<-input$file2
    #     if (is.null(inFile))
    #       return(NULL)
    #     temp_df<-read.table(inFile$datapath,
    #                         header = TRUE,
    #                         sep="\t",
    #                         stringsAsFactors = FALSE)
    #     exp_design_test(temp_df)
    #     temp_df$label<-as.character(temp_df$label)
    #     return(temp_df)
    #   })    
    # })
    exp_design_input<-eventReactive(input$analyze,{
      inFile<-input$file2
      if (is.null(inFile))
        return(NULL)
      temp_df<-read.table(inFile$datapath,
                          header = TRUE,
                          sep="\t",
                          stringsAsFactors = FALSE)
      exp_design_test(temp_df)
      temp_df$label<-as.character(temp_df$label)
      temp_df$condition<-trimws(temp_df$condition, which = "left")
      return(temp_df)
    })
   
    
### Load data from Rdata
  # observeEvent(input$load_data,{
      # example_data<-reactive({
      #   load("data/example_data.RData", envir = .GlobalEnv)
      # })
      # maxquant_data<-reactive({example_data[1]})
      # exp_design<-reactive({example_data[2]})
   #  env<-reactive({
   #    LoadToEnvironment("data/example_data.RData", env = globalenv())
   #  })
   # 
   #   observeEvent(input$load_data,{
   # # message(env()[["exp_design"]])
   #     maxquant_data_example<-reactive({
   #     env()[["maxquant_output"]]
   #   })
   #   })
   #   observeEvent(input$load_data,{
   #     exp_design_example<-reactive({
   #     env()[["exp_design"]]
   #   })
   #  })
   # }) ## leave this commented
   #  
   #  maxquant_data<-eventReactive(input$load_data,{
   #    env()[['maxquant_output']]
   #  })
   # 
   # exp_design<-eventReactive(input$load_data,{
   #    env()[['exp_design']]
   #  })
   
   
### Reactive components
   processed_data<- reactive({
     ## check which dataset
     if(!is.null (maxquant_data_input() )){
       maxquant_data <- reactive({maxquant_data_input()})
     }
     else if (!is.null (maxquant_data_example() )){
       maxquant_data <-reactive({maxquant_data_example()})
     }
     
     if(!is.null (exp_design_input() )){
       exp_design<-reactive({exp_design_input()})
     }
     
    if(!is.null (exp_design_example())){
       exp_design <-reactive({exp_design_example()})
     }
     
     message(exp_design())
     if(grepl('+',maxquant_data()$Reverse)){
     filtered_data<-dplyr::filter(maxquant_data(),Reverse!="+")
     }
     else{filtered_data<-maxquant_data()}
     if(grepl('+',filtered_data$Potential.contaminant)){
       filtered_data<-dplyr::filter(filtered_data,Potential.contaminant!="+")
     }
     if(grepl('+',filtered_data$Only.identified.by.site)){
       filtered_data<-dplyr::filter(filtered_data,Only.identified.by.site!="+", 
                                    Razor...unique.peptides>=2)
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
     temp1<-cbind(ProteinID=rownames(temp1),temp1) 
     #temp1$ProteinID<-rownames(temp1)
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
   pca_input<-eventReactive(input$analyze ,{ 
     if(input$analyze==0 ){
       return()
     }
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
	pca_label<-SummarizedExperiment::colData(dep())$replicate
       pca_plot<-DEP::plot_pca(dep(), point_size = 4, indicate = "condition")
        pca_plot + geom_point()
       pca_plot + ggrepel::geom_text_repel(aes(label=pca_label),
                                           size = 5,
                                           box.padding = unit(0.1, 'lines'),
                                           point.padding = unit(0.1, 'lines'),
                                           segment.size = 0.5)

 #        pca_plot<-DEP::plot_pca(dep(), point_size = 4, indicate = "condition")
#         pca_plot + ggrepel::geom_text_repel(aes(label=SummarizedExperiment::colData(dep())$replicate),
     #                                        size = 5,
  #                                           box.padding = unit(0.1, 'lines'),
   #                                          point.padding = unit(0.1, 'lines'),
    #                                         segment.size = 0.5)
       }
     }
     
   })
   
   ### Heatmap Differentially expressed proteins
   heatmap_input<-eventReactive(input$analyze ,{ 
     if(input$analyze==0 ){
       return()
     }
     get_cluster_heatmap(dep(),
                         type="centered",kmeans = TRUE,
                         k=6, col_limit = 6,
                         indicate = "condition"
                         )
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
       diff_proteins <- grep(paste(input$volcano_cntrst, "_log2", sep = ""),
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
                        name = proteins_selected$`Gene Name`)
       #print(df_protein)
       p<-plot_volcano_new(dep(),
                    input$volcano_cntrst,
                    input$fontsize,
                    input$check_names,
                    input$p_adj)
       
       p + geom_point(data = df_protein, aes(x, y), color = "maroon", size= 3) +
         ggrepel::geom_text_repel(data = df_protein,
                                  aes(x, y, label = name),
                                  size = 4,
                                  box.padding = unit(0.1, 'lines'),
                                  point.padding = unit(0.1, 'lines'),
                                  segment.size = 0.5)## use the dataframe to plot points

       }
    })
    
    protein_input<-reactive({ 
      
      protein_selected  <- data_result()[input$contents_rows_selected,1]
      
      if(length(levels(as.factor(colData(dep())$replicate))) <= 8){
        plot_protein(dep(), protein_selected, input$type)
      }
      else{
        protein_plot<-plot_protein(dep(), protein_selected, input$type)
        protein_plot + scale_color_brewer(palette = "Paired")
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
   
   ## Enrichment inputs
    
   go_input<-eventReactive(input$go_analysis,{
     withProgress(message = 'Gene ontology enrichment is in progress',
                  detail = 'Please wait for a while', value = 0, {
                    for (i in 1:15) {
                      incProgress(1/15)
                      Sys.sleep(0.25)
                    }
                  })
     
    if(!is.null(input$contrast)){
    go_results<- test_gsea(dep(), databases = input$go_database, contrasts = TRUE)
    plot_go<- plot_enrichment(go_results, number = 5, alpha = 0.05, contrasts =input$contrast,
    databases = input$go_database, nrow = 2, term_size = 8) + aes(stringr::str_wrap(Term, 60), log_odds) +
     xlab(NULL)
    go_list<-list("go_result"=go_results, "plot_go"=plot_go)
    return(go_list)
    }
   })

   pathway_input<-eventReactive(input$pathway_analysis,{
     progress_indicator("Pathway Analysis is running....")
     pathway_results<- test_gsea(dep(), databases=input$pathway_database, contrasts = TRUE)
     plot_pathway<-plot_enrichment(pathway_results, number = 5, alpha = 0.05, contrasts =input$contrast_1,
               databases=input$pathway_database, nrow = 3, term_size = 8) + aes(stringr::str_wrap(Term, 30), log_odds) +
       xlab(NULL)
     pathway_list<-list("pa_result"=pathway_results, "plot_pa"=plot_pathway)
     return(pathway_list)
     })

   
#### Interactive UI
   output$significantBox <- renderInfoBox({
     num_total <- dep() %>%
       nrow()
     num_signif <- dep() %>%
       .[SummarizedExperiment::rowData(.)$significant, ] %>%
       nrow()
     frac <- num_signif / num_total
     
       info_box <- 		infoBox("Significant proteins",
                             paste0(num_signif,
                                    " out of ",
                                    num_total),
                             paste0(signif(frac * 100, digits = 3),
                                    "% of proteins differentially expressed across all conditions"),
                             icon = icon("stats", lib = "glyphicon"),
                             color = "olive",
                            # fill = TRUE,
                             width = 4)
     
     return(info_box)
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
  options = list(scrollX = TRUE,
autoWidth=TRUE,
                columnDefs= list(list(width = '400px', targets = c(-1))))
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
    options = list(scrollX = TRUE,
autoWidth=TRUE,
                columnDefs= list(list(width = '400px', targets = c(-1))))
    )
  })

   protein_name_brush<- reactive({
     #protein_tmp<-nearPoints(volcano_df(), input$protein_click, maxpoints = 1)
     protein_tmp<-brushedPoints(volcano_df(), input$protein_brush, 
                                xvar = "diff", yvar = "p_values")
     protein_selected<-protein_tmp$name
     }) 
   protein_name_click<- reactive({
     protein_tmp<-nearPoints(volcano_df(), input$protein_click, maxpoints = 1)
    # protein_tmp<-brushedPoints(volcano_df(), input$protein_brush, 
                                #xvar = "diff", yvar = "p_values")
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
      df<- data_result()[data_result()[["Gene Name"]] %in% protein_name_brush(), ]
      return(df)
    },
    options = list(scrollX= TRUE)
    )
  })

 observeEvent(input$protein_click,{
   output$contents <- DT::renderDataTable({
     df<- data_result()[data_result()[["Gene Name"]] %in% protein_name_click(), ]
     return(df)
   },
   options = list(scrollX= TRUE,
autoWidth=TRUE,
                columnDefs= list(list(width = '400px', targets = c(-1))))
   )
 })
  ## Render Result Plots
  output$pca_plot<-renderPlot({
    pca_input()
  })
  output$heatmap<-renderPlot({
    withProgress(message = 'Heatmap rendering is in progress',
                 detail = 'Please wait for a while', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
   heatmap_input()
  })
 
  output$volcano <- renderPlot({
    withProgress(message = 'Volcano Plot calculations are in progress',
                 detail = 'Please wait for a while', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    if(is.null(input$contents_rows_selected)){
   volcano_input()
     }
    else if(!is.null(input$volcano_cntrst)){
      volcano_input_selected()
      } # else close
  })
  
  output$protein_plot<-renderPlot({
    if(!is.null(input$contents_rows_selected)){
    protein_input()
    }
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
  
  ## Enrichment Outputs
  output$go_enrichment<-renderPlot({
    go_input()$plot_go
  })
  
  output$pathway_enrichment<-renderPlot({
   pathway_input()$plot_pa
  })
  
  ##### Download Functions
  datasetInput <- reactive({
    switch(input$dataset,
           "Results" = get_results_proteins(dep()),
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
  
  
  ## Protein plot download
  output$downloadProtein <- downloadHandler(
    filename = function() {
      paste0(input$type,".pdf")
    },
    content = function(file) {
      pdf(file)
      print(protein_input())
      dev.off()
    }
  )
  
  ###### ==== DOWNLOAD GO TABLE ==== ####
  output$downloadGO <- downloadHandler(
    filename = function() { paste("GO_enrichment_",input$go_database, ".csv", sep = "") }, ## use = instead of <-
    content = function(file) {
      write.table(go_input()$go_result,
                  file,
                  col.names = TRUE,
                  row.names = FALSE,
                  sep =",") }
  )
  
  ###### ==== DOWNLOAD PATHWAY TABLE ==== ####
  output$downloadPA <- downloadHandler(
    filename = function() { paste("Pathway_enrichment_",input$pathway_database, ".csv", sep = "") }, ## use = instead of <-
    content = function(file) {
      write.table(pathway_input()$pa_result,
                  file,
                  col.names = TRUE,
                  row.names = FALSE,
                  sep =",") }
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
    filename = "LFQ-Analyst_report.pdf",
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
                     num_signif= sig_proteins,
                     pg_width = pg_width,
                     tested_contrasts= tested_contrasts,
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
  
  
### ===== Download Plots ===== #####
  # output$downloadPlots1 <- downloadHandler(
  #   filename = "Plots.pdf",
  #   content = function(file) {
  #     tempReport <- file.path(tempdir(), "Plots.Rmd")
  #     file.copy("templates/Plots.Rmd", tempReport, overwrite = TRUE)
  #     
  #     tested_contrasts<- gsub("_p.adj", "", 
  #                             colnames(SummarizedExperiment::rowData(dep()))[grep("p.adj", 
  #                                                                                 colnames(SummarizedExperiment::rowData(dep())))])
  #     pg_width<- ncol(imputed_data()) / 2.5
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(tested_contrasts= tested_contrasts,
  #                    pg_width = pg_width,
  #                    numbers_input= numbers_input,
  #                    detect_input = detect_input,
  #                    imputation_input = imputation_input,
  #                    missval_input = missval_input,
  #                    p_hist_input = p_hist_input,
  #                    pca_input = pca_input,
  #                    coverage_input= coverage_input,
  #                    correlation_input =correlation_input,
  #                    heatmap_input = heatmap_input,
  #                    cvs_input= cvs_input,
  #                    dep = dep
  #     )
  #     
  #     # Knit the document, passing in the `params` list
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
 
 
 #### Demo logic ========== #############
 
 ####======= Render Functions
 
 output$volcano_cntrst_dm <- renderUI({
   if (!is.null(comparisons_dm())) {
     df <- SummarizedExperiment::rowData(dep_dm())
     cols <- grep("_significant$",colnames(df))
     selectizeInput("volcano_cntrst_dm",
                    "Comparison",
                    choices = gsub("_significant", "", colnames(df)[cols]))
   }
 })
 
 ##comparisons
 output$contrast_dm <- renderUI({
   if (!is.null(comparisons_dm())) {
     df <- SummarizedExperiment::rowData(dep_dm())
     cols <- grep("_significant$",colnames(df))
     selectizeInput("contrast_dm",
                    "Comparison",
                    choices = gsub("_significant", "", colnames(df)[cols]))
   }
 })
 
 output$contrast_dm_1 <- renderUI({
   if (!is.null(comparisons_dm())) {
     df <- SummarizedExperiment::rowData(dep_dm())
     cols <- grep("_significant$",colnames(df))
     selectizeInput("contrast_dm_1",
                    "Comparison",
                    choices = gsub("_significant", "", colnames(df)[cols]))
   }
 })
 
 output$downloadTable_dm <- renderUI({
   if(!is.null(dep_dm())){
     selectizeInput("dataset_dm",
                    "Download data table" ,
                    c("Results",
                      "Full dataset"))
   }
 })
 
 output$downloadButton_dm <- renderUI({
   if(!is.null(dep_dm())){
     downloadButton('downloadData_dm', 'Save')
   }
 })
 
 
 output$downloadreport_dm <- renderUI({
   if(!is.null(dep_dm())){
     downloadButton('downloadReport_dm', 'Download Report')
   }
 })
 
 output$downloadPlots_dm <- renderUI({
   if(!is.null(dep_dm())){
     downloadButton('downloadPlots1_dm', 'Download Plots')
   }
 })
 
 env_dm<-reactive({
   LoadToEnvironment("data/demo_data.RData", env = globalenv())
 })
 
 
 
 dep_dm<-reactive({
   env_dm()[["dep"]]
 })
 
 
comparisons_dm<-reactive({
  comparisons<-gsub("_p.adj", "", colnames(SummarizedExperiment::rowData(dep_dm()))[grep("p.adj", 
                                                                                 colnames(SummarizedExperiment::rowData(dep_dm())))])
})
 ## Results plot inputs
 
 ## PCA Plot

pca_label_dm<-reactive({
 pca_lable<-levels(as.factor(colData(dep_dm())$replicate))
print(pca_label)
})

 pca_input_dm<-reactive({
   if (num_total_dm()<=500){
     if(length(levels(as.factor(colData(dep_dm())$replicate))) <= 6){
       DEP::plot_pca(dep_dm(), n=num_total_dm(), point_size = 4)
     }
     else{
       DEP::plot_pca(dep_dm(), n=num_total_dm(), point_size = 4, indicate = "condition") 
     }
   }
   else{
     if(length(levels(as.factor(colData(dep_dm())$replicate))) <= 6){
       DEP::plot_pca(dep_dm(), point_size = 4)
     }else{
	pca_label<-SummarizedExperiment::colData(dep_dm())$replicate
       pca_plot<-DEP::plot_pca(dep_dm(), point_size = 4, indicate = "condition")
	pca_plot + geom_point()
       pca_plot + ggrepel::geom_label_repel(aes(label=pca_label),
                                           size = 5,
                                           box.padding = unit(0.1, 'lines'),
                                           point.padding = unit(0.1, 'lines'),
                                           segment.size = 0.5)
	return(pca_plot)
     }
   }
   
 })
 
 ### Heatmap Differentially expressed proteins
 heatmap_input_dm<-reactive({ 
   get_cluster_heatmap(dep_dm(),
                       type="centered",kmeans = TRUE,
                       k=6, col_limit = 6,
                       indicate = "condition"
   )
 })
 
 ### Volcano Plot
 volcano_input_dm <- reactive({
   if(!is.null(input$volcano_cntrst_dm)) {
     plot_volcano_new(dep_dm(),
                      input$volcano_cntrst_dm,
                      input$fontsize_dm,
                      input$check_names_dm,
                      input$p_adj_dm)
     
   }
 })
 
 volcano_df_dm<- reactive({
   if(!is.null(input$volcano_cntrst_dm)) {
     get_volcano_df(dep_dm(),
                    input$volcano_cntrst_dm)
     
   }
 })
 
 
 volcano_input_selected_dm<-reactive({
   if(!is.null(input$volcano_cntrst_dm)){
     proteins_selected<-data_result_dm()[c(input$contents_dm_rows_selected),] ## get all rows selected
     ## convert contrast to x and padj to y
     diff_proteins <- grep(paste(input$volcano_cntrst_dm, "_log2", sep = ""),
                           colnames(proteins_selected))
     if(input$p_adj_dm=="FALSE"){
       padj_proteins <- grep(paste(input$volcano_cntrst_dm, "_p.val", sep = ""),
                             colnames(proteins_selected))
     }
     else{
       padj_proteins <- grep(paste(input$volcano_cntrst_dm, "_p.adj", sep = ""),
                             colnames(proteins_selected))
     }
     
     df_protein <- data.frame(x = proteins_selected[, diff_proteins],
                              y = -log10(as.numeric(proteins_selected[, padj_proteins])),#)#,
                              name = proteins_selected$`Gene Name`)
     #print(df_protein)
     p<-plot_volcano_new(dep_dm(),
                         input$volcano_cntrst_dm,
                         input$fontsize_dm,
                         input$check_names_dm,
                         input$p_adj_dm)
     
     p + geom_point(data = df_protein, aes(x, y), color = "maroon", size= 3) +
       ggrepel::geom_text_repel(data = df_protein,
                                aes(x, y, label = name),
                                size = 4,
                                box.padding = unit(0.1, 'lines'),
                                point.padding = unit(0.1, 'lines'),
                                segment.size = 0.5)## use the dataframe to plot points
     
   }
 })
 
 protein_input_dm<-reactive({ 
   
   protein_selected  <- data_result_dm()[input$contents_dm_rows_selected,1]
   
   #protein<-row_selected$name
   if(length(levels(as.factor(colData(dep_dm())$replicate))) <= 8){
   plot_protein(dep_dm(), protein_selected, input$type_dm)
   }
   else{
     protein_plot<-plot_protein(dep_dm(), protein_selected, input$type_dm)
     protein_plot + scale_color_brewer(palette = "Paired")
     }
   
 })
 
 ## Get processed data
 
 processed_data_dm<-reactive({
   env_dm()[["data_filter"]]
 })
 normalised_data_dm<-reactive({
   DEP::normalize_vsn(processed_data_dm())
 })
	
	
imputed_data_dm<-reactive({
   DEP::impute(processed_data_dm(),input$imputation)
 })
 
 
 diff_all_dm<-reactive({
   test_diff(imputed_data_dm(),type = 'all')
 })
	
 ## QC Inputs
 norm_input_dm <- reactive({
   plot_normalization(processed_data_dm(),
                      normalised_data_dm())
 })
 
 missval_input_dm <- reactive({
   plot_missval(processed_data_dm())
 })
 
 detect_input_dm <- reactive({
   plot_detect(processed_data_dm())
 })
 
 imputation_input_dm <- reactive({
   plot_imputation(normalised_data_dm(),
                   diff_all_dm())
 })
 
 p_hist_input_dm <- reactive({
   plot_p_hist(dep_dm())
 })
 
 numbers_input_dm <- reactive({
   plot_numbers(normalised_data_dm())
 })
 
 coverage_input_dm <- reactive({
   plot_coverage(normalised_data_dm())
 })
 
 correlation_input_dm<-reactive({
   plot_cor(dep_dm())
 })
 
 cvs_input_dm<-reactive({
   plot_cvs(dep_dm())
 })
 
 num_total_dm<-reactive({
   dep_dm() %>%
     nrow()
 }) 
 
 ## Enrichment inputs
 
 go_input_dm<-eventReactive(input$go_analysis_dm,{
   withProgress(message = 'Gene ontology enrichment is in progress',
                detail = 'Please wait for a while', value = 0, {
                  for (i in 1:15) {
                    incProgress(1/15)
                    Sys.sleep(0.25)
                  }
                })
   
   if(!is.null(input$contrast_dm)){
     go_results<- test_gsea(dep_dm(), databases = input$go_database_dm, contrasts = TRUE)
     plot_go<- plot_enrichment(go_results, number = 5, alpha = 0.05, contrasts =input$contrast_dm,
                               databases = input$go_database_dm, nrow = 2, term_size = 8) + aes(stringr::str_wrap(Term, 60), log_odds) +
       xlab(NULL)
     go_list<-list("go_result"=go_results, "plot_go"=plot_go)
     return(go_list)
   }
 })
 
 pathway_input_dm<-eventReactive(input$pathway_analysis_dm,{
   withProgress(message = 'Pathway enrichment is in progress',
                detail = 'Please wait for a while', value = 0, {
                  for (i in 1:15) {
                    incProgress(1/15)
                    Sys.sleep(0.25)
                  }
                })
   
   pathway_results<- test_gsea(dep_dm(), databases=input$pathway_database_dm, contrasts = TRUE)
   plot_pathway<-plot_enrichment(pathway_results, number = 5, alpha = 0.05, contrasts =input$contrast_dm_1,
                                 databases=input$pathway_database_dm, nrow = 3, term_size = 8) + aes(stringr::str_wrap(Term, 30), log_odds) +
     xlab(NULL)
   pathway_list<-list("pa_result"=pathway_results, "plot_pa"=plot_pathway)
   return(pathway_list)
   
 })
 
 
 #### Interactive UI
 output$significantBox_dm <- renderInfoBox({
   num_total <- dep_dm() %>%
     nrow()
   num_signif <- dep_dm() %>%
     .[SummarizedExperiment::rowData(.)$significant, ] %>%
     nrow()
   frac <- num_signif / num_total
     
     info_box <- 		infoBox("Significant proteins",
                           paste0(num_signif,
                                  " out of ",
                                  num_total),
                           paste0(signif(frac * 100, digits = 3),
                                  "% of proteins differentially expressed across all conditions"),
                           icon = icon("stats", lib = "glyphicon"),
                           color = "olive",
                           # fill = TRUE,
                           width = 4)
     
     return(info_box)
   })
   
 
 ##### Get results dataframe from Summarizedexperiment object
 data_result_dm<-reactive({
   get_results_proteins(dep_dm())
   #get_results(dep())
 })
 
 
 #### Data table
 output$contents_dm <- DT::renderDataTable({
   df<- data_result_dm()
   return(df)
 },
 options = list(scrollX = TRUE,
	autoWidth=TRUE,
                columnDefs= list(list(width = '400px', targets = c(-1))))
 )
 
 ## Deselect all rows button
 proxy <- dataTableProxy("contents_dm")
 
 observeEvent(input$clear_dm,{
   proxy %>% selectRows(NULL)
 })
 
 observeEvent(input$original_dm,{
   output$contents_dm <- DT::renderDataTable({
     df<- data_result_dm()
     return(df)
   },
   options = list(scrollX = TRUE,
	autoWidth=TRUE,
                columnDefs= list(list(width = '400px', targets = c(-1))))
   )
 })
 
 protein_name_brush_dm<- reactive({
   #protein_tmp<-nearPoints(volcano_df(), input$protein_click, maxpoints = 1)
   protein_tmp<-brushedPoints(volcano_df_dm(), input$protein_brush_dm, 
                              xvar = "diff", yvar = "p_values")
   protein_selected<-protein_tmp$name
 }) 
 protein_name_click_dm<- reactive({
   protein_tmp<-nearPoints(volcano_df_dm(), input$protein_click_dm, maxpoints = 1)
   # protein_tmp<-brushedPoints(volcano_df(), input$protein_brush, 
   #xvar = "diff", yvar = "p_values")
   protein_selected<-protein_tmp$name
 }) 
 
 
 ## Select rows dynamically
 observeEvent(input$protein_brush_dm,{
   output$contents_dm <- DT::renderDataTable({
     df<- data_result_dm()[data_result_dm()[["Gene Name"]] %in% protein_name_brush_dm(), ]
     return(df)
   },
   options = list(scrollX= TRUE,
autoWidth=TRUE,
                columnDefs= list(list(width = '400px', targets = c(-1))))
   )
 })
 
 observeEvent(input$protein_click_dm,{
   output$contents_dm <- DT::renderDataTable({
     df<- data_result_dm()[data_result_dm()[["Gene Name"]] %in% protein_name_click_dm(), ]
     return(df)
   },
   options = list(scrollX= TRUE)
   )
 })
 ## Render Result Plots
 output$pca_plot_dm<-renderPlot({
   pca_input_dm()
 })
 output$heatmap_dm<-renderPlot({
   withProgress(message = 'Heatmap rendering is in progress',
                detail = 'Please wait for a while', value = 0, {
                  for (i in 1:15) {
                    incProgress(1/15)
                    Sys.sleep(0.25)
                  }
                })
   heatmap_input_dm()
 })
 
 output$volcano_dm <- renderPlot({
   withProgress(message = 'Volcano Plot calculations are in progress',
                detail = 'Please wait for a while', value = 0, {
                  for (i in 1:15) {
                    incProgress(1/15)
                    Sys.sleep(0.25)
                  }
                })
   if(is.null(input$contents_dm_rows_selected)){
     volcano_input_dm()
   }
   else if(!is.null(input$volcano_cntrst_dm)){
     volcano_input_selected_dm()
   } # else close
 })
 
 output$protein_plot_dm<-renderPlot({
   if(!is.null(input$contents_dm_rows_selected)){
     protein_input_dm()
   }
 })
 
 
 ### QC Outputs
 output$sample_corr_dm <-renderPlot({
   correlation_input_dm()
 })
 
 output$sample_cvs_dm <- renderPlot({
   cvs_input_dm()
 })
 
 output$norm_dm <- renderPlot({
   norm_input_dm()
 })
 
 output$missval_dm <- renderPlot({
   missval_input_dm()
 })
 
 output$detect_dm <- renderPlot({
   detect_input_dm()
 })
 
 output$imputation_dm <- renderPlot({
   imputation_input_dm()
 })
 
 output$p_hist <- renderPlot({
   p_hist_input_dm()
 })
 
 output$numbers_dm <- renderPlot({
   numbers_input_dm()
 })
 
 output$coverage_dm <- renderPlot({
   coverage_input_dm()
 })
 
 ## Enrichment Outputs
 output$go_enrichment_dm<-renderPlot({
   go_input_dm()$plot_go
 })
 
 output$pathway_enrichment_dm<-renderPlot({
   pathway_input_dm()$plot_pa
 })
 
 ##### Download Functions
 datasetInput_dm <- reactive({
   switch(input$dataset_dm,
          "Results" = get_results_proteins(dep_dm()),
          "Full dataset" = get_df_wide(dep_dm()))
 })
 
 output$downloadData_dm <- downloadHandler(
   filename = function() { paste(input$dataset_dm, ".csv", sep = "") }, ## use = instead of <-
   content = function(file) {
     write.table(datasetInput_dm(),
                 file,
                 col.names = TRUE,
                 row.names = FALSE,
                 sep =",") }
 )
 
 ### === Cluster Download ==== ####
 
 individual_cluster_dm <- reactive({
   cluster_number <- input$cluster_number_dm
   cluster_all <- heatmap_input_dm()
   data_result_dm()[cluster_all[[cluster_number]],]
 })
 
 
 
 output$downloadCluster_dm <- downloadHandler(
   filename = function() { paste("Cluster_info_",input$cluster_number_dm, ".csv", sep = "") }, ## use = instead of <-
   content = function(file) {
     write.table(individual_cluster_dm(),
                 file,
                 col.names = TRUE,
                 row.names = FALSE,
                 sep =",") }
 )
 
 output$downloadVolcano_dm <- downloadHandler(
   filename = function() {
     paste0("Volcano_", input$volcano_cntrst_dm, ".pdf")
   },
   content = function(file) {
     pdf(file)
     print(volcano_input_selected_dm())
     dev.off()
   }
 )
 
 
 ## Protein plot download
 output$downloadProtein_dm <- downloadHandler(
   filename = function() {
     paste0(input$type_dm,".pdf")
   },
   content = function(file) {
     pdf(file)
     print(protein_input_dm())
     dev.off()
   }
 )
 
 ###### ==== DOWNLOAD GO TABLE ==== ####
 output$downloadGO_dm <- downloadHandler(
   filename = function() { paste("GO_enrichment_",input$go_database_dm, ".csv", sep = "") }, ## use = instead of <-
   content = function(file) {
     write.table(go_input_dm()$go_result,
                 file,
                 col.names = TRUE,
                 row.names = FALSE,
                 sep =",") }
 )
 
 ###### ==== DOWNLOAD PATHWAY TABLE ==== ####
 output$downloadPA_dm <- downloadHandler(
   filename = function() { paste("Pathway_enrichment_",input$pathway_database_dm, ".csv", sep = "") }, ## use = instead of <-
   content = function(file) {
     write.table(pathway_input_dm()$pa_result,
                 file,
                 col.names = TRUE,
                 row.names = FALSE,
                 sep =",") }
 )
 
 
 
 #####===== Download Report =====#####
 output$downloadReport_dm <- downloadHandler(
   
   filename = "LFQ-Analyst_report.pdf",
   content = function(file) {
     file.copy("www/LFQ-Analyst_report.pdf",file)
     # tempReport <- file.path(tempdir(), "LFQ_report.Rmd")
     # file.copy("./templates/LFQ_report.Rmd", tempReport, overwrite = TRUE)
     # 
     # sig_proteins_dm<-dep_dm() %>%
     #   .[SummarizedExperiment::rowData(.)$significant, ] %>%
     #   nrow()
     # 
     # tested_contrasts_dm<- gsub("_p.adj", "", 
     #                            colnames(SummarizedExperiment::rowData(dep()))[grep("p.adj", colnames(SummarizedExperiment::rowData(dep_dm())))])
     # pg_width_dm<- ncol(processed_data_dm()) / 2.5
     # # Set up parameters to pass to Rmd document
     # params <- list(data = processed_data_dm,
     #                alpha = 0.05,
     #                lfc = 1,
     #                num_signif= sig_proteins_dm,
     #                tested_contrasts= tested_contrasts_dm,
     #                pg_width = pg_width_dm,
     #                numbers_input= numbers_input_dm,
     #                pca_input = pca_input_dm,
     #                coverage_input= coverage_input_dm,
     #                correlation_input =correlation_input_dm,
     #                heatmap_input = heatmap_input_dm,
     #                dep = dep_dm
     # )
     # 
     # # Knit the document, passing in the `params` list
     # rmarkdown::render(tempReport, output_file = file,
     #                   params = params,
     #                   envir = new.env(parent = globalenv())
     # )
   }
 )
 
 
 ### ===== Download Plots ===== #####
 # output$downloadPlots1_dm <- downloadHandler(
 #   filename = "Plots.pdf",
 #   content = function(file) {
 #     tempReport <- file.path(tempdir(), "Plots.Rmd")
 #     file.copy("templates/Plots.Rmd", tempReport, overwrite = TRUE)
 #     
 #     tested_contrasts_dm<- gsub("_p.adj", "", 
 #                                colnames(SummarizedExperiment::rowData(dep_dm()))[grep("p.adj", 
 #                                                                                         colnames(SummarizedExperiment::rowData(dep_dm())))])
 #     pg_width_dm<- ncol(processed_data_dm()) / 2.5
 #     
 #     # Set up parameters to pass to Rmd document
 #     params <- list(tested_contrasts= tested_contrasts_dm,
 #                    pg_width = pg_width_dm,
 #                    numbers_input= numbers_input_dm,
 #                    detect_input = detect_input_dm,
 #                    imputation_input = imputation_input_dm,
 #                    missval_input = missval_input_dm,
 #                    p_hist_input = p_hist_input_dm,
 #                    pca_input = pca_input_dm,
 #                    coverage_input= coverage_input_dm,
 #                    correlation_input =correlation_input_dm,
 #                    heatmap_input = heatmap_input_dm,
 #                    cvs_input= cvs_input_dm,
 #                    dep = dep_dm
 #     )
 #     
 #     # Knit the document, passing in the `params` list
 #     rmarkdown::render(tempReport, output_file = file,
 #                       params = params,
 #                       envir = new.env(parent = globalenv())
 #     )
 #   }
 # ) ## Download plot close
 
 
  
}
