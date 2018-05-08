# Define UI for data upload app ----
ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title = "LFQ Analysis"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Input Files", selected = TRUE,
                 fileInput('file1',
                           'Upload MaxQuant ProteinGroups.txt',
                           accept=c('text/csv',
                                    'text/comma-separated-values,text/plain',
                                    '.csv')),
                 fileInput('file2',
                           'Upload Experimental Design Matrix',
                           accept=c('text/csv',
                                    'text/comma-separated-values,text/plain',
                                    '.csv')),
                 radioButtons("anno",
                              "Sample Information",
                              choices = list("Parse from MaxQuant file" = "columns",
                                             "Use Experimental Design information" = "expdesign"),
                              selected = "expdesign")
        ),
        menuItemOutput("columns"),
        tags$hr(),
        menuItem("Advanced Options",
                 numericInput("p", 
                              "Adjusted p-value cutoff",
                              min = 0.0001, max = 0.1, value = 0.05),
                 numericInput("lfc",
                              "Log2 fold change cutoff",
                              min = 0, max = 10, value = 1),
                 radioButtons("imputation",
                              "Imputation type",
                              choices = c("Perseus-type"="man", MSnbase::imputeMethods())[1:9],
                              selected = "man"),
                 p(a("Imputation method information",
                     href = "https://www.rdocumentation.org/packages/MSnbase/versions/1.20.7/topics/impute-methods",
                     target="_blank"))
        ),
       
        tags$hr(),
        actionButton("analyze", "Start Analysis"),
        tags$hr()
      )
    ),
    dashboardBody(
     #  Add logo to the body
#      tags$img(src="mbpf_logo.jpg",height=50, align="right"),
      infoBoxOutput("significantBox",width = 7),
      uiOutput("downloadTable"),
      uiOutput("downloadButton"),
      tags$br(),
     uiOutput("downloadreport"),
      tags$br(),
      fluidRow(
        box(
          title = "LFQ Results Table",
        DT::dataTableOutput("contents"),
        width = 6
        ),
        column(
          width= 6,
          tabBox(
            title = "Result Plots",
            width = 12,
            # tabPanel(title = "Protein Frequency",
            #          plotOutput("protein_frequency")),
            #downloadButton('downloadPlot', 'Save plot')),
            tabPanel(title = "PCA Plot",
                     plotOutput("pca_plot")),
            tabPanel(title= "Heatmap",
                     plotOutput("heatmap")),
            tabPanel(title = "Volcano plot",
                     fluidRow(
                       box(uiOutput("volcano_cntrst"), width = 6),
                       box(numericInput("fontsize",
                                        "Font size",
                                        min = 0, max = 8, value = 4),
                           width = 3),
                       box(checkboxInput("check_names",
                                         "Display names",
                                         value = TRUE),
                           checkboxInput("p_adj",
                                         "Adjusted p values",
                                         value = FALSE),
                           width = 3)
                     ),
                     fluidRow(
                       plotOutput("volcano", height = 600)
                     ))
          )
        )
      ),
      ## QC Box
      fluidRow(
        column(
          width=7,
        tabBox(title = "QC Plots", width = 12,
                                 tabPanel(title="Sample Correlation",
                                           plotOutput("sample_corr", height = 600)
                                   ),
                                  tabPanel(title= "Sample CVs",
                                           plotOutput("sample_cvs", height = 600)
                                           ),
                                  tabPanel(title = "Protein Numbers",
                                           plotOutput("numbers", height = 600)
                                        #   downloadButton('downloadNumbers', 'Save')
                                  ),
                                  
                                  tabPanel(title = "Sample coverage",
                                           plotOutput("coverage", height = 600)
                                          # downloadButton('downloadCoverage', 'Save')
                                  ),
                                  tabPanel(title = "Normalization",
                                           plotOutput("norm", height = 600)
                                      #     downloadButton('downloadNorm', 'Save')
                                  ),
                                  tabPanel(title = "Missing values - Quant",
                                           plotOutput("detect", height = 600)
                                       #    downloadButton('downloadDetect', 'Save')
                                  ),
                                  tabPanel(title = "Missing values - Heatmap",
                                           plotOutput("missval", height = 600)
                                      #     downloadButton('downloadMissval', 'Save')
                                  ),
                                  tabPanel(title = "Imputation",
                                           plotOutput("imputation", height = 600)
                                      #     downloadButton('downloadImputation', 'Save')
                                  )
                           )
      )
      )
    )
  #     fluidRow(
  #       box(numericInput("p",
  #                        "adj. P value",
  #                        min = 0.0001, max = 0.1, value = 0.05),
  #           width = 2),
  #       box(numericInput("lfc",
  #                        "Log2 fold change",
  #                        min = 0, max = 10, value = 1),
  #           width = 2),
  #       infoBoxOutput("significantBox"),
  #       box(radioButtons("pres",
  #                        "Data presentation",
  #                        c("contrast", "centered"),
  #                        selected = "contrast"),
  #           width = 2),
  #       box(radioButtons("contrasts",
  #                        "Contrasts",
  #                        c("control", "all"),
  #                        selected = "control"),
  #           width = 2)
  #     ),
  #     fluidRow(
  #       column(width = 7,
  #              box(title = "Top Table",
  #                  box(uiOutput("select"), width = 6),
  #                  box(uiOutput("exclude"), width = 6),
  #                  DT::dataTableOutput("table"), width = 12)
  #       ),
  #       column(width = 5,
  #              tabBox(title = "Result Plots", width = 12,
  #                     tabPanel(title = "Selected Protein",
  #                              plotOutput("selected_plot"),
  #                              downloadButton('downloadPlot', 'Save plot')),
  #                     tabPanel(title = "Heatmap",
  #                              fluidRow(
  #                                box(numericInput("k",
  #                                                 "Kmeans clusters",
  #                                                 min = 0, max = 15, value = 7),
  #                                    width = 4),
  #                                box(numericInput("limit",
  #                                                 "Color limit (log2)",
  #                                                 min = 0, max = 16, value = 6),
  #                                    width = 4),
  #                                box(numericInput("size",
  #                                                 "Heatmap size (4-30)",
  #                                                 min = 4, max = 30, value = 10),
  #                                    width = 4)
  #                              ),
  #                              fluidRow(
  #                                uiOutput("plot"),
  #                                downloadButton('downloadHeatmap', 'Save heatmap'))
  #                     ),
  #                     tabPanel(title = "Volcano plot",
  #                              fluidRow(
  #                                box(uiOutput("volcano_cntrst"), width = 6),
  #                                box(numericInput("fontsize",
  #                                                 "Font size",
  #                                                 min = 0, max = 8, value = 4),
  #                                    width = 3),
  #                                box(checkboxInput("check_names",
  #                                                  "Display names",
  #                                                  value = TRUE),
  #                                    checkboxInput("p_adj",
  #                                                  "Adjusted p values",
  #                                                  value = FALSE),
  #                                    width = 3)
  #                              ),
  #                              fluidRow(
  #                                plotOutput("volcano", height = 600),
  #                                downloadButton('downloadVolcano', 'Save volcano')
  #                              )
  #                     )
  #              ),
  #              
  #       )
  #     )
  #   )

 ####===== Download report ===== ####
  


   )
)
