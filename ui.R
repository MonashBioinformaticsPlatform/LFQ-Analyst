# Define UI for data upload app ----
ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title = "LFQ Analysis"),
                   # disable = TRUE),# Disable title bar
    dashboardSidebar(
      useShinyalert(),
      sidebarMenu(
       # id="tabs",
        menuItem("Analysis",  tabName="analysis", icon=icon("flask"),
        menuItem("Input Files", tabName="file", icon=icon("file"), #selected = TRUE,
                 fileInput('file1',
                           'Upload MaxQuant ProteinGroups.txt',
                           accept=c('text/csv',
                                    'text/comma-separated-values,text/plain',
                                    '.csv')),
                 fileInput('file2',
                           'Upload Experimental Design Matrix',
                           accept=c('text/csv',
                                    'text/comma-separated-values,text/plain',
                                    '.csv'))#,
                 # radioButtons("anno",
                 #              "Sample Information",
                 #              choices = list("Parse from MaxQuant file" = "columns",
                 #                             "Use Experimental Design information" = "expdesign"),
                 #              selected = "expdesign")
        ),
        menuItemOutput("columns"),
        tags$hr(),
        menuItem("Advanced Options",tabName="advanced", icon = icon("cogs"), 
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
                
                 radioButtons("fdr_correction",
                              "Type of FDR correction",
                              choices =  c("Benjamini Hocheberg"="BH",
                                              "t-statistics-based"="fdrtool"
                                              ), selected= "BH")
        ),
       
        tags$hr(),
        actionButton("analyze", "Start Analysis")),
        tags$hr(),
        menuItem('Information', icon=icon("question"), tabName = "info"),
        tags$hr()
      )
    ), # sidebar close
    dashboardBody(
      useShinyjs(), #imp to use shinyjs functions
      
     #  Add logo to the body
    #  tags$img(src="mbpf_logo.jpg",height=50, align="right"),
    
    ## Add tabItems
    id="body",
    # tabItems(
    #   tabItem(tabName = "analysis",
        shinyjs::hidden(div(id="downloadbox",
        fluidRow(
                          box(
                          column(6,uiOutput("downloadTable"),offset = 1), 
                          column(4,uiOutput("downloadButton")), # make the button on same line
                          width = 4),
                          
        infoBoxOutput("significantBox",width = 4),
      box(
        column(5,uiOutput("downloadreport"), offset = 1), # offset for dist between buttons
        #tags$br(),
       column(5,uiOutput('downloadPlots')),
        width = 4
      )
          ))), #close div and first row 
    
    # align save button
    tags$style(type='text/css', "#downloadButton { width:100%; margin-top: 25px;}"), 
    tags$style(type='text/css', "#downloadreport { width:100%; margin-top: 25px; margin-bottom: 25px;}"),
    tags$style(type='text/css', "#downloadPlots { width:100%; margin-top: 25px;}"),
    
      tags$br(), # Blank lines
      tags$br(),
    
  ## Data table and result plots box
      fluidRow(
        shinyjs::hidden(div(id="results_tab",
        box(
          title = "LFQ Results Table",
        DT::dataTableOutput("contents"),
        actionButton("clear", "Deselect Rows"),
        width = 6,
        status = "success",
       #color=""
        solidHeader = TRUE
        ),
       # column(
       box(
          width= 6,
          collapsible = TRUE,
          #status="primary",
          #solidHeader=TRUE,
          tabBox(
            title = "Result Plots",
            width = 12,
            tabPanel(title = "PCA Plot",
                     plotOutput("pca_plot"), height=600),
            tabPanel(title= "Heatmap",
                     fluidRow(
                     plotOutput("heatmap", height = 600)
                     ),
                     fluidRow(
                       box(numericInput("cluster_number",
                                    "Cluster to download (1-6)",
                                    min=1, max=6, value = 1), width = 6),
                    box(downloadButton('downloadCluster',"Save Cluster"),width = 3)
                       )
                     ),
            tabPanel(title = "Volcano plot",
                     fluidRow(
                       box(uiOutput("volcano_cntrst"), width = 5),
                       box(numericInput("fontsize",
                                        "Font size",
                                        min = 0, max = 8, value = 4),
                           width = 3),
                       box(checkboxInput("check_names",
                                         "Display names",
                                         value = FALSE),
                           checkboxInput("p_adj",
                                         "Adjusted p values",
                                         value = FALSE),
                           width = 4),
                       tags$p("Select protein from LFQ Results Table to show on plot") #Add text line
                      ),
                     
                     fluidRow(
                       plotOutput("volcano", height = 600,
                                  hover = "protein_hover"),
                                  #),# click = "protein_click"),
                       downloadButton('downloadVolcano', 'Save Highlighted Plot')
                     #)),
                     ), 
            verbatimTextOutput("protein_info"))
            
          )
        ) # box or column end
      ))),
  
      ## QC Box
      fluidRow(
        shinyjs::hidden(div(id="qc_tab",
        column(
          width=6,
        tabBox(title = "QC Plots", width = 12,
                                 tabPanel(title="Sample Correlation",
                                           plotOutput("sample_corr", height = 600)
                                   ),
                                  tabPanel(title= "Sample CVs",
                                           plotOutput("sample_cvs", height = 600)
                                           ),
                                  tabPanel(title = "Protein Numbers",
                                           plotOutput("numbers", height = 600)
                                  ),
                                  
                                  tabPanel(title = "Sample coverage",
                                           plotOutput("coverage", height = 600)
                                  ),
                                  tabPanel(title = "Normalization",
                                           plotOutput("norm", height = 600)
                                  ),
                                  tabPanel(title = "Missing values - Quant",
                                           plotOutput("detect", height = 600)
                                  ),
                                  tabPanel(title = "Missing values - Heatmap",
                                           plotOutput("missval", height = 600)
                                  ),
                                  tabPanel(title = "Imputation",
                                           plotOutput("imputation", height = 600)
                                  ),
                                 tabPanel(title = "p-value Histogram",
                                          plotOutput("p_hist", height = 600)
                                 )
                           ) # Tab box close
      )
      ))), # fluidrow qc close
  #  )#, analysis tab close
  tabItem(tabName = "info",
           fluidRow( 
           # uiOutput("howto")
          includeMarkdown("www/Info.Rmd")
          # ))
           )
  )# analysis tab close
  # tabItems(
  #  ) Tab items close
         
  #)# info tab lose
  #   )#tabitems close
  ) # Dasbboardbody close
    
  ) #Dashboard page close
) #Shiny U Close
