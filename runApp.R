# function to run LFQ-Analyst with file path as arguments

LFQAnalyst <- function(protein_path=NULL, exp_path=NULL){
  source("global.R")
  source("ui.R")
  source("server.R")
  
  lfq_ui <- ui()
  # lfq_server <- server(input, output, session,protein_path = protein_path, exp_path = exp_path)
  lfq_server <- function(input, output, session) {
    server(input, output, session, protein_path = protein_path, exp_path = exp_path)
  }
  
  shinyApp(ui = lfq_ui, server = lfq_server)
}
