### functions to deal with demo data

LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

#env<-LoadToEnvironment("data/example_data.RData")

#https://stackoverflow.com/questions/31794702/r-shiny-dashboard-tabitems-not-apparing
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

progress_indicator<-function(text_message){
  withProgress(message = text_message,
               detail = 'Please wait for a while', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
}
#env<-LoadToEnvironment("data/example_data.RData")
