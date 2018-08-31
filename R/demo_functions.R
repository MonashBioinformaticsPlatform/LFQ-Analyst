### functions to deal with demo data

LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

#env<-LoadToEnvironment("data/example_data.RData")