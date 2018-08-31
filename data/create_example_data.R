### create example data rds

maxquant_output<-read.table("data/proteinGroups.txt",header=TRUE,sep="\t")

exp_design <- read.table("data/exp_design_p10_0144.txt", 
                         header=TRUE, 
                         sep = "\t", 
                         stringsAsFactors=FALSE)

save(maxquant_output, exp_design, file = "data/example_data.RData")

