### Test if column names are proper in experiment design file

exp_design_test<-function(exp_design){
  col_names<-colnames(exp_design)
  ## 
  if(!"label" %in% col_names){
    stop("The column 'label' is not found in the Experimental Design Matrix File",
         call. = FALSE)
  }
  
  else if (!"condition" %in% col_names){
    stop("The column 'condition' is not found in the Experimental Design Matrix File",
         call. = FALSE)
  }
  
  else if (!"replicate" %in% col_names){
    stop("The column 'replicate' is not found in the Experimental Design Matrix File",
         call. = FALSE)
  }
  
}


maxquant_input_test<-function(maxquant_input){
  col_names<-colnames(maxquant_input)
  ## 
  if(!"Gene.names" %in% col_names){
    stop("The column 'Gene names' is not found in the MaxQuant proteinGroups File",
         call. = FALSE)
  }
  
  else if (any(grepl("LFQ", col_names))==FALSE){
    stop("Columns starting with 'LFQ' are not found in the MaxQuant proteinGroups File",
         call. = FALSE)
  }
  
  else if (!"Protein.IDs" %in% col_names){
    stop("The column 'Protein IDs' is not found in the MaxQuant proteinGroups File",
         call. = FALSE)
  }
  
}