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