### Test if column names are proper in experiment design file

exp_design_test<-function(exp_design){
  col_names<-colnames(exp_design)
  ## 
  if(!"label" %in% col_names){
    stop(safeError("The column 'label'(case sensitive) is not found in the Experimental Design File"))
  }
  
  else if (!"condition" %in% col_names){
    sstop(safeError("The column 'condition' (case sensitive) is not found in the Experimental Design File"))
  }
  
  else if (!"replicate" %in% col_names){
    stop(safeError("The column 'replicate' (case sensitive) is not found in the Experimental Design File"))
  }
  
}


maxquant_input_test<-function(maxquant_input){
  col_names<-colnames(maxquant_input)
  ## 
  if(!"Gene.names" %in% col_names){
    stop(safeError("The column 'Gene names' is not found in the MaxQuant proteinGroups File"))
  }
  
  else if (any(grepl("LFQ", col_names))==FALSE){
    stop(safeError("Columns starting with 'LFQ' are not found in the MaxQuant proteinGroups File"))
  }
  
  else if (!"Protein.IDs" %in% col_names){
    stop(safeError("The column 'Protein IDs' is not found in the MaxQuant proteinGroups File"))
  }
  
  else if (!"Reverse" %in% col_names){
    stop(safeError("The column 'Reverse' is not found in the MaxQuant proteinGroups File"))
  }
  
  else if (!"Potential.contaminant" %in% col_names){
    stop(safeError("The column 'Potential contaminant' is not found in the MaxQuant proteinGroups File"))
  }
  
  else if (!"Only.identified.by.site" %in% col_names){
    stop(safeError("The column 'Only identified by site' is not found in the MaxQuant proteinGroups File"))
  }
  
  else if (!"Razor...unique.peptides" %in% col_names){
    stop(safeError("The column 'Razor + unique peptides' is not found in the MaxQuant proteinGroups File"))
  }
  
  else if (!"Protein.names" %in% col_names){
    stop(safeError("The column 'Protein names' is not found in the MaxQuant proteinGroups File"))
  }
  
  
  
}
