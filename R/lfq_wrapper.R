### A wrapper function for LFQ analysis
## takes two input 
## 1. Maxquant proteinGroup file
## 2. Experiment design file (a tab separated file containing three columns
## lable, condition and replicate)
library("SummarizedExperiment")

LFQ_wrapper_old<-function(maxquant_data,expdesign){

source("functions.R")
  
### Import input MaxQuant proteinGroups.txt file
maxquant_output<- maxquant_data

## Optional experimental structure file
exp_design <- expdesign
# exp_design$label<-as.character(exp_design$label) ## as the experiment name contains numbers


######========= DATA PREPROCESSING =========#######
# Filter proteins by removing
# 1. Reverse sequences
# 2. Potential contaminants
# 3. Proteins only identified by sites
# 4. Proteins identified by single peptide
data <- maxquant_output %>% 
  dplyr:::filter(Reverse!="+", Only.identified.by.site!="+", Razor...unique.peptides>=2)

##===== Sanity Checks ======## 
## check if there are any duplicate Gene names in the data
# data$Gene.names %>% duplicated() %>% any()

## Count how many times duplication occured
# data %>% group_by(Gene.names) %>% summarise(frequency=n()) %>% arrange(desc(frequency)) %>% filter(frequency>1)

## Make data unique by combining  gene names and protein ids in rows where either of them is absent
data_unique<- DEP:::make_unique(data,"Gene.names","Protein.IDs",delim=";")

## Re-check if there are any duplicates
#data_unique$name %>% duplicated() %>% any()

#####======= Gnerating Summerised Experiment Object for differential expression analysis =======######

## get the location of LFQ intensity columns
lfq_columns<-grep("LFQ.", colnames(data_unique))

## Convert the protein dataframe into Summerised experiment object for further analysis
data_se<-DEP:::make_se(data_unique,lfq_columns,exp_design)

### OR generate SummerizedExperiment object on the fly based on the maxquant output
#data_se<-make_se_parse(data_unique,lfq_columns)

## Plot proteins count distribution across all samples
## This plot can be optional
pdf("protein_distribution.pdf")
DEP:::plot_frequency(data_se)
dev.off()

## Remove the rows with lot of missing values
## This is important step and depends of "thr" option
## If thr=0, it means all the replicates in each sample should have valid values, no missing values is allowed
## thr=1 means atleast 2 out 3 replicates should have valid values in each sample 
data_filter<-DEP:::filter_missval(data_se, thr = 1)


####================= QC Plots ==============#######
### How many proteins in each replicate
pdf("protein_counts.pdf")
DEP:::plot_numbers(data_filter)
dev.off()

## Barplot of proteins identificaiton overlap between samples 
pdf("protein_overlap_in_samples.pdf")
DEP:::plot_coverage(data_filter)
dev.off()


# meanSdPlot(data_filter) This can be optional

######============== Normalisation ===========########
## This function uses variance stabilizing transofmation (vsn) function for background correction 
## Not much useful for proteomics datasets
data_norm<-DEP:::normalize_vsn(data_filter)
#meanSdPlot(data_norm)# Can be optional

## Visualise effect of normalisation by ploting barplots before and after normalisation
pdf("normalization_plot.pdf")
plot_normalization(data_filter,data_norm)
dev.off()

### Important QC plot to visualised missing values in each protein across the samples
### Might not be useful for collaborators

## Heatmap to visualise patterns of missing value
## Important to decide which imputational method to use, i.e. if missing values are biased towards specific condition (for example control),
## then they fall under missing values not at random (MNAR) category and randoms drawn from left-shifted distribution (eg. MinProb or Similar to Perseus)  method can be used 
pdf("missing_value_heatmap.pdf")
plot_missval(data_filter)
dev.off()

## Check if missing values are biased towards lower intensity proteins
## Intensity distribution plot for proteins with and without missing values
## Might not be useful for collarborators
pdf("protein_intensities_and_missing_values.pdf")
plot_detect(data_filter)
dev.off()

######================ MISSING VALUE IMPUTATION ===============##########
## As for this dataset protein intensity plot indicated that proteins with missing value have on average low intensities,
## I choose missing value imputation to be one of MNAR method 
## Following code will impute missing value with algorithm similar to perseus
## i.e. random values drawn from normal distribution of 1.8 SD apart with the width of 0.3
data_imp_man<-DEP:::impute(data_filter,fun="man",shift=1.8,scale=0.3)

## Other imputation options are also available such as "Minimum Probablistic Algorithm", "K-nearest neighbourhood (knn)"
## "Quantile regression-based left-censored function (QRILC)" or "Maximum likelyhood (MLE)" methods
## knn and MLE are methods of choise for missing values at random (MAR) datasets
## all the options can be viewed by using "?impute" command in R console

## Visualise effect of imputation
## Plot intensity distribution before and after imputation
pdf("imputation_effect.pdf")
plot_imputation(data_filter,data_imp_man)
dev.off()

######============= DIFFERENTIAL EXPRESSION ANALYSIS ============###########

## This test uses protein-wise linear model with emperical Bayes statistics (used in R package limma)
## Limma is modified version of t-test and widely used for transcriptomics analysis
## Number of different options available such as "all", "control" and "man" to compare various conditions
## "all"- compares all pairwise comparisons
## "control"- asks to specify the control sample and compares every other sample against control
## "man"- manually specify which condition to test 

#data_diff_all_contrasts<-test_diff(data_imp_man,type='all')
# If the data has multiple pairwise comparisions we need to generate volcano plot for each comparison
# "comparison" is a character vectors that stores the name of all comparison and captured from
#  the message generated by "test_diff" function
# Example of message "Tested_contrasts: "Control_vs_Condition1", "Control_vs_Condition2""
comparisons<-capture.output(data_diff_all_contrasts<-test_diff(data_imp_man,type='all'),type = "message")

## Remove "Tested contrasts:"
comparisons<-gsub(".*: ","",comparisons)
## Split conditions into character vector
comparisons<-unlist(strsplit(comparisons,","))
## Remove leading and trailing spaces
comparisons<-trimws(comparisons)

## Mark significantly different proteins
## Input needed to define the threshold
## "alpha"- adjusted p-value cutoff
## "lfc"- log2(Fold Change) cutoff
alpha<- 0.05
lfc<- 1
param<- data.frame(alpha, lfc)
dep<-DEP:::add_rejections(data_diff_all_contrasts,alpha = 0.05,lfc = log2(1))

###### ===========  EXPLORATORY ANALYSIS ========= ######

## PCA plot
pdf("PCA_plot.pdf")
plot_pca(dep,x=1,y=2,n=nrow(dep),point_size = 4)
dev.off()

## Correlation plot
## Visualise correlation between different replicates
pdf("sample_correlation.pdf")
plot_cor(dep)
dev.off()

## Volcano plot for each pair-wise comparisons
#for (i in comparisons){

for(i in comparisons){
  pdf(paste0('volcano_plot_',i,'.pdf',sep=""))
  print(plot_volcano(dep,contrast = i,label_size = 2,add_names = T, adjusted=T))
  dev.off()
}

## Plot multiple scatterplots
## First get the LFQ expression data
## Use ggpairs function from GGally library to generate multiple matrix plots
paired_data<-as.data.frame(assay(dep))

## Plot scatterplot
pdf("scatterplot.pdf")
ggpairs(paired_data, 
        lower = list(continuous = wrap(matrixplot_modify,
                                       pts=list(size=1, colour="gray"), 
                                       smt=list(method="loess", se=F, size=1, colour="blue"))), 
        upper=list(continuous="cor"),
        diag = list(continuous="densityDiag")) +
  theme(panel.grid.major = element_blank())
dev.off()


## Heatmap
## Plot heatmap of all significant proteins with their expression in each sample
pdf("Heatmap.pdf")
plot_heatmap(dep,type="centered",kmeans = TRUE, k=6, col_limit = 6, show_row_names=T)
dev.off()


######=============== ENRICHMENT ANALYSIS ============== ##########
## Protein Set Enrichment Analysis is based on EnrichR
## Need to specify the databases to perform enrichment test

## Gene Ontology Enrichment
#gsea_results_GO <- test_gsea(dep)

## Plot GO Enrichment Results
#pdf("GO_Enrichment.pdf")
#plot_gsea(gsea_results_GO)
#dev.off()

## Pathway Enrichment Analysis (KEGG)

## KEGG enrichment
#results_kegg<- test_gsea(dep,databases = c("KEGG_2016"))

## Plot pathway enrichment results
#pdf("KEGG_Enrichment.pdf")
#plot_gsea(results_kegg)
#dev.off()

####=============== Write Results ===========#######
data_result<-get_results(dep)
write.csv(data_result,"LFQ_results.csv",row.names = FALSE)


#### return objects
return(data_filter)
return(dep)
return(data_result)

}
