FROM rocker/shiny-verse:3.6.0
RUN apt-get update && apt-get install -y libnetcdf-dev
RUN Rscript -e 'install.packages(c("devtools", "mvtnorm", "tmvtnorm","impute", "pcaMethods", "imputeLCMD", "plotly", "DT", "BiocManager","testthat", "RColorBrewer", "shiny","shinyalert","shinydashboard", "shinyjs", "svglite"), dependencies=TRUE)'
FROM bioconductor/bioconductor_docker:RELEASE_3_12
RUN apt-get update
RUN R -e 'BiocManager::install(ask = F)'
RUN Rscript -e 'BiocManager::install(pkgs=c("DEP", "SummarizedExperiment", "limma", "ComplexHeatmap", ask=F))'
COPY . /srv/shiny-server/LFQ-Analyst
RUN chmod -R +r /srv/shiny-server/LFQ-Analyst
