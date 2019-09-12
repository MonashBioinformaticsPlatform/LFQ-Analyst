FROM rocker/shiny-verse:3.4.3
RUN apt-get update && apt-get install -y libnetcdf-dev
RUN Rscript -e 'install.packages(c("devtools", "mvtnorm", "tmvtnorm","impute", "pcaMethods", "imputeLCMD", "plotly", "DT", "BiocInstaller","testthat", "RColorBrewer", "shiny","shinyalert","shinydashboard", "shinyjs"), dependencies=TRUE)'
RUN Rscript -e 'BiocInstaller::biocLite(pkgs=c("DEP", "SummarizedExperiment", "limma", "ComplexHeatmap"))'
COPY . /srv/shiny-server/LFQ-Analyst
