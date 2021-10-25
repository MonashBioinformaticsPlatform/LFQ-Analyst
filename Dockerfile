FROM rocker/shiny-verse:3.4.3
RUN apt-get update && apt-get install -y libnetcdf-dev libcairo2-dev
RUN pkg-config --cflags cairo | export PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig
RUN Rscript -e 'install.packages(c("devtools", "mvtnorm", "tmvtnorm","impute", "pcaMethods", "imputeLCMD", "plotly", "DT", "BiocManager","testthat", "RColorBrewer", "shiny","shinyalert","shinydashboard", "shinyjs", "svglite"), dependencies=TRUE, repos="http://cran.rstudio.com/")'
RUN Rscript -e 'BiocManager::install(pkgs=c("DEP", "SummarizedExperiment", "limma", "ComplexHeatmap"))'
COPY . /srv/shiny-server/LFQ-Analyst
