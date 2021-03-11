FROM rocker/shiny-verse:3.4.3
RUN apt-get update && apt-get install -y libnetcdf-dev libcairo2-dev
RUN pkg-config --cflags cairo | export PKG_CONFIG_PATH=/usr/local/opt/libffi/lib/pkgconfig
RUN Rscript -e 'install.packages(c("devtools", "mvtnorm", "tmvtnorm","impute", "pcaMethods", "imputeLCMD", "plotly", "DT", "BiocInstaller","testthat", "RColorBrewer", "shiny","shinyalert","shinydashboard", "shinyjs", "svglite"), dependencies=TRUE)'
RUN Rscript -e 'BiocInstaller::biocLite(pkgs=c("DEP", "SummarizedExperiment", "limma", "ComplexHeatmap"))'
COPY . /srv/shiny-server/LFQ-Analyst
