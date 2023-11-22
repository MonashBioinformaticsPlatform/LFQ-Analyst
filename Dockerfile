FROM rocker/shiny-verse:4.2.1

RUN apt-get update && apt-get install -yq \
    libhdf5-dev libnetcdf-dev build-essential libgd-dev libbz2-dev libudunits2-dev libproj-dev libgdal-dev \
    texlive-latex-base texlive-fonts-recommended texlive-fonts-extra texlive-latex-extra

RUN Rscript -e 'install.packages(c("devtools", "tidyverse", "ggrepel", "httr", "rjson", "mvtnorm", "tmvtnorm", \
"imputeLCMD", "plotly", "DT", "BiocManager","testthat", "RColorBrewer", "shiny","shinyalert","shinydashboard", \
"shinyjs", "svglite", "rhandsontable", "shinyBS", "shinyWidgets", "ggVennDiagram", "shinycssloaders","shiny.info"), dependencies=TRUE)'

#FROM bioconductor/bioconductor_docker:RELEASE_3_15
RUN Rscript -e 'BiocManager::install(pkgs=c("DEP", "SummarizedExperiment", "limma", "ComplexHeatmap","pcaMethods","impute"), ask=F, dependencies=TRUE)'


COPY . /srv/shiny-server
#COPY /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY entrypoint.sh /
#COPY shiny-server.sh /usr/bin/shiny-server.sh
#RUN chmod -R 755 /usr/bin/shiny-server.sh

# disable shiny-server
#RUN touch /etc/services.d/shiny-server/down

#RUN rm -f /srv/shiny-server/lfq-analyst/.Rprofile
RUN chmod -R +r /srv/shiny-server

RUN mkdir /srv/shiny-server/logs

RUN chmod -R ugo+wrX /var/log/shiny-server/

RUN chown -R shiny:shiny /srv/shiny-server/

RUN chmod o+w /usr/local/lib/R/site-library

RUN rm -rf /srv/shiny-server/[0-9][0-9]_* /srv/shiny-server/index.html /srv/shiny-server/sample-apps

#USER shiny

CMD ["/entrypoint.sh", "/usr/bin/shiny-server"]
