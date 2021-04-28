FROM rocker/shiny-verse

#RUN add-apt-repository -y ppa:opencpu/poppler
RUN  apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install lightdm -y 
RUN  sudo apt-get install -y \
  pandoc   \
  coinor-libcgl-dev \
  libglpk-dev \
  coinor-symphony \
  coinor-libsymphony-dev \
  coinor-libsymphony-doc \
  pandoc-citeproc
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    xclip \
    libmagick++-dev \
    libcurl4-gnutls-dev \
    libxt-dev \
    libssl-dev \
    libjpeg-dev \
    libv8-dev \
    vlc \
   
    libsodium-dev \
    vim
RUN chmod -R 755 /srv/shiny-server/
RUN    sudo sudo apt-get install -y libpoppler-cpp-dev


RUN R -e "install.packages(c('pdftools'), repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages(c('ROI.plugin.symphony'), repos='http://cran.rstudio.com/')"
RUN install2.r --error \
    -r 'http://cran.rstudio.com' \
    readtext 

# Install R packages that are required
# TODO: add further package if you need!

	
RUN R -e "devtools::install_github('paulc91/shinyauthr')"
RUN R -e 'install.packages("https://cran.r-project.org/src/contrib/Archive/shinydashboardPlus/shinydashboardPlus_0.6.0.tar.gz", repos=NULL, type="source")'
RUN R -e "install.packages(c('ROI.plugin.symphony', 'ROI.plugin.glpk','optiRum', 'dplyr', 'ROI', 'ompr.roi', 'ompr', 'adagio', 'dragulaR', 'qdapRegex','clipr', 'shinyWidgets','shinydashboard','shinyjs','data.table','lubridate','DT','reshape2','jsonlite','rdrop2','zoo','rpivotTable','rvest','curl','stringr'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('ggplot2', 'magick','ggthemes','shinyalert','anytime','readxl','qdapRegex','httr','V8','rhandsontable','tidyverse','beepr','reshape2','grid','gridExtra', 'beepr', 'RMySQL', 'testthat'), repos='http://cran.rstudio.com/')"



## Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
#COPY /mstat /srv/shiny-server/

## Copy further configuration files into the Docker image
#COPY shiny-server.sh /usr/bin/shiny-server.sh

#CMD ['/usr/bin/shiny-server.sh']
