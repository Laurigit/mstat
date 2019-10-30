FROM rocker/shiny-verse

#RUN add-apt-repository -y ppa:opencpu/poppler
RUN  apt-get update
RUN  sudo apt-get install -y \
  pandoc   \
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
    libsodium18 \
    libsodium-dev \
    vim
RUN chmod -R 755 /srv/shiny-server/
RUN    sudo sudo apt-get install -y libpoppler-cpp-dev


RUN R -e "install.packages(c('pdftools'), repos='http://cran.rstudio.com/')"

RUN install2.r --error \
    -r 'http://cran.rstudio.com' \
    readtext 

# Install R packages that are required
# TODO: add further package if you need!
RUN R -e "devtools::install_github('paulc91/shinyauthr')"
RUN R -e "install.packages(c('dragulaR', 'qdapRegex','clipr', 'shinyWidgets','shinydashboard','shinyjs','data.table','lubridate','DT','reshape2','jsonlite','rdrop2','zoo','rpivotTable','rvest','curl','stringr'), repos='http://cran.rstudio.com/')"
RUN R -e "install.packages(c('ggplot2', 'magick','ggthemes','shinyalert','anytime','readxl','qdapRegex','httr','V8','shinydashboardPlus','rhandsontable','tidyverse','beepr','reshape2','grid','gridExtra', 'beepr', 'RMySQL', 'testthat'), repos='http://cran.rstudio.com/')"


## Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
#COPY /mstat /srv/shiny-server/

## Copy further configuration files into the Docker image
#COPY shiny-server.sh /usr/bin/shiny-server.sh

#CMD ['/usr/bin/shiny-server.sh']
