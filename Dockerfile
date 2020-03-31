FROM rocker/shiny-verse

#RUN add-apt-repository -y ppa:opencpu/poppler
RUN  apt-get update
RUN  sudo apt-get install -y \
  pandoc   \
  apt-get install libglpk-dev \
  coinor-libsymphony-dev \
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


RUN install2.r --error \
    -r 'http://cran.rstudio.com' \
    readtext 

# Install R packages that are required
# TODO: add further package if you need!
RUN R -e "install.packages(c('ROI.plugin.glpk'), repos='http://cran.rstudio.com/')"



## Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
#COPY /mstat /srv/shiny-server/

## Copy further configuration files into the Docker image
#COPY shiny-server.sh /usr/bin/shiny-server.sh

#CMD ['/usr/bin/shiny-server.sh']
