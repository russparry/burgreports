FROM rocker/shiny:4.1.0

ADD install_packages.R .

RUN Rscript install_packages.R

COPY app /srv/shiny-server

USER shiny
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
