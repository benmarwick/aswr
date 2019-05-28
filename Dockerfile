FROM rocker/verse

COPY . home/rstudio/aswr

RUN  chmod 777 -R home/rstudio \
     && cd home/rstudio/aswr \
     && Rscript -e 'install.packages(c("remotes", "plotly", "docxtractr"))' \
     && Rscript -e 'remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer", "benmarwick/wordcountaddin"))' \
     && Rscript -e 'rmarkdown::render_site("inst/.", output_format = "bookdown::gitbook", encoding = "UTF-8")'

