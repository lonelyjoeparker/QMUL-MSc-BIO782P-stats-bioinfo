FROM r-base
COPY . ~/docker_test
WORKDIR ~/docker_test

RUN R -e "install.packages('ggplot2', repos = 'http://cran.us.r-project.org')"

CMD ["Rscript","part_one.R"]


