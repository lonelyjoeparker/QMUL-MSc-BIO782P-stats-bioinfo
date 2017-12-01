FROM r-base
COPY . ~/docker_test
WORKDIR ~/docker_test

# get ggplot2
RUN R -e "install.packages('ggplot2', repos = 'http://cran.us.r-project.org')"

# this changes to part_two.R and part_three.R...
CMD ["Rscript","part_one.R"]


