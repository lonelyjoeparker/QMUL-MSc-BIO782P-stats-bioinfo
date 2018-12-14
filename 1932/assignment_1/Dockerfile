# Docker R base image
FROM r-base

COPY . /usr/local/src/myscripts
WORKDIR /usr/local/src/myscripts

# Install the R libraries needed to run the scripts
RUN /usr/bin/R --vanilla -f install_libraries.R

# Execute the target script
CMD ["Rscript", "Ana_Penedos-UID_1932-Assignment_1.R"]
