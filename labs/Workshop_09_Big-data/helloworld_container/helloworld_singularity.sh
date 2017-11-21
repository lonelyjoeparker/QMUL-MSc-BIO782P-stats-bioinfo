#$ -cwd
#$ -S /bin/bash
#$ -j y
#$ -pe smp 1
#$ -l h_vmem=2G

### run the whalesay container from Docker Hub
module load singularity
singularity run  /data1/SBCS-MSc-BioInf/2017-BIO782P/QMUL-MSc-BIO782P-stats-bioinfo/labs/Workshop_09_Big-data/helloworld_container/phylodev-helloworld.img  oooo 
