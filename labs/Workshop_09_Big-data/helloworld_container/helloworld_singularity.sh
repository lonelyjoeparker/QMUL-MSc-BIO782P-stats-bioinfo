#$ -cwd
#$ -S /bin/bash
#$ -j y
#$ -pe smp 1
#$ -l h_vmem=2G

### run the whalesay container from Docker Hub
module load singularity
singularity run phylodev-helloworld.img  oooo -o whales.out
