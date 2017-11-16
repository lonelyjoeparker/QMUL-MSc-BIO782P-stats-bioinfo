#$ -cwd
#$ -S /bin/bash
#$ -j y
#$ -pe smp 1
#$ -l h_vmem=2G

### run the whalesay container from Docker Hub
module load singularity
singularity run -B /data1/SBCS-MSc-BioInf/2017-BIO782P/QMUL-MSc-BIO782P-stats-bioinfo/labs/Workshop_09_Big-data/raxml-container-test/RAxML:/tmp phylodev-helloworld.img -s demo_data/mc.paml -m GTRGAMMAI -p 122 -n agave_01  
