# print usage information first
echo usage: docker run raxml_executable_container \<input alignment\> \<model\> \<number of starting trees\> \<random seed\> \<name for output\>
./raxmlHPC -s $1 -m $2 -N $3 -p $4 -n $5
# copy the outputs to /data, assuming it's bound of course...
cp /app/RAxML*$5 /data/
