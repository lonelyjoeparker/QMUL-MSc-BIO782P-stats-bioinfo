# Summary

This directory contains several directories. Each contains at least one Dockerfile, which is used to build a single Docker container. You can try these out for yourself. You can also see them on the Docker Hub at https://hub.docker.com/r/lonelyjoeparker/phylodev/tags/

## An extremely simple container:

The `simply_print` directory builds a container which simply prints 'I am a working container', using `/bin/echo`. You can append arguments.

Build/run it with:

```
# build the container, pointing to the directory where the Dockerfile is, and tagging (naming) the container with the -t option
sudo docker build ./simply_print -t a_simple_container
# run this container
sudo docker run a_simple_container
# > 'I am a simple container'
# run this container passing more arguments to the /bin/echo command
sudo docker run a_simple_container 'and here are' 'some more' ' arguments'
# > 'I am a simple container' and here are some more  arguments
```

## A slightly more complicated container which uses a shell script to rearrange input arguments:

The `simple_echo` directory contains another minimal Dockerfile but this time the ENTRYPOINT calls `/bin/bash` and an associated shell script, `reverse_args.sh`. This means that we need to copy that script into the Dockerfile using the `ADD` command:

```
# build the simple echo container
docker build simple_echo/ -t reverser_container
# run it
sudo docker run reverser_container foo bar
# > /app
# > total 16K
# > drwxr-xr-x 35 root root 4.0K Nov 15 21:59 ..
# > drwxr-xr-x  2 root root 4.0K Nov 15 21:59 .
# > -rw-rw-r--  1 root root  166 Nov 15 21:56 reverse_args.sh
# > -rw-rw-r--  1 root root  285 Nov 15 21:59 Dockerfile
# > input arguments were foo bar
# > reversed they are bar foo
```

## A fairly complicated script which uses head to print the first few lines of an input argument to an output file

See `./directory_binder`.

## A vanilla RAxML version which just prints version info

See `./raxml_simple_container`.

## A RAxML version which runs `-N` separate starting trees

See `./raxml_executable_container`.

# Running these on singularity

You should be able to run these on singularity (**from within the appropriate directories...**) with `singularity run <container_image>` e.g.

```
# cd /data1/SBCS-MSc-BioInf/2017-BIO782P/QMUL-MSc-BIO782P-stats-bioinfo/labs/Workshop_09_Big-data/simple_docker_containers/simply_print
singularity run ./phylodev-simple_container.simg 

# cd /data1/SBCS-MSc-BioInf/2017-BIO782P/QMUL-MSc-BIO782P-stats-bioinfo/labs/Workshop_09_Big-data/simple_docker_containers/simple_echo
singularity run ./phylodev-reverser.simg arg_one_foo arg_two_bar

# cd /data1/SBCS-MSc-BioInf/2017-BIO782P/QMUL-MSc-BIO782P-stats-bioinfo/labs/Workshop_09_Big-data/simple_docker_containers/directory_binder
singularity run -B  /data/home/<username>:/custom_data  <some input file which exists already> <some output file to create> ./phylodev-binder.simg <input_file> <output_file>

# cd /data1/SBCS-MSc-BioInf/2017-BIO782P/QMUL-MSc-BIO782P-stats-bioinfo/labs/Workshop_09_Big-data/simple_docker_containers/raxml_simple_container
singularity run phylodev-raxml_container_simple.simg

# cd /data1/SBCS-MSc-BioInf/2017-BIO782P/QMUL-MSc-BIO782P-stats-bioinfo/labs/Workshop_09_Big-data/simple_docker_containers/raxml_executable_container
singularity run -B  /data1/SBCS-MSc-BioInf/2017-BIO782P/QMUL-MSc-BIO782P-stats-bioinfo/labs/Workshop_07_Beyond-GLMs:/custom_data phylodev-raxml_container_executable.simg /custom_data/mc.paml GTRCAT 1 2 testrax
```

More general form for the RAxML executable container:

```
singularity run -B  /data1/SBCS-MSc-BioInf/<some_valid_directory_containing_input_file>:/custom_data phylodev-raxml_container_executable.simg /custom_data/<input_file> <model> <replicates> <seed> <filename>
```
