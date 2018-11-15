# Summary

This directory contains several directories. Each contains at least one Dockerfile, which is used to build a single Docker container. You can try these out for yourself.

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
