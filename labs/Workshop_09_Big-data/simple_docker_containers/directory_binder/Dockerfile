# Use an official Ubuntu runtime as a parent image
FROM ubuntu:16.04

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
ADD . /app

# Whenever we run this image we need to mount an external resource using the -v notation.
# See https://docs.docker.com/storage/bind-mounts

# Run reverse_args.sh when the container launches
ENTRYPOINT ["bash","bound_directory_operations.sh"]
