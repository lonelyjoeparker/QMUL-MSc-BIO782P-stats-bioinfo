# Use an official Ubuntu runtime as a parent image
FROM ubuntu:16.04

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
ADD . /app

# Run reverse_args.sh when the container launches
ENTRYPOINT ["bash","run_raxml.sh"]
