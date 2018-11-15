# Whenever we run this image we need to mount an external resource using the -v notation.
# See https://docs.docker.com/storage/bind-mounts

# print the working directory
pwd
# list the contents of the working directory
ls -laht
# list the contents of the bound directory at '/data'
echo list contents of /data
ls -laht /data
# do something to a file ($1) inside the /data folder, sending it to ($2)
echo head /data/$1
head /data/$1
echo head /data/$1 > /data/$2
head /data/$1 > /data/$2
