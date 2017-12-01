#!/bin/bash

docker pull bt17001/bio782p:part_one
docker pull bt17001/bio782p:part_two
docker pull bt17001/bio782p:part_three

docker run --rm  part_one
docker run --rm  part_two
docker run --rm  part_three
