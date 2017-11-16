#!/bin/bash

raxmlHPC-PTHREADS-SSE3 -T 12 -p `date +%s` -n out "$@"
