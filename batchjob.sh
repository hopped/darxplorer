#!/bin/bash

########################
##  BATCH PROCESSING  ##
########################

# redirecting output
exec 1>./batchjob.${$}.log
exec 2>./batchjob.${$}.debug

# let's start!
echo "DARXplorer 2.0:" $1
echo
echo "Compiling ..."
echo

make clean
make run-compiler ARG=$1
make function

# Laura
echo "Running 'Laura' .."
echo
./darxplorer -l -t=laura -h=1
./darxplorer -l -t=laura -h=2
./darxplorer -l -t=laura -h=3
# Grete
echo "Running 'Grete' .."
echo
./darxplorer -l -t=grete -h=1
./darxplorer -l -t=grete -h=2
./darxplorer -l -t=grete -h=3
# Petra
echo "Running 'Petra'"
echo
./darxplorer -l -t=petra -h=1
./darxplorer -l -t=petra -h=2
./darxplorer -l -t=petra -h=3



