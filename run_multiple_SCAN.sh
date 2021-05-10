#! /bin/bash

cd /data/hansard

param_files=$(find . -name "parameters_*")


for params in $param_files; do

  # echo $params
 ./dynamic-senses -parameter_file=$params -store=true &>> civility_scan.log
 
done
