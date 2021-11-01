#! /bin/bash
# this file to be called with "scan.sh"

main_folder=$1

if [[ -z $main_folder ]]; then main_folder=$HANSARD_MAIN; fi

cd /data/hansard || exit 1

param_files=$(find "${main_folder}_data/lea_test_parameters/" -name "*.txt")

for params in $param_files; do

  # echo $params

 ./dynamic-senses -parameter_file="$params" -create_corpus -store=true
 ./dynamic-senses -parameter_file="$params" -store=true

 
done

  
echo "DONE"
