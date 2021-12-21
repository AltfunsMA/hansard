#! /bin/bash
# Set-up download of text from the Hansard
# Alfonso Martínez Arranz
# R 3.6.1

# Download hansard metadata

this_script=$(basename "$0")

lockfile=/var/lock/${this_script%.sh}.lock

trap "rm -f $lockfile" EXIT

if [[ -e $lockfile  ]]; then

	echo "Lockfile '$lockfile' found. $this_script most likely already running."

	exit

fi
  
echo $$ >> "$lockfile"

echo "$lockfile created with PID: $$"

check_length() {

touch /data/hansard/logs/get_hans_metadata.log
touch /data/hansard/logs/dl_metadata_codes.log


length=$(< /data/hansard/logs/get_hans_metadata.log wc -l)

echo "Current number of confirmed downloads is $length"

cd /data/hansard || exit

}

check_length

attempt=0
max_attempts=5

num_months=1450 # from 1 Jan 1901 until 30 Nov 2021


while (( length < num_months && attempt < max_attempts )); do

  scripts/wrangling/00_get_metadata.R $attempt
  
  check_length
  
  attempt=$((attempt + 1))
  
  
done

if (( length == num_months )); then

echo "Full download DONE"

elif (( attempt == max_attempts )); then

echo "Too many attempts"

else 

echo "Unsure why this is finished"


fi

