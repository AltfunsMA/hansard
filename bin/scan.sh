#! /bin/bash
# RUNNING SCAN PIPELINE
# Runs the full pipeline.

# Alfonso Martinez Arranz

current_directory=$PWD

go_back() {

	# shellcheck disable=SC2164
	cd "$current_directory"

}

trap "" HUP
trap go_back EXIT

root_folder='/data/hansard'
main_topic=$1

if [[ -z $main_topic && -z $HANSARD_MAIN ]]; then

	echo "You should specify a main_topic such as 'coal' or 'civility' \
	as the first argument"
	echo "Defaulting to 'coal'"

	main_topic='coal'

fi

export HANSARD_MAIN=$main_topic

model_inputs="${main_topic}_data/04_model_inputs/"
model_outputs="${main_topic}_output/scan/"

export MODEL_INPUTS=$model_inputs

cd $root_folder || exit 1



logfile="logs/latest_${main_topic}_scan.log"

echo "All STDOUT and STDERR from here on will be logged at: $logfile"

{
echo "LOG STARTED"

timestamp=$(TZ="Australia/Melbourne"  date +"%Y-%m-%d_%H%M")

export TIMESTAMP=$timestamp

echo $timestamp

# printf "\n**************** LEMMATISING AND TF-IDF FILTERING *******************\n"

# # shellcheck disable=SC1091
# source .env/bin/activate



time scripts/modeling/01_get_final_dataframe_full_lemmas_tfidf.py &&

time scripts/modeling/02b_to_scan_input.py || exit 1


printf "\n**************** REPLACING BIGRAMS AND STOPWORDS *******************\n"

cd $model_inputs || exit 1

origcorpus="corpus.txt"; bincorpus='corpus.bin'; proccorpus='corpus_bigrammed.txt'

bigrams='BIGRAMS.txt'; stops='STOPWORDS.txt'

if [[ ! -f $proccorpus || \
$origcorpus -nt $proccorpus || \
$bigrams -nt $proccorpus || \
$stops -nt $proccorpus ]]; then

	time bigramise.sh $origcorpus $bigrams $stops || exit 1 # changes in place

else 

	echo "$origcorpus, $bigrams, and $stops are all  older than $proccorpus, \
	no need to reprocess"

fi &&

cd $root_folder || exit 1

printf "\n******************* GENERATING PARAMETER FILES *********************\n" 

time scripts/modeling/04_generate_params.R || exit 1


printf "\n************************* RUNNING MODELS***************************\n"

cd $model_inputs || exit 1

if [[ ! -f $bincorpus || \
$origcorpus -nt $bincorpus || \
-z "$(find $model_outputs -type f)" ]]; then

	mod_logfile="logs/${main_topic}_modeling.log"
	echo "Model logging is verbose and is redirected to $mod_logfile"
	
	cd $root_folder || exit 1
	
	time scripts/modeling/05_run_multiple_scan.sh &> logs/${main_topic}_modeling.log || exit 1

else
	echo "$origcorpus is older than $bincorpus, no need to run models"

fi &&

printf "\n************ EXTRACTING TOPICS FROM OUTPUT.DAT FILES****************\n"

cd $root_folder || exit 1

time scripts/modeling/06_scan_topic_cleaning.R || exit 1

printf "\n******************** CALCULATING COHERENCE ************************\n"

time scripts/modeling/07_calc_topic_coherence.py || exit 1

} &> $logfile


go_back
