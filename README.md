# ANALYSING TRENDS IN THE AUSTRALIAN HANSARD RECORDS

(Best displayed w markdown formatting on)

Currently, the following refers to the "coal" subset of data, although most of
the structure is replicated and the scripts are the same.

 "Civility" data is kept separate even though some of the records may be the
 same because space is not an issue downloading can occur in the background with little extra input.
 With just two datasets about rather different topics, this is more
 straightforward than devising a new structure to keep them together (e.g. by
 date) but callable separately at
 will. In future, this should be considered but probably with a proper
 query structure (SQL-like).

## DATA FOLDER

The subfolders are structured in order of execution:

1) `records` obtained from aph website as per search string
2) `full_text` raw downloads as rectangular dataframes by year including records
3) `processed` cleaned up version of `full_text`
4) `model_inputs` generated for the model, includes the combined full text
5) `scan_parameters` produced in bulk

The first three correspond to scripts in `scripts/download`
The last two correspond to scripts in `scripts/modeling`

## OUTPUT FOLDER

1. `dtm` contains raw output from [Dynamic Topic Modelling](https://github.com/blei-lab/dtm)
2. `scan` contains raw output from [SCAN](https://github.com/ColiLea/scan)
3. `cleaned` contains processed output from either model in CSV format, which is
   used to more easily calculate coherence
4. `scan_coherence_*.csv` calculations 

All scripts generating the above are stored under `scripts/modeling`.

## Others

Files needed to run SCAN, Python requirements.txt and R Project data.


## Run the full pipeline

Run `scan.sh` to run the scan modelling pipeline. It's in $PATH so can be called
anywhere and will run with the settings in the corresponding scripts.


