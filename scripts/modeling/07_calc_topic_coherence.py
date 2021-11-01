#! .env/bin/python
# gensim topic coherence calculation
# Takes 96 minutes to run in full (815 minutes)


#%%

from os.path import splitext
import pandas as pd
from gensim.models.coherencemodel import CoherenceModel
from gensim.corpora import Dictionary
import os
import re
import numpy as np
from datetime import datetime
from dateutil import tz

AEST = tz.gettz("Australia/Melbourne")
now = datetime.now(AEST)
timestamp = os.getenv('TIMESTAMP', now.strftime("%Y%m%d_%H%M"))

main_topic = os.getenv('HANSARD_MAIN', 'coal')
input_folder = main_topic + "_output/cleaned/scan_" + timestamp + "/"
output_path = main_topic + "_output/scan_coh_"  + timestamp + ".csv"





#%% 
print("Loading data...")

# lemmas = pd.read_csv('coal_data/input/final_dtm_lemmas.df').\
#     dropna(subset=['lemmas']).lemmas

lemmas = pd.read_csv(main_topic + '_data/04_model_inputs/corpus_bigrammed.txt', 
    delimiter= "\t",
    names=['year', 'lemmas']).\
        dropna(subset=['lemmas']).lemmas
    
texts = lemmas.apply(lambda s: re.split(" ", s)).tolist()

dictionary = Dictionary(documents=texts)

#%%
print("Topic coherence calculation...")

topics_files = os.listdir(input_folder)

def calc_coherence(path):

    base = os.path.basename(path)

    print('Processing: ', base)

    topics_raw = pd.read_csv(input_folder + path)
    topics = topics_raw.drop(['prob', 'time', 'topic'], axis = 1).\
        values.tolist()

    # parameters are stored in basename
    out_params = os.path.splitext(base)[0]

    try:
        cuci = CoherenceModel(dictionary=dictionary, topics = topics, texts=texts, 
            coherence="c_uci")

        c_npmi = CoherenceModel(dictionary=dictionary, topics = topics, texts=texts, 
            coherence="c_npmi")

        out_cuci = cuci.get_coherence()
        out_cnpmi = c_npmi.get_coherence()

    except KeyError as err:
        print("Handled a KeyError exception:", err)

        out_cuci = "KeyError:" + str(err)
        out_cnpmi = "----"

    return out_params, out_cuci, out_cnpmi

params, cuci, cnpmi  = zip(*[calc_coherence(p) for p in topics_files])

out_df = pd.DataFrame(zip(params, cuci, cnpmi), columns=['params', 'c_uci', 'c_npmi'])


#%%
out_df.to_csv(output_path)

print("Saved to: ", output_path)

cor_df = out_df[out_df['c_npmi'] != "----"]

print("\nCorrelation between uci and npmi:",  np.corrcoef(cor_df.c_uci, cor_df.c_npmi))

print("\nDONE")




