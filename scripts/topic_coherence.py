# env/bin/python
# gensim topic model 

#%%

import pandas as pd
from gensim.models.coherencemodel import CoherenceModel
from gensim.corpora import Dictionary
import os
import re

os.chdir('/data/hansard')

main_topic = 'coal' 
# os.getenv('HANSARD_TOPIC_TO_PROCESS')


#%% 
# Load data
lemmas = pd.read_csv('coal_data/input/final_dtm_lemmas.df').\
    dropna(subset=['lemmas']).lemmas

topics_raw = pd.read_csv("analyse_output/scan/coal_1900_k10.csv")


#%%
# Format data

texts = lemmas.apply(lambda s: re.split(" |_", s)).tolist()

dictionary = Dictionary(documents=texts)

topics = topics_raw.drop(['prob', 'time', 'topic'], axis = 1).\
    values.tolist()

out = ["power_station" in s for s in topics]

textout = ["power_station" in s for s in texts]

sum(out)
sum(textout)
#%%
# Calculate coherence

cm = CoherenceModel(dictionary=dictionary, topics = topics, texts=texts, 
    coherence="c_uci")

coherence = cm.get_coherence()




#%%
# Create elements needed for topic coherence analysis by year
for i in set(df.year):



