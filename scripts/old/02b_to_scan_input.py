#! .env/bin/python 

import pandas as pd
import pickle as pkl
from collections import defaultdict
import random
import os

#%%
random.seed(42)

main_topic = os.getenv('HANSARD_MAIN', 'coal')

print("Creating SCAN input for: ", main_topic)

data = pd.read_csv(open(main_topic + "_data/04_model_inputs/final_dtm_lemmas.df", 'r')).\
    dropna(subset=["lemmas"])


vocab = [l.strip() for l in open(main_topic + "_data/04_model_inputs/vocab.txt", 'r').readlines()]


data["date"] = data["date"].apply(lambda x: int(x.split("-")[-1]))

# be sure we're in order!!
data = data.sort_values(by="date")

scanfile = open(main_topic + "_data/04_model_inputs/corpus.txt", 'w+')

for i,r in data.iterrows():
    text = r["lemmas"]
    if type(text)==str and len(text.split(" ")) > 10:
        date = r["date"]
        filtered = " ".join([w for w in text.split(" ") if w in vocab])
        scanfile.write(f"{date}\t{filtered}\n")
