import pandas as pd
import pickle as pkl
from collections import defaultdict
import random
import os

#%%
random.seed(42)

main_topic = os.getenv('HANSARD_MAIN')

main_topic = 'coal'

print("Creating SCAN input for:", main_topic)

data = pd.read_csv(open(main_topic + "_data/input/final_dtm_lemmas.df", 'r')).\
    dropna(subset=["lemmas"])

bigrams_raw = open("BIGRAMS.txt", 'r').readlines()
bigrams_us = [s.replace(" ", "_") for s in bigrams_raw]


for raw, us in zip(bigrams_raw, bigrams_us):
    data['lemmas'] = data.lemmas.apply(lambda s: s.replace(raw, us))

vocab_orig = [l.strip() for l in open(main_topic + "_data/input/vocab.txt", 'r').readlines()]

# Ensure words of interest are included
targets= [l.strip() for l in open(main_topic + "_data/targets.txt", 'r').readlines()]

vocab = list(set(vocab_orig + targets))


data["date"] = data["date"].apply(lambda x: int(x.split("-")[-1]))

# be sure we're in order!!
data = data.sort_values(by="date")

scanfile = open(main_topic + "_data/corpus.txt", 'w+')

for i,r in data.iterrows():
    text = r["lemmas"]
    if type(text)==str and len(text.split(" ")) > 10:
        date = r["date"]
        filtered = " ".join([w for w in text.split(" ") if w in vocab])
        scanfile.write(f"{date}\t{filtered}\n")
