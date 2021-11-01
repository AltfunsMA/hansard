#! .env/bin/python 

import pandas as pd
import pickle as pkl
from collections import defaultdict
import random
import os

main_topic = os.getenv('HANSARD_MAIN')

random.seed(42)

data = pd.read_csv(open(main_topic + "_data/04_model_inputs/final_dtm_lemmas.df", 'r'))
data["date"] = data["date"].apply(lambda x: int(x.split("-")[-1][:-1]))

# be sure we're in order!!
data = data.sort_values(by="date").sample(frac=0.1)

multfile = open(main_topic + "_data/04_model_inputs/hansard-mult.dat", 'w+')
timesfile = open(main_topic + "_data/04_model_inputs/hansard-seq.dat", 'w+')
timecounts = defaultdict(lambda:0)

new_vocab={defaultdict:[0,""]}
for i,r in data.dropna(subset=["tfidf"]).iterrows():
    times = r["tfidf"]
    nwords = len(times.split(" "))
    
    if nwords > 3:
    #if random.random() < 0.1:
        timecounts[r["date"]]+=1
        multfile.write(f"{nwords} {times}\n")
        
ntimes = len(set(data["date"].values))
timesfile.write(f"{ntimes}\n")
for v in sorted(set(data["date"].values)):
    print(v)
    timesfile.write(f"{timecounts[v]}\n")

multfile.close()
timesfile.close()
