#! .env/bin/python

import pandas as pd
import pickle as pkl
from collections import defaultdict
import random
import os

current_path = os.getcwd()

data_path = os.environ['MODEL_INPUTS']

os.chdir(data_path)

random.seed(42)

# for AUS data
data = pd.read_csv(open("final_1000_40000.tsv", 'r'), sep="\t")
vocab = [l.strip() for l in open("vocab_1000_40000.txt", 'r').readlines()]
yearidx=-1
datecol="date"
outfile="scan.dat"

# for GER data
#data = pd.read_csv(open("./bundestag_data/final_300_6000.tsv", 'r'), sep="\t")
#vocab = [l.strip() for l in open("./bundestag_data/vocab_300_6000.txt", 'r').readlines()]
#yearidx=0
#datecol="Date"
#outfile="bundestag_data/scan/scan.dat"

data[datecol] = data[datecol].apply(lambda x: int(x.split("-")[yearidx]))

# be sure we're in order!!
data = data.sort_values(by=datecol)

scanfile = open(outfile, 'w+')

for i,r in data.iterrows():
    text = r["spacy"]
    if type(text)==str and len(text.split(" ")) > 10:
        date = r[datecol]
        filtered = " ".join([w for w in text.split(" ") if w in vocab])
        scanfile.write(f"{date}\t{filtered}\n")

os.chdir(current_path)