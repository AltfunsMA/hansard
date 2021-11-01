import pandas as pd
import pickle as pkl
from collections import defaultdict
import random

random.seed(42)
# for AUS data
datecol="date"
data = pd.read_csv(open("final_1000_40000.tsv", 'r'), sep="\t")
yearidx=-1
outprefix="hansard"

#datecol="Date"
#data = pd.read_csv(open("./bundestag_data/final_300_6000.tsv", 'r'), sep="\t")
#yearidx=0
#outprefix="bundestag-decade"

data[datecol] = data[datecol].apply(lambda x: int(x.split("-")[yearidx][:-1]))

# be sure we're in order!!
data = data.sort_values(by=datecol).sample(frac=0.1)

multfile = open(f"{outprefix}-mult.dat", 'w+')
timesfile = open(f"{outprefix}-seq.dat", 'w+')
timecounts = defaultdict(lambda:0)

new_vocab={defaultdict:[0,""]}
for i,r in data.dropna(subset=["wbow"]).iterrows():
    times = r["wbow"]
    nwords = len(times.split(" "))
    
    if nwords > 3:
    #if random.random() < 0.1:
        timecounts[r[datecol]]+=1
        multfile.write(f"{nwords} {times}\n")
        
ntimes = len(set(data[datecol].values))
timesfile.write(f"{ntimes}\n")
for v in sorted(set(data[datecol].values)):
    print(v)
    timesfile.write(f"{timecounts[v]}\n")

multfile.close()
timesfile.close()
