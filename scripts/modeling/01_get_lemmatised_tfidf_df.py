#! .env/bin/python -u

import csv
from tqdm import tqdm
import spacy
import pandas as pd
import os
from sklearn.feature_extraction.text import TfidfVectorizer
from collections import defaultdict, Counter

current_path = os.getcwd()

# data_path = os.environ['MODEL_INPUTS']

# main_hansard = os.environ['HANSARD_MAIN'] 

data_path = "/data/hansard/civility_data/04_model_inputs/"
main_folder = "civility"


os.chdir(data_path)

print("Script running in :" + os.getcwd())

nlp = spacy.load("en_core_web_sm")
# nlp.remove_pipe("tagger")
nlp.remove_pipe("parser")
nlp.remove_pipe("ner")
nlp.add_pipe('sentencizer')

def clean_df(path):
    data = pd.read_csv(open(path, 'r'))
    # drop empty texts
    data = data.dropna(subset=["main_text"])
    data.fillna("")
    # remove tabs and newlines in main_text
    for c in data.columns:
        try:
            data[c] = data[c].str.replace('\s+', ' ', regex=True)
        except:
            print(f"skipping {c} ... not string valued!")
    data["main_text"]  = data["main_text"].apply(lambda x: " ".join(x.split("-")[1:]))
    data=data[(data.main_text.astype(str).str.len()>100)]
    data["lengths"]    = data["main_text"].apply(lambda x: len(x.split(" ")))
    # store clean one
    data.to_csv("clean_df.tsv", sep="\t", index=False)


def get_short_docs(path, max_length=5000):
    print("Getting docs in shorter chunks")
    short_docs = []
    data = pd.read_csv(open(path, 'r'), sep="\t")
    for i,l in data.iterrows():
        txt = [w for w in l["main_text"].split(" ")]
        if len(txt) > max_length:
            start = 0
            end = max_length
            while end < len(txt):
                line = l.copy()
                line["main_text"] = " ".join(txt[start:end])
                line["lengths"] = len(line["main_text"].split(" "))
                short_docs.append(line)
                start+=max_length
                end+=max_length
                # print(len(txt), l["lengths"], start, end, short_docs[-1]["lengths"])
            line = l.copy()
            line["main_text"] = " ".join(txt[start:len(txt)])
            line["lengths"] = len(line["main_text"].split(" "))
            short_docs.append(line)
            # print(len(txt), l["lengths"], start, end, short_docs[-1]["lengths"])
            # print()
            # print()
        else:
            short_docs.append(l)
            
    with open("short_docs.tsv", 'w+') as of:
        pd.DataFrame(short_docs).to_csv(of, index=False, sep="\t")
    


def apply_spacy(path, chunksize=5000):
    print("Applying spacy...") 
    data = pd.read_csv(open(path, 'r'), sep="\t")
      
    start=0
    end=chunksize
    idx=0
    while end < len(data):
        idx+=1
        chunk = data.iloc[start:end].copy()
        
        output = []
        
        for doc in tqdm(nlp.pipe(chunk['main_text'].tolist(), n_process=6),
            total=len(chunk['main_text'])):
            output.append(" . ".join([" ".join([w.lemma_.lower() \
            for w in s if len(w.lemma_)>1 and w.is_alpha and not w.like_num]) for s in doc.sents]))
          
        chunk["spacy"] = output
        print(f"now processing {idx}th chunk ({chunk.shape}) ... up to {end}/{len(data)}")
        with open(f"short_docs_with_spacy_{idx}.tsv", 'w+') as of:
            chunk.to_csv(of, index=False, sep="\t")
        start+=chunksize
        end+=chunksize
        del chunk
    #     
    # idx+=1
    # chunk = data.iloc[start:len(data)-1].copy()
    # print(f"now processing {idx}th chunk ({chunk.shape}) ... up to {end}/{len(data)}")
    # chunk["spacy"] = chunk["main_text"].apply(lambda x: " . ".join([" ".join([w.lemma_.lower() for w in s if len(w.lemma_)>1 and w.is_alpha and not w.like_num]) for s in nlp(x).sents]))
    # 
    # 
    # with open(f"short_docs_with_spacy_{idx}.tsv", 'w+') as of:
    #     chunk.to_csv(of, index=False, sep="\t")
    
    
def tfidf(dir_path, max_df=40000, min_df=1000, stop_words=None, max_features=3000, 
    store_underscores=False):
    
    from spacy.lang.en.stop_words import STOP_WORDS as SPACY_STOP_WORDS
    
    BIGRAMS = [w.strip() for w in open(dir_path + "BIGRAMS.txt", 'r').readlines()]
    BIGRAMS = [' '.join([l.lemma_.lower()  for l in nlp(w)]) for w in BIGRAMS]
    STOPWORDS = set([w.strip() for w in open("STOPWORDS.txt", 'r').readlines()] + list(SPACY_STOP_WORDS))
    
    # print(BIGRAMS)
    
    
    if not os.path.exists(dir_path + "all_with_spacy.tsv"):
        
        print("Creating stacked file")
    
        chunks = [f for f in os.listdir(dir_path) if f.startswith("short_docs_with_spacy_")]
    
        # concatenate them all
        all_chunks = []
        for c in chunks:
            chunk = pd.read_csv(open(os.path.join(dir_path, c), 'r'), sep="\t")
            all_chunks.append(chunk)
        all_chunks = pd.concat(all_chunks)
        all_chunks = all_chunks.dropna(subset=["spacy"])
        
        with open(dir_path + "all_with_spacy.tsv", 'w+') as of:
            all_chunks.to_csv(of, sep="\t", index=False)
            
    print("Loading stacked file")        
    all_chunks = pd.read_csv(open(dir_path + "all_with_spacy.tsv", 'r'), sep="\t")
    
    if store_underscores:
        # create bigrams
        for idx, big in enumerate(BIGRAMS):
            if idx%100==0:
                print(idx)
            all_chunks["spacy"] = all_chunks["spacy"].apply(lambda x: x.replace(" "+big+" ", " "+big.replace(" ", "_")+" "))
        print("bigrams marked!") 
        
        with open(dir_path +  "all_with_spacy.tsv", 'w+') as of:
            all_chunks.to_csv(of, sep="\t", index=False)
    
    FOUND_BIGRAMS=defaultdict(lambda: 0)
    for big in BIGRAMS:
        for l in all_chunks["spacy"].values:
            if big in l:
                FOUND_BIGRAMS[big]+=1
                break
    # print(FOUND_BIGRAMS)
    print("bigrams filtered!")
    
    
    # get & save vocabulary
    tfidf = TfidfVectorizer(max_df=max_df, min_df=min_df, stop_words=STOPWORDS)
    all_lemma_docs = all_chunks["spacy"].to_list()
    tfidf.fit(all_lemma_docs)
    vocab = [w.replace(" ", "_") for w in set(tfidf.get_feature_names()+list(FOUND_BIGRAMS.keys()))]
    with open(dir_path +  f"vocab_{min_df}_{max_df}.txt", 'w+') as vf:
        for v in vocab:
            vf.write(f"{v}\n")
    print("fit and vocab done")
        
        
    # filter lemmas by tfidf vocab
    all_chunks["wbow"] = all_chunks["spacy"].apply(lambda x: " ".join([i for i in (f"{vocab.index(k)}:{v}" for k,v in Counter(x.split(" ")).items() if k in vocab)]))
    print("new column done")
    with open(dir_path +  f"final_{min_df}_{max_df}.tsv", 'w+') as of:
        all_chunks.to_csv(of, index=False, sep="\t")
    
        

def main():
  
    clean_df(main_folder + "_full_downloaded.csv")
    get_short_docs("clean_df.tsv", max_length=5000)
    apply_spacy("short_docs.tsv")
    tfidf("./")
    os.chdir(current_path)
    
    
if __name__ == "__main__":
    main()
                
