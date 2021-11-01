#! .env/bin/python 

#%%
import pandas as pd
import pickle as pkl
from collections import defaultdict
import os

main_topic = os.getenv('HANSARD_MAIN', 'coal')

print("lemmatising input for: ", main_topic)

data = pd.read_csv(main_topic + "_data/04_model_inputs/" + main_topic + "_full_downloaded.csv")

lemmas_outpath = main_topic + "_data/04_model_inputs/final_dtm_lemmas.df"

vocab_outpath = main_topic + "_data/04_model_inputs/vocab.txt"


#%%
# filter columns, nan rows, and rows where main text doesn't contain 
# '-' as this seems to be used to indicate start of speech
# split off boilerplate into "main_text_0"
txt              = data[["main_text", "date", "source"]].dropna()
txt["main_text"] = txt["main_text"].str.replace("\n", "")
txt              = txt[txt["main_text"].str.contains("-")]

txt["main_text_0"] = txt["main_text"].str.split("-").str[0]
txt["main_text"]   = txt["main_text"].apply(lambda x: " ".join(x.split("-")[1:]))
txt["main_text"]   = txt["main_text"].apply(lambda x: " ".join(x.split(" ")[:min(len(x.split(" ")), 500)]))
   
#%%    
### Get lemmas -> Spacy 
import spacy 

nlp = spacy.load("en_core_web_sm")
nlp.remove_pipe("tagger")
nlp.remove_pipe("parser")
nlp.remove_pipe("ner")
nlp.add_pipe(nlp.create_pipe('sentencizer'))

print("now applying spacy...")
docs = list(txt["main_text"])
dates = list(txt["date"])
sources = list(txt["source"])
nlpdocs = []
for doc in docs:
    nlpdocs.append(nlp(doc))
pkl.dump(nlpdocs, open(main_topic + "_data/04_model_inputs/spacydocs.pkl", 'wb+'))
#nlpdocs = pkl.load(open("spacydocs.pkl", 'rb'))
    

    
#%%    
### map to lemmas and remove spaces and numbers
print("now mapping to lemmas...")
lemmas = []

for doc in nlpdocs:
    ldoc = [w.lemma_.lower() \
        for s in doc.sents for w in s \
        if len(w.lemma_) > 1 and w.is_alpha and not w.like_num]
    lemmas.append(" ".join(ldoc))

pkl.dump(lemmas, open(main_topic + "_data/04_model_inputs/lemmas.pkl", 'wb+'))
# del nlpdocs
#lemmas = pkl.load(open("lemmas.pkl", 'rb'))
     

#%%
## TFIDF vectorizer
from sklearn.feature_extraction.text import TfidfVectorizer

tfidf = TfidfVectorizer(max_df=0.9, min_df=0.005, stop_words='english')
tfidf.fit(lemmas)
vocab = tfidf.get_feature_names()

print("now creating dtmdocs with tfidf-filtered words")
dtm_docs = []
for idx, doc in enumerate(lemmas):
    if idx%5000==0:
        print(f"...now doc {idx} / {len(lemmas)}")
    this_doc = defaultdict(lambda: 0)
    for w in doc.split(" "):
        if w in vocab:  
            this_doc[vocab.index(w)] += 1
    dtm_docs.append([f"{k}:{v}" for k,v in this_doc.items()])
    
pkl.dump(dtm_docs, open(main_topic + "_data/04_model_inputs/dtm_docs.pkl", 'wb+'))
#dtm_docs = pkl.load(open("dtm_docs.pkl", 'rb'))



    
print("Now storing in...", lemmas_outpath)
## Store back into DF
dfdict = {"date": dates,
        "source": sources, 
        "lemmas": lemmas,
        "tfidf": dtm_docs}

df = pd.DataFrame(dfdict)
df.to_csv(open(lemmas_outpath, 'w+'), index=False)

print("and in...", vocab_outpath)
with open(vocab_outpath, 'w+') as vf:
    for i,v in enumerate(tfidf.get_feature_names()):
        vf.write(f"{v}\n")

