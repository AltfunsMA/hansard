#! .env/bin/python

#%%
from transformers import AutoTokenizer, AutoModelForSequenceClassification, pipeline
import pandas as pd
import spacy
from itertools import chain
import math
from tqdm import tqdm

#%%
tokenizer = AutoTokenizer.from_pretrained("chkla/roberta-argument")

model = AutoModelForSequenceClassification.from_pretrained("chkla/roberta-argument")

df = pd.read_csv("coal_data/04_model_inputs/preprocessed_datasets/all_2a_id.csv")

nlp = spacy.load("en_core_web_sm")

argu_pipe = pipeline(task = "text-classification", model = model, 
tokenizer = tokenizer)


#%%
sample_df = df.sample(n=100)

model_out = []
sent_out = []

batch_num = 50

core_num = min(10, math.ceil(sample_df.shape[0]/batch_num))

for doc in tqdm(nlp.pipe(sample_df['main_text'].to_list(), 
disable = ['lemmatizer', 'ner'],
batch_size = batch_num, 
    n_process = core_num), total=sample_df.shape[0]):

    for sent in doc.sents:

        if len(sent) < 512:
            sent_out.append(sent.text)

        else:
            sent_out.append(NaN)

#%%

res = []

for s in tqdm(sent_out):
    res.append(argu_pipe(s))


#%%
dic_list = list(chain(*model_out))

new_df = pd.DataFrame(dic_list)

new_df['sent'] = sent_out

new_df.label.value_counts()

#%%
new_df.to_csv('argu/first_test.csv', index = False) 




#%%
new_df = pd.concat([sample_df.reset_index(drop = True), 
    pd.DataFrame(dic_list)], axis = 1 )





# %%
