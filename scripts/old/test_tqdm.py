#! .env/bin/python

#%%
from numpy.core.fromnumeric import size
import pandas as pd
from tqdm import tqdm
import spacy

df = pd.read_csv("/data/hansard/coal_data/04_model_inputs/coal_single_orator_300tkn_para.csv")

nlp = spacy.load("en_core_web_sm")

#%%
df_s = df.sample(n = 100)

#%%


for date, text in tqdm(zip(df_s['date'], df_s['main_text']),
total= df_s.shape[0]):

    store = nlp(text)


    
# %%
