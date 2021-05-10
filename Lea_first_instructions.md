--------------------
./generate_input
--------------------
* final_1000_40000.csv
  --> cleaned-up version of the original hansard dataframe
  --> Fields: -date
              -source
              -main_text (raw, original)
              -spacy: lemmatized main_text (ALL WORDS)
              -tfidf: wID:wCount representation (FILTERED BY TFIDF)
  --> TFIDF: Some dumb stopword filtering
  --> But also created a couple of bigrams based on the Eurovoc data, 
      e.g., "climate_change" is now a 'word', as is "coal_mine" 
      (cf. vocab_1000_40000.txt file for a full list)
  --> And use a domain-specific stopword list
  
* STOPWORDS.txt
  --> hansard-specific stopword list
  
* BIGRAMS.txt
  --> the original list of Eurovoc bigrams I considered
              
* vocab_1000_40000.txt
  --> vocabulary after TFIDF filtering (wID = linenumber-1)
  
* get_final_dataframe_full_lemmas_tfidf.py
  --> generates the two files above 
  --> all docs with >5K words are split into a series of 5K-word docs
    
* to_dtm_input.py
  --> takes vocab* and final* and generates
      the input files required by the DTM (*-seq.dat and *-mult.dat)
  --> uses TFIDF-filtered vocabulary and ignores documents of length<3
      
* to_scan_input.py
  --> takes vocab* and final* and generates
      the input file required by SCAN
  --> uses TFIDF-filtered vocabulary and ignores documents of length<10

  
  
-------------------------
./output_dtm 
-------------------------
** Background **
The DTM takes as input the number of topics (K) and number of time intervals (T)
and for each topic, produces a time-specific distribution over words.
It learns *general* time-specific topics.

Paper: https://icml.cc/Conferences/2016/awards/dtm.pdf
Code : https://github.com/blei-lab/dtm

** Parameters **
- Run on only 10% of the data 
- K=30 (number of topics) and K=10
- T=13 (each time interval corresponds to a decade: 1900-1910, 1911-1920, etc)
- gamma (variance)=0.05 (makes the same topic consistent over time)
- otherwise, defaults

** output format ** 
- Each block=1 topic
- indices 0...12 correspond to decades, in order (1900s, ... 1980s, ... 2010s)

-------------------------
./output_scan
-------------------------
** Background **
Scan takes as input a target term of interest (largely 'coal' I guess, but I also 
played with {renewable, nuclear, solar, climate_change}), the number of topics (K) and
the number of time intervals (T). It creates a target-term centered sub-corpus. It
then learns a time-specific distribution for each topic.
It learns *target-term-specific* time-specific topics.

Paper: 
Code : 

** Parameters **
- K=30 (number of topics)
- T=13 (each time interval corresponds to a decade: 1900-1910, 1911-1920, etc)
- Window size = 15 (document = mention of "coal" within a context of 5 preceding and 5 following words)
- otherwise, defaults

** output format ** 
- Each topic k at time t represented as the 10 words w with highest p(w|k,t) ... in descending order left-to-right
- T=0 ... T=12 are the decates (1900s, 1910s, ... 2010s, 2020)
- First probability per line is p(k|t) the overall prevalence of that topic in a particular decade 
- T=-1 (at the bottom of each "topic block") is the general topic representation, averaged over all years
- K=0 ... K=20 are the 30 topics


** outputs **
k=topics ; w=context_size ; dt=time_interval


-------------------------
./output_word2vec
-------------------------
I trained word embeddings on the full (lemmatized) hansard data set and looked at the nearest neighbors of
Eurovoc terms in some of the Eurovoc "topics" I deemed informative. 

->  word2vec_embeddings_of_eurovoc_terms: output
-> *.ipynb: Code
-> *.tsv: the Eurovoc subset I used





------------------------
some things I've tried
------------------------
- Smaller / larger context for scan. Larger context: more diverse topics. Converging towards 15 words +/-;
  also tried 2 and 5 words windows
- Number of topics: chose 30 based on the German paper, but fewer topics will be easier to interpret. TODO.
- Didn't play much with vocabulary filtering
- 

