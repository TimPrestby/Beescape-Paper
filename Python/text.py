#Import necessary Packages
import pytextrank, spacy, csv, os
import scattertext as st
import numpy as np
import pandas as pd
from scattertext import SampleCorpora, PhraseMachinePhrases, dense_rank, RankDifference, AssociationCompactor, produce_scattertext_explorer
from scattertext.CorpusFromPandas import CorpusFromPandas


abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname)

##Open the CSV file 
df_text = pd.read_csv('Qualitative_PY.csv')
df_text.head()

#Load shaply in english 
nlp = spacy.load("en")

#Create a corpus to visualize the data
def term_freq(df_text):
    #####Single phrase######

    corpus = (st.CorpusFromPandas(df_text, category_col='q', text_col='text', nlp=nlp)
        .build()
        #Remove the stop words
        .remove_terms(nlp.Defaults.stop_words, ignore_absences=True)
    )
    term_category_scores = corpus.get_metadata_freq_df('')
    #Get a list of common words
    #print(list(corpus.get_scaled_f_scores_vs_background().index[:10]))

    ##Create HTML page backbone
    html = st.produce_scattertext_explorer(corpus,
        category='Q1',
        category_name='Question One',
        not_category_name='Question Two',
        width_in_pixels=750)

    #Output the HTML page
    html_file_name = "Beescape-Scattertext.html"
    open(html_file_name, 'wb').write(html.encode('utf-8'))

term_freq(df_text)
print('done')