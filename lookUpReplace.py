#########################################################################################################
# BC2407 Semester Analytics Project: Look Up to Replace Terms using Synonym Dataset
# Team Number 8
# Members: Christopher Gerard Toh, Teo Tian Shun Kenneth, Lim De Quan, Jonathan Kevin Chandra
# Datasets: skills_synonyms.csv, skills_td_cut.csv
# Library: NumPy, Pandas, re, collections, csv
####################################################################################################

import numpy as np
import pandas as pd
import re
import collections
import csv


#import files
skills_synonyms = pd.read_csv("skills_synonyms.csv")
skills_td_cut = pd.read_csv("skills_td_cut.csv")


#Create a Synonym dictionary
skills_synonyms["sub"] = skills_synonyms["sub"].apply(lambda x: x.split(";"))
skills_synonyms.head()
skills_dict = (skills_synonyms.set_index('main').T.to_dict('index'))['sub']
skills_dict

changedWord = []

def findSynonym(value):
    for root, synonymArr in skills_dict.items():
        for synonym in synonymArr:
            if (synonym == value.lower()):
                return root
    return value

# For each word in skills_td_cut swap the value of the synonym to the root word
for value in skills_td_cut['word']:
    changedWord.append(findSynonym(value))

#overwrite the word with the changed word
skills_td_cut["word"] = changedWord

#write into csv file
skills_td_cut.to_csv("skills_td_cut_syn.csv", index = False)

#Go back to Data
