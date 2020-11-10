#! /usr/bin/env python

def get_wordnet_nltk():
    import nltk
    nltk.download('wordnet')

def get_pos_flair():
    from flair.models import SequenceTagger
    SequenceTagger.load('pos')

if __name__ == '__main__':
    get_wordnet_nltk()
    get_pos_flair()
