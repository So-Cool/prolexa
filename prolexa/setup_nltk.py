#! /usr/bin/env python

import nltk

def get_wordnet():
    nltk.download('wordnet')

def setup_flair():
    from flair.models import SequenceTagger
    SequenceTagger.load('pos')

if __name__ == '__main__':
    get_wordnet()
    setup_flair()
