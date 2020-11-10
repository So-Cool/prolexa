#! /usr/bin/env python

import os

from pyswip import Prolog

import prolexa.meta_grammar as meta

TEST_PATH = os.path.dirname(os.path.realpath(__file__))

pl = Prolog()

def test_prolexa_plus():
    test_data_path = os.path.join(TEST_PATH, 'data')

    for example in os.listdir(test_data_path) :
        kb_path = os.path.join(test_data_path, example, 'knowledge.txt')
        with open(kb_path, 'r') as f :
            kb = f.read().splitlines()

        queries_path = os.path.join(test_data_path, example, 'queries.txt')
        with open(queries_path, 'r') as f :
            queries = f.read().splitlines()

        answers_path = os.path.join(test_data_path, example, 'answers.txt')
        with open(answers_path, 'r') as f :
            answers = f.read().splitlines()

        # assert prolexa_sense.test(kb, queries) == answers
        meta.reset_grammar()
        meta.initialise_prolexa(pl)

        for fact in kb:
            meta.standardised_query(pl, fact)
        for question, answer in zip(queries, answers):
            answer_ = meta.standardised_query(pl, question)[0]['Output']
            print(answer_)
            print(answer)
            assert answer_ == answer

if __name__ == '__main__':
    test_prolexa_plus()
