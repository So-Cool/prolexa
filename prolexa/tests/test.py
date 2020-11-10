import os
import prolexa_sense

for example in os.path.listdir("data") :
    kb_path = example + "knowledge.txt"
    queries_path = example + "queries.txt"
    answers_path = example + "answers.txt"
    
    with open(kb_path, "r") as f :
        kb = f.read().splitlines()
    with open(queries_path, "r") as f :
        queries = f.read().splitlines()
    with open(answers_path, "r") as f :
        answers = f.read.splitlines()
    
    assert(prolexa_sense.test(kb, queries) == answers)
