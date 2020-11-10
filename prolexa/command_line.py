import prolexa.setup_models as setup_models_
import prolexa.prolexa_plus as prolexa_plus_

def setup_models():
    setup_models_.get_wordnet_nltk()
    setup_models_.get_pos_flair()

def prolexa_plus():
    prolexa_plus_.prolexa_plus_repl()
