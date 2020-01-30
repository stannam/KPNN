import regex as re
from collections import defaultdict

class Corpus(object):
    def __init__(self, name):
        self.name = name
        self.orth, self.freq, self.seg, self.syll = defaultdict(), defaultdict(), defaultdict(), defaultdict()
        self.PNL_seg, self.PNL_syll = [], []

    def update(self, key, orth, freq, seg, syll):
        self.orth[key] = orth
        self.freq[key] = freq
        self.seg[key] = seg
        self.syll[key] = syll


def monomorph(language, test = False):
    monomorph_list = []

    morphemic_analysis_path = "./data/"+language.lower()[0]+"ml.cd"
    phon_path = "./data/"+language.lower()[0]+"pl.cd"

    with open(morphemic_analysis_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()

        for line in lines:
            unit = line.split('\\')
            try:
                MorphStatus = unit[3]

                #cases = [
                #    MorphStatus == "M", #monomorphemic
                #    MorphStatus == "I", #irrelevant (eg., meow)
                #    MorphStatus == "O", # obscure (eg., dedicate)
                #    MorphStatus == "R", # root inside (eg., IMPRIMATUR)
                #    MorphStatus == "U",  # undetermined (eg., hinterland)
                #    MorphStatus == "F"  # lexical flection (eg., anhaltend in german)
                #]

                if (MorphStatus == "M" or MorphStatus == "I" or MorphStatus == "O" or MorphStatus == "R" or
                        MorphStatus == "U" or MorphStatus == "F"):
                    monomorph_list.append(unit[1])

            except IndexError:
                continue


    if test:
        numWords = 100
    else:
        numWords = None

    target_corpus = decipher(path = phon_path, name = language, numWords= numWords, monomorph_list=monomorph_list)
    return target_corpus

def decipher(path, name, numWords=None, monomorph_list=None):
    _corpus = Corpus(name)
    with open(path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
        key = 0
        if numWords is not None:
            lines = lines[:numWords]

        for line in lines:
            unit = line.split('\\')
            try:
                freq = int(unit[2])
                if freq > 0 and unit[1] in monomorph_list:
                    orth = unit[1]
                    if name == "English":
                        phon = unit[5]
                    else:
                        phon = unit[3]
                    phon = re.sub('\'|\"|:', '', phon) # removing vowel length and stress symbols.
                    if len(phon) > 0:
                        seg = re.sub('-', '', phon) # removing syllable separaters.
                        syll = phon.split('-')

                        _corpus.update(key, orth, freq, seg, syll)
                        key += 1

            except IndexError:
                continue
    return _corpus

# get path from outside
# read by line

if __name__ == '__main__':
    result = decipher(path='../data/epl.cd', name="english", numWords=20)
    print(result)
