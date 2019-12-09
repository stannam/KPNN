import regex as re
class Corpus(object):
    def __init__(self, name):
        self.name = name
        self.orth, self.freq, self.seg, self.syll = [], [], [], []
        self.PNL_seg, self.PNL_syll = [], []

    def update(self, orth, freq, seg, syll):
        self.orth.append(orth)
        self.freq.append(freq)
        self.seg.append(seg)
        self.syll.append(syll)


def decipher(path, name, numWords=None):
    _corpus = Corpus(name)
    with open(path, 'r', encoding='utf-8') as f:
        lines = f.readlines()

        if numWords is not None:
            lines = lines[:numWords]

        for line in lines:
            unit = line.split('\\')
            try:
                freq = int(unit[2])
                if freq > 0:
                    orth = unit[1]
                    phon = unit[5]
                    phon = re.sub('\'|\"|:', '', phon) # removing vowel length and stress symbols.
                    if len(phon) > 0:
                        seg = re.sub('-', '', phon) # removing syllable separaters.
                        syll = phon.split('-')

                        _corpus.update(orth, freq, seg, syll)

            except IndexError:
                continue
    return _corpus

# get path from outside
# read by line

if __name__ == '__main__':
    result = decipher(path='../data/epl.cd', name="english", numWords=20)
    print(result)
