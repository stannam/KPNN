import regex as re
import sys
import os

GA_CODE, G_CODE, ONSET, CODA = 44032, 12593, 588, 28
# The unicode representation of the Korean syllabic orthography starts with GA_CODE
# The unicode representation of the Korean phonetic (jamo) orthography starts with G_CODE

# ONSET LIST. 00 -- 18
ONSET_LIST = ['ㄱ', 'ㄲ', 'ㄴ', 'ㄷ', 'ㄸ', 'ㄹ', 'ㅁ', 'ㅂ', 'ㅃ', 'ㅅ', 'ㅆ', 'ㅇ', 'ㅈ', 'ㅉ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ']

# VOWEL LIST. 00 -- 20
VOWEL_LIST = ['ㅏ', 'ㅐ', 'ㅑ', 'ㅒ', 'ㅓ', 'ㅔ', 'ㅕ', 'ㅖ', 'ㅗ', 'ㅘ', 'ㅙ', 'ㅚ', 'ㅛ', 'ㅜ', 'ㅝ', 'ㅞ', 'ㅟ', 'ㅠ', 'ㅡ', 'ㅢ',
                 'ㅣ']

# CODA LIST. 00 -- 27 + 1 (1 for open syllable)
CODA_LIST = ['', 'ㄱ', 'ㄲ', 'ㄳ', 'ㄴ', 'ㄵ', 'ㄶ', 'ㄷ', 'ㄹ', 'ㄺ', 'ㄻ', 'ㄼ', 'ㄽ', 'ㄾ', 'ㄿ', 'ㅀ', 'ㅁ', 'ㅂ', 'ㅄ', 'ㅅ',
                 'ㅆ', 'ㅇ', 'ㅈ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ']


def han_to_jamo(word, empty_onset=False):
    split_word = list(word)

    if not empty_onset:
        onset_list = ONSET_LIST[:]
        onset_list[11]=''

    result = list()
    for letter in split_word:
        # If not Korean character, return.
        if re.match('[가-힣]', letter) is not None:
            char_code = ord(letter) - GA_CODE
            if char_code < 0:
                result.append(letter)
            onset = int(char_code / ONSET)
            result.append(onset_list[onset])

            vowel = int((char_code - (ONSET * onset)) / CODA)
            result.append(VOWEL_LIST[vowel])

            coda = int((char_code - (ONSET * onset) - (CODA * vowel)))
            result.append(CODA_LIST[coda])
        else:
            result.append(letter)
    jamo = "".join(result)
    jamo = double_coda(jamo, encode=False)
    return(jamo)

def jamo_to_han(jamo):
    jamo = re.sub(" ", "", jamo)
    jamo = double_coda(jamo, encode=True)
    cv = cv_tagger(jamo)

    # insert empty codas in between VV sequences
    if cv[0] =="V":
        jamo = "ㅇ" + jamo
        cv.insert(0, "C")

    cv_combined = ''.join(cv)
    while re.search("VV", cv_combined):
        VV = re.search("VV", cv_combined)
        start, end = VV.span()
        cv_combined = cv_combined[:(start+1)] + "C" + cv_combined[(end-1):]
        jamo = jamo[:(start+1)] + "ㅇ" + jamo[(end-1):]

    split_jamo = list(jamo)
    cv = list(cv_combined)

    result = []
    onset = -1

    for c, letter in enumerate(split_jamo):
        if cv[c] == "V":
            vowel = VOWEL_LIST.index(letter)
        elif c == len(split_jamo)-1:
            coda = CODA_LIST.index(letter)
        elif cv[c+1] == "V":
            if onset > -1: # check if this is the first syllable. if not, add syllable to the result
                try:
                    coda
                except UnboundLocalError:
                    coda = 0

                syllable = chr((((onset * 21) + vowel) * 28) + coda + GA_CODE)
                result.append(syllable)
                del coda, vowel
            onset = ONSET_LIST.index(letter)
        else:
            coda = CODA_LIST.index(letter)

    try:
        coda
    except UnboundLocalError:
        coda = 0
    syllable = chr((((onset * 21) + vowel) * 28) + coda + GA_CODE)
    result.append(syllable)
    result = ''.join(result)
    # print("onset: {}, nucleus: {}, coda: {}".format(onset,vowel,coda))
    # print(target)
    return result


def cv_tagger(jamo):
    result = []
    for character in list(jamo):
        if character in VOWEL_LIST:
            result.append("V")
        else:
            result.append("C")
    return result

def double_coda(jamo, encode=False):
    double, sep = [], []
    if hasattr(sys, "frozen"):
        bundle_dir = getattr(sys, '_MEIPASS', os.path.abspath(os.path.dirname(__file__)))
        path = os.path.join(bundle_dir, 'criteria', 'double_coda.csv')
    else:
        path = os.path.join(os.getcwd(), 'criteria', 'double_coda.csv')

    with open(path, "r", encoding="utf-8") as f:
        next(f)
        for line in f:
            items = line.split(",")
            double.append(items[0])
            sep.append(items[1])
        #print(double)
        #print(sep)

    cv = cv_tagger(jamo)

    if encode:
        for c, separated in enumerate(sep):
            targets = re.finditer(separated, jamo)
            for target in targets:
                start, end = target.span()
                try:
                    if cv[end] == "V":
                        continue
                    else:
                        jamo = jamo[:start] + double[c] + "X" + jamo[end:]
                except IndexError:
                    jamo = jamo[:start] + double[c] + "X" + jamo[end:]
    else:
        for c, double_coda in enumerate(double):
            jamo = re.sub(double_coda, sep[c], jamo)

    #print(jamo)
    jamo = jamo.replace('X', '')
    return(jamo)


if __name__ == '__main__':
    # test full decoding of hangul into jamo
    print(han_to_jamo("오빠"))

    # test double coda
#    print(double_coda("ㅂㅣㄺㅜㄾㅑ", False))

    # test jamo-to-hangul
    print(jamo_to_han("ㅗㅃㅏ"))

    # test hangul-to-jamo
#    if len(sys.argv) > 1:
#        inputfile = open(sys.argv[1], 'r')
#        for line in inputfile.readlines():
#            han_to_jamo(line)
#    else:
#        test_keyword = input("input your text:")
#        han_to_jamo(test_keyword)
