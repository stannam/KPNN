import regex as re
import sys
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


def han_to_jamo(word):
    split_word = list(word)

    result = list()
    for letter in split_word:
        # If not Korean character, return.
        if re.match('[가-힣]', letter) is not None:
            char_code = ord(letter) - GA_CODE
            if char_code < 0:
                result.append(letter)
            onset = int(char_code / ONSET)
            result.append(ONSET_LIST[onset])

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
    split_jamo = list(jamo)
    cv = cv_tagger(jamo)

    CC = double_coda(jamo, encode=True)

    for c, letter in enumerate(split_jamo):
        if cv[c] == "V":
            vowel = VOWEL_LIST.index(letter)
        elif c == len(split_jamo)-1:
            coda = CODA_LIST.index(letter)
        elif cv[c+1] == "V":
            onset = ONSET_LIST.index(letter)
        else:
            coda = CODA_LIST.index(letter)

    target = (((onset * 21) + vowel) * 28 )+ coda + GA_CODE
    # print("onset: {}, nucleus: {}, coda: {}".format(onset,vowel,coda))
    # print(target)
    return chr(target)


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
    with open("../criteria/double_coda.csv", "r", encoding="utf-8") as f:
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
                        jamo = jamo[:start] + double[c] + jamo[end:]
                except IndexError:
                    jamo = jamo[:start] + double[c] + jamo[end:]
    else:
        for c, double_coda in enumerate(double):
            jamo = re.sub(double_coda, sep[c], jamo)

    #print(jamo)
    return(jamo)

# TODO: 'ㅇ' removal / insertion for the empty onset


if __name__ == '__main__':
    # test full decoding of hangul into jamo
    print(han_to_jamo("밝뚥쀾뱦가웉"))

    # test double coda
    double_coda("ㅂㅣㄺㅇㅜㄾㅑ",False)

    # test jamo-to-hangul
#    jamo_to_han("ㄴㅏㄴ")

    # test hangul-to-jamo
#    if len(sys.argv) > 1:
#        inputfile = open(sys.argv[1], 'r')
#        for line in inputfile.readlines():
#            han_to_jamo(line)
#    else:
#        test_keyword = input("input your text:")
#        han_to_jamo(test_keyword)
