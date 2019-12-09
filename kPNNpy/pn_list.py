from itertools import combinations
from editdistance import eval

def looping(Corpus):
    seg_pairs = combinations(Corpus.seg, 2)
    Corpus.PNL_seg = genPNPair(seg_pairs)

    syll_pairs = combinations(Corpus.syll, 2)
    Corpus.PNL_syll = genPNPair(syll_pairs)

    return Corpus

def genPNPair(pairs):

    neighbourPairs = []

    for p in pairs:
        w1 = p[0]
        w2 = p[1]

        if (len(w1) > (len(w2) - 2)) & (len(w1) < (len(w2) + 2)):
            ed = eval(w1, w2)

            if ed == 1:
                #print((p[0], p[1]))
                neighbourPairs.append((p[0], p[1]))

    return neighbourPairs


def edit_dist(str1, str2):
    # edit distance as a dynamic programming problem
    # example code from https://www.geeksforgeeks.org/edit-distance-dp-5/
    m = len(str1)
    n = len(str2)

    dp = [[0 for x in range(n + 1)] for x in range(m + 1)]

    for i in range(m + 1):
        for j in range(n + 1):

            if i == 0:
                dp[i][j] = j  # Min. operations = j

            elif j == 0:
                dp[i][j] = i  # Min. operations = i

            elif str1[i - 1] == str2[j - 1]:
                dp[i][j] = dp[i - 1][j - 1]

            else:
                dp[i][j] = 1 + min(dp[i][j - 1],  # Insert
                                   dp[i - 1][j],  # Remove
                                   dp[i - 1][j - 1])  # Replace

    return dp[m][n]

if __name__ == '__main__':
    pass

#    code_to_test = '''
#str1 = ["f","s","f","f","v","f","d","s","b","b","d","f","v","v","d","a","v","a","v","a","v"]
#str2 = ["sa", "tur", "day"]
#    '''
#    for i in [100, 1000, 10000, 100000, 1000000, 10000000]:
#        t_eval = timeit.timeit(code_to_test+'\neval(str1,str2)',
#                               "from editdistance import eval",
#                               number=i)
#        print(str(i) + "\t" + str(t_eval))
#
    #t1 = timeit.timeit('edit_dist([["say"], ["tur"], ["day"]], [["sa"], ["tur"], ["day"]])',
    #                  "from __main__ import edit_dist",
    #                  number=1000)
    #print(t1)

