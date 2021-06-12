from igraph import Graph
from scipy.stats import skew
import pickle

def main():
    # load pickles files of English, German and Dutch
    english = pickle.load(open('ready_for_igraph_eng.pickle','rb'))
    german = pickle.load(open('ready_for_igraph_ger.pickle','rb'))
    dutch = pickle.load(open('ready_for_igraph_dut.pickle','rb'))

    # first we need to generate a pnn 'g'
    eng_p, eng_s = twoGraphs(english)
    ger_p, ger_s = twoGraphs(german)
    dut_p, dut_s = twoGraphs(dutch)

    allgraphs = [eng_p, eng_s, ger_p, ger_s, dut_p, dut_s]
    allReturn = ["English P: ","English S: ","German P: ","German S: ","Dutch P: ","Dutch S: "]

    # degree distribution
    allDegree = calDegree(allgraphs)

    print("[RESULTS --- DEGREE SKEWNESS]")
    for n, deg in enumerate(allDegree):
        print(allReturn[n], deg)

    # AMD
    allAMD = calAMD(allgraphs)

    print("[RESULTS --- DEGREE ASSORTATIVITY]")
    for n, amd in enumerate(allAMD):
        print(allReturn[n],amd)


def calDegree(list_of_graph):
    list_of_degree = []
    for g in list_of_graph:
        degree = g.indegree() # calculate degree of g -> degree
        skewness = skew(degree)
        list_of_degree.append(skewness)

    return list_of_degree

def calAMD(list_of_graph):
    list_of_AMD = []

    for g in list_of_graph:
        AMD = g.assortativity_degree(directed = False)
        list_of_AMD.append(AMD)

    return list_of_AMD

def twoGraphs(corpus):

    vertices = corpus.orth
    pEdges = corpus.PNL_seg #edges for phoneme PNN
    sEdges = corpus.PNL_syll # edges for syllable PNN

    g_phoneme = Graph(vertex_attrs={'name': vertices},
                      edges=pEdges,
                      directed=False)
    g_syll = Graph(vertex_attrs={'name': vertices},
                   edges=sEdges,
                   directed=False)

    return g_phoneme, g_syll


if __name__ == '__main__':
    main()