from kPNNpy.fileIO import *
from kPNNpy.pn_list import *
import pickle

def main():
    test = input("is it a test? (Y/N)")
    if test == "Y":
        english = monomorph("English", test = True)
        german = monomorph("German", test = True)
        dutch = monomorph("Dutch", test=True)
    else:
        english = monomorph("English", test=False)
        german = monomorph("German", test=False)
        dutch = monomorph("Dutch", test=False)

    ready_for_igraph_eng = looping(english)
    pickle.dump(ready_for_igraph_eng, open("./ready_for_igraph_eng.pickle", 'wb'))
    ready_for_igraph_ger = looping(german)
    pickle.dump(ready_for_igraph_ger, open("./ready_for_igraph_ger.pickle", 'wb'))
    ready_for_igraph_dut = looping(dutch)
    pickle.dump(ready_for_igraph_dut, open("./ready_for_igraph_dut.pickle", 'wb'))



if __name__ == '__main__':
    main()