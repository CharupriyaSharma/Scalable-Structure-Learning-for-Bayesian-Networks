import sys
from math import log
import numpy as np
import os.path

fmerge = open(sys.argv[1] +  ".20.merged", "w+")

nstr = sys.argv[2]
n= int(nstr)

fmerge.write(nstr+"\n")

for i in range(n):
    print(i)
    score_file = sys.argv[1]+ "_" + str(i)

    scoresNO = []
    parentsNO = []

    if(os.path.isfile(score_file)) :
        #read scores from noisy-or file for ithvariable 
        fno = open(score_file, 'r')
        f2 = fno.readlines()
        for line in f2: 
            items = [float(it) for it in line.split()]

            if len([int(p) for p in items[2:]]) >= 0 and len([int(p) for p in items[2:]]) <=7 :
                #print(line)
                scoresNO.append(-1*items[0])
                #print(str(items[1]) + " -> " + str(items[0] + newpenalty) )
                #print(str(log(N)/2*(2^parsize))+ str(" - ") +str(parsize/2 *log(N)) + " -> " +str( newpenalty ))
                parentsNO.append([int(p) for p in items[2:]])
        fno.close()
    else :
        parentsNO.append([])
        scoresNO.append(0)
    if  [] not in parentsNO :
        parentsNO.append([])
        scoresNO.append(0)
    #write merged scores to file
    totalscorecount = len(scoresNO)
    fmerge.write( str(i) + " " + str(totalscorecount) + "\n")

    for k in range(len(scoresNO)) :
        fmerge.write(str(scoresNO[k])+ " " + str(len(parentsNO[k])) + " " +  " ".join(str(it) for it in parentsNO[k]) + "\n")


fmerge.close()
