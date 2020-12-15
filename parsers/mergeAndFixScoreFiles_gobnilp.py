import sys
from math import log
import numpy as np
import os.path

bicfile = sys.argv[1] + ".BIC.20"
freg = open(bicfile, "r")
fmerge = open(sys.argv[1] + ".20.merged", "w+")
#get var count

nstr = freg.readline()
n= int(nstr)

fmerge.write(nstr)

for i in range(n):
    metrics_file = sys.argv[1]+ "_" + str(i) + "_metrics"
    score_file = sys.argv[1]+ "_" + str(i)

    scoresMARS = []
    scoresREG = []
    parentsMARS = []
    parentsREG = []
    
    if(os.path.isfile(score_file)) :
        metrics = np.genfromtxt(metrics_file, skip_header=1)
        
        top_aic =  range(0,len(metrics))
        if (len(metrics) >  200):
            s = max(200, int(len(metrics)))
            top_aic = metrics[:,0].argsort()[-s:][::-1]
            top_aic = [x+1 for x in top_aic]
    

        #read scores from noisy-or file for ithvariable 
        fmars = open(sys.argv[1] + "_" + str(i), 'r')
        f2 = fmars.readlines()
    
        for i in top_aic: 
            line =f2[i]
            items = [float(it) for it in line.split()]
        
            if len([int(p) for p in items[2:]]) > 1 :
                scoresMARS.append(items[0])
                parentsMARS.append([int(p) for p in items[2:]])
        fmars.close()
    
        #get scores for ith variable  
        scorecountstr = freg.readline()
        scorecountitems = [int(it) for it in scorecountstr.split()]
        scorecount = scorecountitems[1]

        for j in range(scorecount):
            line = freg.readline()

            items = [float(it) for it in line.split()]
            parentset = [int(p) for p in items[2:]]
            score = items[0]

            if parentset in parentsMARS :
                conflictid = parentsMARS.index(parentset)
                print (scoresMARS[conflictid] -  score, parentset, parentsMARS[conflictid])
                if score >=  scoresMARS[conflictid] :
                    parentsMARS.pop(conflictid)
                    scoresMARS.pop(conflictid)
                    scoresREG.append(score)
                    parentsREG.append(parentset)
            else:
                scoresREG.append(score)
                parentsREG.append(parentset)

                
            for ps in parentsMARS:
                psindex = parentsMARS.index(ps)
                if set(parentset).issubset(set(ps)) and score >= scoresMARS[psindex]  :
                    parentsMARS.pop(psindex)
                    scoresMARS.pop(psindex)
                
    #write merged scores to file
    totalscorecount = len(scoresREG) + len(scoresMARS)
    fmerge.write( str(i) + " " + str(totalscorecount) + "\n")

    for k in range(len(scoresMARS)) :
        fmerge.write(str(scoresMARS[k])+ " " + str(len(parentsMARS[k])) + " " +  " ".join(str(it) for it in parentsMARS[k]) + "\n")

    for k in range(len(scoresREG)) :
        fmerge.write(str(scoresREG[k])+ " "  + str(len(parentsREG[k])) + " " + " ".join(str(it) for it in parentsREG[k])+ "\n")


freg.close()
fmerge.close()
