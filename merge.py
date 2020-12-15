import sys
from math import log
import numpy as np
import os.path

bicfile = sys.argv[1] + ".BIC.20" 
freg = open(bicfile, "r")
fmerge = open(sys.argv[1] +  ".20.merged", "w+")
fmerge1 = open(sys.argv[1] + ".20.merged.annotated", "w+")
#get var count

nstr = freg.readline()
n= int(nstr)

fmerge.write(nstr)
fmerge1.write(nstr)

for i in range(n):
    print(i)
    metrics_file = sys.argv[1]+ "_" + str(i) + "_metrics"
    score_file = sys.argv[1]+ "_" + str(i)

    scoresNO = []
    scoresREG = []
    parentsNO = []
    parentsREG = []

    if(os.path.isfile(score_file)) :
        metrics = np.genfromtxt(metrics_file, skip_header=1)
        
        top_aic =  range(0,len(np.shape(metrics)))
        if (len(metrics) >  200):
            s = max(200, int(len(metrics)/4))
            top_aic = metrics[:,0].argsort()[-s:][::-1]
            top_aic = [x+1 for x in top_aic]
        #read scores from noisy-or file for ithvariable 
        fno = open(score_file, 'r')
        f2 = fno.readlines()
        nc=0    
        for ta in top_aic :
            nc=1
            line=f2[ta]
            items = [float(it) for it in line.split()]
        
            if len([int(p) for p in items[2:]]) > 1 :
                #print(line)
                scoresNO.append(items[0])
                #print(str(items[1]) + " -> " + str(items[0] + newpenalty) )
                #print(str(log(N)/2*(2^parsize))+ str(" - ") +str(parsize/2 *log(N)) + " -> " +str( newpenalty ))
                parentsNO.append([int(p) for p in items[2:]])
        fno.close()
    #empty file, score null set 
    #parentsNO.append([])
    #scoresNO.append(0)

    #get scores for ith variable  
    scorecountstr = freg.readline()
    scorecountitems = [int(it) for it in scorecountstr.split()]
    scorecount = scorecountitems[1]

    for j in range(scorecount):
        line = freg.readline()

        items = [float(it) for it in line.split()]
        parentset = [int(p) for p in items[2:]]
        score = items[0]
        psregindex = -1

        if parentset in parentsNO and score < 0 and len(items) > 2 :
            conflictid = parentsNO.index(parentset)
            print (scoresNO[conflictid] -  score, parentset, parentsNO[conflictid])
            if score >=  scoresNO[conflictid]:
                parentsNO.pop(conflictid)
                scoresNO.pop(conflictid)
                scoresREG.append(score)
                parentsREG.append(parentset)
                psregindex = parentsREG.index(parentset)
               # print("adding to index :" + str(psregindex))
        elif len(items) > 2 and items[0]>-0.00009:
            #scoresREG.append(-1*log(N)/2.0*pow(2,len(parentset)))
            cv=2
		
        else : 
            scoresREG.append(score)
            parentsREG.append(parentset)
            psregindex = parentsREG.index(parentset)
            #print("adding to index :" + str(psregindex))

        for ps in parentsNO:
                psindex = parentsNO.index(ps)
        
                if set(parentset).issubset(set(ps)) and score >= (scoresNO[psindex] + log(20))  :
                    print(str(parentset) + " is subset of Noisy-OR set" + str(ps) )
                    print(str(score) + " " + str(scoresNO[psindex]))
                    parentsNO.pop(psindex)
                    scoresNO.pop(psindex)

                #if psregindex>-1 and set(ps).issubset(set(parentset)) and (score + log(20))<= scoresNO[psindex]  :
                #    print(str(parentset) + " is superset of Noisy-OR set" + str(ps) )
                #    print(str(score) + " " + str(scoresNO[psindex]))
                #    print(psregindex)
                #    print(len(parentsREG))
                #    parentsREG.pop(psregindex)
                #    scoresREG.pop(psregindex)
                #    break    
                
    #write merged scores to file
    totalscorecount = len(scoresREG) + len(scoresNO)
    fmerge.write( str(i) + " " + str(totalscorecount) + "\n")

    for k in range(len(scoresNO)) :
        fmerge.write(str(scoresNO[k])+ " " + str(len(parentsNO[k])) + " " +  " ".join(str(it) for it in parentsNO[k]) + "\n")

    for k in range(len(scoresREG)) :
        fmerge.write(str(scoresREG[k])+ " "  + str(len(parentsREG[k])) + " " + " ".join(str(it) for it in parentsREG[k])+ "\n")

    fmerge1.write( str(i) + " " + str(totalscorecount) + "\n")

    for k in range(len(scoresNO)) :
        fmerge1.write("1 " + str(scoresNO[k])+ " " + str(len(parentsNO[k])) + " " +  " ".join(str(it) for it in parentsNO[k]) + "\n")

    for k in range(len(scoresREG)) :
        fmerge1.write("0 " + str(scoresREG[k])+ " "  + str(len(parentsREG[k])) + " " + " ".join(str(it) for it in parentsREG[k])+ "\n")
freg.close()
fmerge.close()
fmerge1.close()
