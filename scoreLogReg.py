# -*- coding: utf-8 -*-
# Compute BIC Scores for Logistic Regression Models
import sys
import itertools as it 
import numpy as np
from math import log, ceil
from collections import deque

from LogRegCPDClass import LogRegCPD
from DataClass import Data
from PruneTreeNodeClass import PruneTreeNodeLogReg, scoreParentSetLogReg
import itertools    
from sklearn.linear_model import LogisticRegression 
from sklearn.preprocessing import PolynomialFeatures 
from scipy.special import gamma

def toNumber(numberList):
    result = 0
    for number in numberList:
        result = result | (1 << number)
    return result     
    
#Check if candidate parent set can be pruned
def notPoisoned(candidateParentSet, poisoned):    
    for key in poisoned.keys():
        if key & toNumber(candidateParentSet) == key and poisoned[key] < len(candidateParentSet):
            return False
    return True

#Check if candidate parent subsets set can be pruned
def checkAdditionToPoisoned(poisoned, candidateParentSet, bic, data, maxSize):
    sizeHorizon = ceil(log(2*bic/log(data.numberOfRows)))
    if (sizeHorizon < maxSize):
        poisoned[toNumber(candidateParentSet)] = sizeHorizon

#Score empty parent set
def scoreNodeWithEmptyParentSet(child, data, scoreName = "BIC"):
    if scoreName == "BIC":
        cpd = data.computeCPD(child, [])
        llTermOfBic = - cpd.getTable(0, []) * log(cpd.getProb(0, [])) - cpd.getTable(1, []) * log(cpd.getProb(1, []))
        penalty = log(data.numberOfRows) / 2
        return llTermOfBic + penalty    
    if scoreName == "BDEU":
        cpd = data.computeCPD(child, [])
        if data.numberOfRows == 0:
            return 0
        bdeuScore = log(gamma(1) / gamma(1+data.numberOfRows))
        bdeuScore = bdeuScore + log(gamma(0.5 + cpd.getTable(0, [])) / gamma(0.5))
        bdeuScore = bdeuScore + log(gamma(0.5 + cpd.getTable(1, [])) / gamma(0.5))
        #print("scoreLogReg.py", "scoreNodeWithEmptyParentSet", child, bdeuScore)
        return -bdeuScore
    print("Unsupported Score Function")
            
            
#Score all candidate Parent Sets of a node
def scoreNode(childNodeIndex, data, maxSize, scoreName = "BIC"):
    
    print("scoring node :", childNodeIndex)
    nullscore = scoreNodeWithEmptyParentSet(childNodeIndex, data, scoreName)
    print("null parent score = ", nullscore)
    totalVar = data.numberOfColumns
    
    parentPowerSet = list(range(totalVar))
    parentPowerSet.remove(childNodeIndex)
    
    scoreMap = {}
    poisoned = {}

    for i in range(1,min(len(parentPowerSet),maxSize)):
        parentPowerSetsi = list(it.combinations(parentPowerSet,i))
        
        for p in parentPowerSetsi:
            candidateParentSet = list(p)
            if notPoisoned(candidateParentSet, poisoned):
                
                #set up logistic regression
                x = d.data[:,candidateParentSet]
                y = d.data[:,childNodeIndex]

                #generate all features
                poly = PolynomialFeatures(interaction_only=True,include_bias = False, degree=len(candidateParentSet)) 
                x=poly.fit_transform(x)
                model = LogisticRegression(solver='liblinear', random_state=0).fit(x, y)
                
                #find names of relevant parent sets and interactions, which are CPD parameters
                nonZeroIndices = np.nonzero(model.coef_)[1]
                nonZeroNames = [poly.get_feature_names()[i].replace('x','').split(' ') for i in nonZeroIndices]
                new_list = [[int(x) for x in lst] for lst in nonZeroNames]
                
                #Generate CPD from logistic model
                logRegCPD = LogRegCPD(d, model.intercept_, new_list, nonZeroIndices, childNodeIndex, candidateParentSet)
                
                #Score candidate parent set
                bic = scoreParentSetLogReg(data, logRegCPD)
                if (bic < 10000):
                    print("parent set:", candidateParentSet, bic)
                else:
                    print("parent set:", candidateParentSet, bic)
                scoreMap[toNumber(candidateParentSet)] = bic
                checkAdditionToPoisoned(poisoned, candidateParentSet, bic, data, maxSize)
            
    return 0  
#Scoring Setup for output files and pruning tree
def score(data , nodeID, maxSize, X, outputfile, outputfile1, scoreName = "BIC"):
    totalVar = data.numberOfColumns
    hm = {}
    queue = deque()
    nextQueue = deque()        
    s = scoreNodeWithEmptyParentSet(nodeID,data,scoreName)
    hm[0] = s        
    
    #Scores
    fo = open(outputfile,"a+")
    output = "   " + str(-s) + " " + str(0) +"\n"
    fo.write(output)
    fo.close()

    #Relevant Feature List
    fo1 = open(outputfile1,"a+")
    output = str(0) +" 0\n"
    fo1.write(output)
    fo1.close()

    print("  Parent set", [], s)

    root = PruneTreeNodeLogReg(hm, nextQueue, None, data, nodeID, totalVar, [], s, X, True, outputfile,outputfile1,scoreName)  
    queue.append(root)
    level = 2
    while (queue or nextQueue):            
        if not queue:
            level = level + 1
            for i in nextQueue:
                queue.appendleft(i)
            nextQueue.clear()
        currentNode = queue.popleft() 
        
        currentNode.createChildren(outputfile, outputfile1)
    print("Done.")
            

# Read the nputs
d = Data()
#try :
d.readFromCSVFile(sys.argv[1])

outfile = sys.argv[3] + "_" + sys.argv[2] + "_" + sys.argv[4]
outfile1 = sys.argv[3] + "_" + sys.argv[2] + "_" + sys.argv[4] + "_params"
fo = open(outfile,"w+")
fo.close()

fo1 = open(outfile1,"w+")
fo1.close()

bf = log(int(sys.argv[4]))

#except :
#print("Usage : python3 scoreLogReg.py path_to_csv_file node_id dataset_name bayes_factor " )



score(d, int(sys.argv[2]), d.numberOfRows, bf, outfile, outfile1, "BDEU")


   



