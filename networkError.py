import sys
import numpy as np

def readBif(biffile, fmap):
    file1 = open(biffile, "r")
    lines1 = file1.readlines()
    g1={}
    j = -1
    correct = 0
    score = 0
    scoremiss = 0
    scoreextra = 0
    for i in range(0, len(lines1)):
        if not lines1[i].startswith("probability ( "):
            continue
        
        line = lines1[i]
        start = line.find("|")+2
        end = line.find(")")-1
        startc = line.find("(")+1
        endc = line.find("|")
        child1 = line[startc:endc].strip()
        child = child1.replace("v", "")
        if start == 1:
            parents1 = []
            endc=end
            child1 = line[startc:endc].strip()
            child = child1.replace("v", "")

        else:
            parents1 = line[start:end].split(", ");
        if len(parents1)>0 :
            parents1 = [p.replace('v', '') for p in parents1]
        parents = [int(p) for p in parents1]
        g1[int(child)]=parents
        
        j = j + 1
             
    for k, v in g1.items():
        print(k, v)
        p1 = set(g1[k])
        p2 = set(fmap[k])
        correct += len(p1.intersection(p2))
        print(p1.intersection(p2))
    print("correct :")
    print(correct)

def BNfileReader(bnfile,  groundtruthbif):
    freg = open(bnfile, "r")
    lines = freg.readlines()
    
    prefixes = ('\n')
    suffixes = ('BN')

    fmap = {}
    
    for line in lines:
        if line.startswith(prefixes):
            continue
        if line.startswith(suffixes):
            continue  
        if len(line) < 6:
            continue

        #read Graph
        parts = line.split()
        subparts = parts[0].split("<-")
        child = subparts[0]
        if len(subparts) <= 1:
            parentsT = []
        else:
            parentsT = subparts[1].split(",")
        #print(child)    
        #print(parentsT)            
        parents = []
        child = int(child)
        for p in parentsT:
            if p != '':
                parents.append(int(p))
        fmap[child] = parents    
    freg.close()

    readBif(groundtruthbif, fmap)

BNfileReader(sys.argv[1],  sys.argv[2])
        
