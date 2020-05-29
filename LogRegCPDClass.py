# -*- coding: utf-8 -*-
"""
Created on Thu Feb 13 21:46:22 2020

"""

from math import exp
import numpy as np

class LogRegCPD:
        
    # Create with parent set
    def __init__(self, data, betaZero, names, coefficients, nonzeros, child, parents):
        self.coefficients = coefficients
        self.data = data
        self.child = child
        self.parents = parents
        self.dimensions = [2]
        for parent in parents:
            self.dimensions.append(2)            
        self.prob = np.ndarray(shape=self.dimensions)
        self.prob.fill(0)
        self.table = np.ndarray(shape=self.dimensions)
        self.table.fill(0)          
        
        
        for row in data.data:
            self.__add(row, child, parents)   
        
        if self.parents == []:
            probOne = exp(betaZero) / (exp(betaZero) + 1)
            self.setProb(0, [], 1 - probOne)
            self.setProb(1, [], probOne)
            return
        indices = self.__createParentSetIndices()

        
        while (indices != []):
            exponent = betaZero
                        
            for index in range(0,len(coefficients)):
                termActive = True
                for i in names[index]:
                    if indices[i] == 0:
                        termActive = False
                        break
                if termActive:
                    #if parents == [1,2]:
                    #print("LogRegCPD.adding", coefficients[index])
                    exponent = exponent + coefficients[index]    
            
            #for i in range(len(indices)):
            #    if indices[i] == 1:
            #        exponent = exponent + coefficients[parents[i]]
            try:
                probIsOne = exp(exponent) / (exp(exponent) + 1)
            except OverflowError:
                print(exponent)
                probIsOne = 1
            probIsZero = 1 - probIsOne
            #if parents == [1,2]:
            #print("LogRegCPD.setProb", 1, indices, probIsOne, "betaZero", betaZero, coefficients)
            #print("LogRegCPD.setProb", 0, indices, probIsZero,"betaZero",  betaZero, coefficients)
            self.setProb(1, indices, probIsOne)
            self.setProb(0, indices, probIsZero) 
            
            indices = self.__incrementParentSetIndices(indices)

    
    def __add(self, row, childNode, parentSet):
        if parentSet == []:
            self.table[row[childNode]] = self.table[row[childNode]] + 1
        else:   
            self.__addTo(self.table[row[childNode]], row, parentSet, 0)
        
    def __addTo(self, a, row, parentSet, index):
        if index == len(parentSet)-1:            
            a[row[parentSet[index]]] = a[row[parentSet[index]]] + 1
        else:
            self.__addTo(a[row[parentSet[index]]], row, parentSet, index+1)         
    
    def __createParentSetIndices(self):
        indices = []
        for d in self.parents:
            indices.append(0)
        return indices    

    def __incrementParentSetIndices(self, indices):
       i = len(indices)-1
       while (i >= 0 and indices[i] == 1):
           indices[i] = 0
           i = i-1
       if i == -1:
           return []
       else:
           indices[i] = indices[i] + 1
           return indices

            
    def getProb(self, childValue, parentValues):
        partProb = self.prob[childValue]
        for value in parentValues:
            partProb = partProb[value]
        return partProb
    
    def setProb(self, childValue, parentValues, p):
        if parentValues == []:
            self.prob[childValue] = p
            return
        partProb = self.prob[childValue]
        i = 0
        for value in parentValues:
            i = i+1
            if i < len(self.parents):                 
                partProb = partProb[value]
            else:
                partProb[value] = p
            
    def getTable(self, childValue, parentValues):
        partTable = self.table[childValue]
        for value in parentValues:
            partTable = partTable[value]
        return partTable               
            
    def __indicesToStr(self, indices):
        line = "("
        first = True
        for i in indices:
            if first:
                first = False
            else:
                line = line + ", "
            if i == 1:
                line = line + "yes"
            else:
                line = line + "no"                
        return line + ")"
            
    def printCPD(self, outfile):  
        fo = open(outfile, "a")
        if self.parents == []:
            fo.write("  table 0.5, 0.5; \n")
            return
        indices = self.__createParentSetIndices()
        while (indices != []):
            line = "  " + self.__indicesToStr(indices) + " " + str(self.getProb(0, indices)) + ", " + str(self.getProb(1, indices)) + ";"                
            fo.write(line + "\n")
            indices = self.__incrementParentSetIndices(indices)
        fo.close()
            
    def __str__(self):
        result = "Conditional Probability Distribution for " + str(self.child) + " with parent set " + str(self.parents) + ":"
        if self.parents == []:        
            for childValue in [0,1]:
                line = "\n      P(X_"+str(self.child)+"="+str(childValue)
                line = line + ") = " + str(self.getProb(childValue,[]))
                result = result + line
            return result     
        result = "Conditional Probability Distribution for " + str(self.child) + " with parent set " + str(self.parents) + ":"
        indices = self.__createParentSetIndices()
        while (indices != []):
            for childValue in [0,1]:
                line = "\n      P(X_"+str(self.child)+"="+str(childValue)+" | "
                i = 0
                for value in indices:
                    line = line + "X_"+str(self.parents[i])+"="+str(value)
                    i = i + 1
                    if i < len(indices):
                        line = line + ", "
                line = line + ") = " + str(self.getProb(childValue,indices))
                result = result + line
            indices = self.__incrementParentSetIndices(indices)
        return result            
