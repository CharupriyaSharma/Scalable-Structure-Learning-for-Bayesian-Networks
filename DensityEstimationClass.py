# -*- coding: utf-8 -*-
"""
Created on Tue Apr 21 21:22:10 2020

@author: gross
"""

class BIFVariable:
    
    def __init__(self, name, index):
        self.name = name
        self.index = index
        self.parents = []
        self.prob = {}
        
    def addParent(self, parent):
        self.parents.append(parent)
        
    def __parentsToIndex(self, value, parents):    
        result = value
        index = 1
        for parent in parents:
            result = result + parent
            index = index + 1
        return result    
        
    def setProb(self, value, parents, prob):
        key = self.__parentsToIndex(value, parents)
        self.prob[key] = prob
        
    def setValues(self, values):
        self.values = values

    def printVariable(self):
        print("  ", self.name, ", parents =", [ p.name for p in self.parents])
        for key in self.prob.keys():
            parentString = ""
            index = 1
            for p in self.parents:
                parentString = parentString + p.name + "=" + key[index] + ", "
                index = index + 1
            print("    P(", self.name, "=", key[0], "|", parentString, ") =", self.prob[key])

class BIFFile:
    
    def __init__(self, filename):
        file = open(filename, "r")
        lines = file.readlines()
        mode = 0
        self.variables = {}
        index = 0
        for line in lines:
            line = line.strip()            
            if "network" in line:
                continue
            if "variable" in line:                
                tokens = line.split(" ")
                variable = BIFVariable(tokens[1], index)
                index = index + 1
                self.variables[tokens[1]] = variable
                continue
            if "type discrete" in line:
                startIndex = line.find("{")
                endIndex = line.find("}")
                values = line[startIndex+2:endIndex-1].split(", ")
                variable.setValues(values)
                continue
            if "probability" in line:
                tokens = line.split(" ")
                variable = self.variables[tokens[2]]
                if "|" in line:
                    startIndex = line.find("|")
                    endIndex = line.find(")")
                    values = line[startIndex+2:endIndex-1].split(", ")
                    for value in values:
                        variable.addParent(self.variables[value])
                continue
            if "table" in line:
                startIndex = line.find("table")
                endIndex = line.find(";")
                tokens = line[startIndex+6:endIndex].split(", ")                
                variable.setProb(variable.values[0], [], tokens[0])
                variable.setProb(variable.values[1], [], tokens[1])
                continue
            if "(" in line:
                startIndex = line.find("(")
                endIndex = line.find(")")
                tokens = line[startIndex+1:endIndex].split(", ")             
                startIndex = line.find(")")
                endIndex = line.find(";")
                tokens2 = line[startIndex+2:endIndex].split(", ")                      
                variable.setProb(variable.values[0], tokens, tokens2[0])
                variable.setProb(variable.values[1], tokens, tokens2[1])
                continue    
            
    def printVariables(self):
        for key in self.variables.keys():
            print("Variable", key)
            self.variables[key].printVariable()
            
class DensityEstimation:

    def __init__(self, biffilename, observationfilename, debug = False):
        self.bif = BIFFile(biffilename)
        obsfile = open(observationfilename, "r")
        lines = obsfile.readlines()             
        for line in lines[1:]:
            line = line.strip()
            tokens = line.split(",")
            product = 1
            for variable in self.bif.variables.values():                
                key = tokens[variable.index]
                for parent in variable.parents:
                    key = key + tokens[parent.index]
                if debug:    
                    print("Debug", variable.name, 
                          ", index=", variable.index, 
                          ", parents", [p.name for p in variable.parents],
                          ", obs=", tokens[variable.index], 
                          ", parentObs=", key)
                    print("Debug Prob", float(variable.prob[key]))
                product = product * float(variable.prob[key])
            print(product)    
                
                

#class DensityEstimation:
    
test = BIFFile("asia.bif")
test.printVariables()
de = DensityEstimation("asia.bif", "100.csv")
