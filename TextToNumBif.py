import sys

biffile = open(sys.argv[1] , "r")
outputfile = open(sys.argv[2], "w")
linenumber = 0
variableindex = 0
# Mapping of variable names
variablemapping = {}
# List of mappings of variable values
valuemappings = []
parents = []
for line in biffile:    
    linenumber = linenumber + 1
    cleanedLine = line.strip()
    tokens = cleanedLine.split(" ")
    newline = line
    #print(cleanedLine + " | " + tokens[0])
    # Rename variables to vX scheme
    if tokens[0] == "variable":
        newline = "variable v" + str(variableindex) +  " {\n"
        variablemapping[tokens[1]] = variableindex
        variableindex = variableindex + 1
        valuemappings.append({})        
    # Rename variable values
    if tokens[0] == "type":
        newline = "  type discrete [ " + tokens[3] + " ] { "
        valueindex = 0
        for index in range(6, len(tokens)-1):
            value = tokens[index]
            if index < len(tokens)-2:
                value = value[:-1]     
            #print(str(index) + " " + value + " " + str(len(tokens)))    
            mapping = valuemappings[variableindex-1]
            mapping[value] = valueindex
            newline = newline + str(valueindex)
            valueindex = valueindex + 1            
            if index < len(tokens)-2:
                newline = newline + ", "
        newline = newline + " };\n"
    if tokens[0] == "probability":
        newline = "probability ( v" + str(variablemapping[tokens[2]])
        parents = []
        if tokens[3] == ")":
            newline = newline + " ) {\n"
        else:
            newline = newline + " | "
            for index in range(4, len(tokens)-2):
                value = tokens[index]
                if index < len(tokens)-3:
                    value = value[:-1]            
                parents.append(variablemapping[value])
                newline = newline + "v" + str(variablemapping[value])
                if index < len(tokens)-3:
                    newline = newline + ", "
            newline = newline + " ) {\n"
    if tokens[0].startswith("("):
        vartokens = cleanedLine.split(")")
        buffer = vartokens[0]
        buffer = buffer[1:]
        varnames = buffer.split(", ")        
        newline = "  ("
        for index in range(0, len(varnames)):
            mapping = valuemappings[parents[index]]
            newline = newline + str(mapping[varnames[index]])
            if index < len(varnames) - 1:
                    newline = newline + ", "
        newline = newline + ")" + vartokens[1] + "\n"
    #if linenumber == 1:        
    #    maps = []
    #    highest = []
    #    for col in columns:
    #        mapping = {}
    #        maps.append(mapping)
    #        highest.append(0) 
    #    outputfile.write(line)    
    #if linenumber > 2:
    #    columnnumber = 0
    #    newline = ""
    #    for col in columns:
    outputfile.write(newline)
biffile.close()
outputfile.flush()
outputfile.close()
