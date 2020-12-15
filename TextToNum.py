import sys

csvfile = open(sys.argv[1] , "r")
outputfile = open(sys.argv[2], "w")
linenumber = 0
for line in csvfile:    
    linenumber = linenumber + 1
    columns = line.split(",")
    if linenumber == 1:        
        maps = []
        highest = []
        for col in columns:
            mapping = {}
            maps.append(mapping)
            highest.append(0) 
    #    outputfile.write(line)    
    if linenumber > 2:
        columnnumber = 0
        newline = ""
        for col in columns:
            if columnnumber > 0:
                newline = newline + ","
            mapping = maps[columnnumber]
            if col in mapping:
                number = mapping[col]
            else:
                number = highest[columnnumber]
                mapping[col] = number
                highest[columnnumber] = highest[columnnumber] + 1
            newline = newline + str(number)
            columnnumber = columnnumber + 1
        line = newline + "\n"
        outputfile.write(line)
csvfile.close()
outputfile.flush()
outputfile.close()
