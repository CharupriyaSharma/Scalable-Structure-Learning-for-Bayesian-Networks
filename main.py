import argparse
import os.path
import re
import statistics
import sys

# Reads a biffile and returns a map: child -> list of parents. Children and parents are ints.
def readBif(biffile):
    file1 = open(biffile, "r")
    lines1 = file1.readlines()
    g1 = {}
    j = -1
    for i in range(0, len(lines1)):
        if not lines1[i].startswith("probability ( "):
            continue

        line = lines1[i]
        start = line.find("|") + 2
        end = line.find(")") - 1
        startc = line.find("(") + 1
        endc = line.find("|")
        child1 = line[startc:endc].strip()
        child = child1.replace("v", "")
        if start == 1:
            parents1 = []
            endc = end
            child1 = line[startc:endc].strip()
            child = child1.replace("v", "")

        else:
            parents1 = line[start:end].split(", ")
        if len(parents1) > 0:
            parents1 = [p.replace('v', '') for p in parents1]
        parents = [int(p) for p in parents1]
        g1[int(child)] = parents

        j = j + 1

    return g1

def splitRDABIFfileLine(line):
    if not ':' in line:
        print("splitBNfileLine should only be called with a line containing : but line =", line)
        return 0, []
    parts = re.split(':', line)
    return int(parts[0].strip()), [int(s.strip()) for s in re.split("\s+", parts[1]) if not s == ""]

# Reads a RDABIFfile and returns a maps: child -> list of parents.
# All children and parents are ints.
def readRDABIFfile(filename):
    freg = open(filename, "r")
    lines = freg.readlines()
    fmap = {}
    for child, parents in [splitRDABIFfileLine(line) for line in lines if ":" in line]:
        fmap[child] = parents
        #print(child, parents)
    freg.close()
    return fmap

def print_child_parent_map(mapping):
    for child, parents in mapping.items():
        print(child, "<-", parents)

# Splits a BNfile line containing a '<-' into its components (child, parents, score) and returns them as int / list of
# ints, double.
def splitBNfileLine(line):
    if not '<-' in line:
        print("splitBNfileLine should only be called with a line containing <- but line =", line)
        return 0, [], 0
    parts = re.split('<-| ', line)
    return int(parts[0]), [int(s) for s in parts[1].split(",")[0:-1]], parts[2].strip()

# Reads a BNfile and returns two maps: child -> list of parents and child -> score.
# All children and parents are ints, scores are doubles.
def readBNfile(filename):
    freg = open(filename, "r")
    lines = freg.readlines()
    fmap = {}
    smap = {}
    for child, parents, score in [splitBNfileLine(line) for line in lines if "<-" in line]:
        fmap[child] = parents
        smap[child] = score
        #print(child, parents, score)
    freg.close()
    return fmap, smap

# Checks a Gobnilp solution versus the ground truth bif file and prints the number of correct parents.
def checkGobnilpSolution(BNfile, groundtruthbiffile):
    bifmap = readBif(groundtruthbiffile)
    bnmap, _ = readBNfile(BNfile)
    additional_edges = 0
    correct_edges = 0
    flipped_edges = 0
    missing_edges = 0
    print("Comparing GOBNILP solution with ground truth network:")
    print("GOBNILP solution:", BNfile)
    print("Ground truth BIF:", groundtruthbiffile)
    for child, v in bifmap.items():
        ground_truth = set(bifmap[child])
        gobnilp = set(bnmap[child])
        additional_edges += len(gobnilp - ground_truth)
        correct_edges += len(ground_truth.intersection(gobnilp))
        missing_edges += len(ground_truth - gobnilp)
        flipped_edges += len([parent for parent in gobnilp - ground_truth if child in bifmap[parent]])
        print(child,"<-", v, "Ground truth:", ground_truth, "GOBNILP:", gobnilp, " | Added:", gobnilp - ground_truth,
              "; Correct:", ground_truth.intersection(gobnilp), "Missing:", ground_truth-gobnilp)
        #print([parent for parent in gobnilp - ground_truth if child in bifmap[parent]])
        #print([parent for parent in ground_truth - gobnilp if child in bnmap[parent]])
    print("Number of correct edges:", correct_edges)
    print("Number of additional edges:", additional_edges)
    print("Number of flipped edges:", flipped_edges)
    print("Number of missing edges:", missing_edges)

# Reads feature file and returns a map (child -> feature list) and the lowest index used in the file.
def readFeatureFile(filename):
    file1 = open(filename, "r")
    lines = file1.readlines()
    features = {}
    firstIndex = -1
    for i in range(0, len(lines)):
        tokens = [int(t.strip()) for t in lines[i].split(",")]
        if i == 0:
            firstIndex = tokens[0]
        features[tokens[0]] = tokens[1:]
    return features, firstIndex

def extractScoreParents(line):
    parents = (line.split(' ')[2:])
    return float(line.split(' ')[0]), set([int(s.strip()) for s in parents])

def extractMetrics(line):
    return [float(s.strip()) for s in line.split(',')]

def readScoreAndMetricFiles(basename, numberOfNodes, nodemap):
    mmap = {}
    for node in range(0,numberOfNodes):
        parents = set(nodemap[node])
        scorefilename = basename + '_' + str(node)
        if os.path.isfile(scorefilename):
            scorefile = open(scorefilename, "r")
            metricsfile = open(basename + '_' + str(node) + '.mets', "r")
            score_lines = [extractScoreParents(line) for line in scorefile.readlines()]
            metrics_lines = metricsfile.readlines()
            score_indices = [ind for ind,p in enumerate(score_lines) if p[1] == parents]
            min_score = min([score for score, p in score_lines if p == parents])
            score_index = [index for index in score_indices if score_lines[index][0] == min_score][0]
            metrics = extractMetrics(metrics_lines[score_index])
            mmap[node] = metrics
            scorefile.close()
            metricsfile.close()
        else:
            mmap[node] = [0.0, 0.0, 0.0, 0.0, 0.0]
    return mmap

def analyzeFeatures(nodeMap, featureMap, firstIndex):
    meanRanks = []
    medianRanks = []
    parentless = 0
    featurelessNodeCount = 0
    parentNotInFeatureMap = 0
    for child in nodeMap.keys():
        if len(nodeMap[child]) == 0:
            parentless = parentless + 1
            meanRanks.append(-1)
            medianRanks.append(-1)
        else:
            if not (child-firstIndex) in featureMap:
                featurelessNodeCount = featurelessNodeCount + 1
                meanRanks.append(-1)
                medianRanks.append(-1)
            else:
                for parent in nodeMap[child]:
                    if not parent in featureMap[child - firstIndex]:
                        parentNotInFeatureMap = parentNotInFeatureMap + 1
                indices = [featureMap[child - firstIndex].index(parent) for parent in nodeMap[child] if parent in featureMap[child - firstIndex]]
                if not indices:
                    meanRanks.append(-1)
                    medianRanks.append(-1)
                else:
                    meanRanks.append(statistics.mean(indices))
                    medianRanks.append(statistics.median(indices))
                #print(child, nodeMap[child], indices)

    return meanRanks, medianRanks, parentless, featurelessNodeCount, parentNotInFeatureMap

def readAndAnalyzeFeatures(biffile, featurefile):
    nodeMap = readBif(biffile)
    featureMap, firstIndex = readFeatureFile(featurefile)
    meanRanks, medianRanks, parentless, featurelessNodeCount, parentNotInFeatureMap = analyzeFeatures(nodeMap, featureMap, firstIndex)
    print("BIF:", biffile)
    print("Features:", featurefile)
    print("  Mean Ranks:", meanRanks)
    print("  Average Mean Rank:", (sum(meanRanks) + parentless + featurelessNodeCount) / max(1, len(meanRanks) - parentless - featurelessNodeCount))
    print("  Median Ranks:", medianRanks)
    print("  Average Median Rank:", (sum(medianRanks) + parentless + featurelessNodeCount) / max(1, len(medianRanks) - parentless - featurelessNodeCount))
    print("  Parentless:", parentless)
    print("  Featureless Nodes:", featurelessNodeCount)
    print("  Parent not in feature map:", parentNotInFeatureMap)

def readAndAnalyzeGobnilp(BNfile, scoreAndMetricsBase):
    nodemap, scoremap = readBNfile(BNfile)
    metricmap = readScoreAndMetricFiles(scoreAndMetricsBase, len(nodemap.keys()), nodemap)
    print("Metrics for", BNfile, "and scores/metrics from", scoreAndMetricsBase)
    for node in nodemap.keys():
        print("  Node", node, "metrics:", metricmap[node])

# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Analyze score-and-search files (bif, features, gobnlip solutions, '
                                                 'metrics & scores). The program needs either: 1) bif and features, 2)'
                                                 'bif and gobnilp, or 3) gobnilp and metrics and scores. If multiple '
                                                 'requirements are fulfilled it will perform all viable analyzes.')
    parser.add_argument('-bif', dest='bif', action='store',
                        default='',
                        help='Bif file containing the ground truth.')
    parser.add_argument('-gobnilp', dest='gobnilp', action='store',
                        default='',
                        help='Gobnilp solution.')
    parser.add_argument('-features', dest='features', action='store',
                        default='',
                        help='Feature file.')
    parser.add_argument('-metricsAndScores', dest='metricsAndScores', action='store',
                        default='',
                        help='The base name for metrics and score files - the program will add _<node> to look for '
                             'score files and _<node>.mets to look for metrics files.')
    #parser.add_argument('-output', dest='output', action='store',
    #                    default='',
    #                    help='The file to write output to.')
    args = parser.parse_args(['-bif', 'c:\\data\\child.bif', '-gobnilp', 'c:\\data\\child_5000.20.merged.opt',
                              '-features', 'c:\\data\\500.1_RandomForest', '-metricsAndScores', 'c:\\data\\child',
                              #'-output', ''
                              ])

    biffile = args.bif
    BNfile = args.gobnilp
    featurefile = args.features
    msfiles = args.metricsAndScores
    if not biffile == '' and not featurefile == '':
        if not os.path.exists(biffile):
            print("BIF file",biffile,"does not exist.")
        if not os.path.exists(featurefile):
            print("Features file",featurefile,"does not exist.")
        readAndAnalyzeFeatures(biffile, featurefile)
    if not biffile == '' and not BNfile == '':
        if not os.path.exists(biffile):
            print("BIF file",biffile,"does not exist.")
        if not os.path.exists(BNfile):
            print("Gobnilp file",BNfile,"does not exist.")
        checkGobnilpSolution(BNfile, biffile)
    if not biffile == '' and not msfiles == '':
        readAndAnalyzeGobnilp(BNfile, msfiles)

    test_map = readRDABIFfile("c:\\data\\ecoli70.bif")
    print_child_parent_map(test_map)



# See PyCharm help at https://www.jetbrains.com/help/pycharm/
