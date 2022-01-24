from main import readRDABIFfile, readFeatureFile, analyzeFeatures
import sys

def readAndAnalyzeFeatures(rdabiffile, featurefile):
    nodeMap = readRDABIFfile(rdabiffile)
    featureMap, firstIndex = readFeatureFile(featurefile)
    meanRanks, medianRanks, parentless, featurelessNodeCount, parentNotInFeatureMap = analyzeFeatures(nodeMap, featureMap, firstIndex)
    print("BIF:", rdabiffile)
    print("Features:", featurefile)
    print("  Mean Ranks:", meanRanks)
    print("  Average Mean Rank:", (sum(meanRanks) + parentless + featurelessNodeCount) / max(1, len(meanRanks) - parentless - featurelessNodeCount))
    print("  Median Ranks:", medianRanks)
    print("  Average Median Rank:", (sum(medianRanks) + parentless + featurelessNodeCount) / max(1, len(medianRanks) - parentless - featurelessNodeCount))
    print("  Parentless:", parentless)
    print("  Featureless Nodes:", featurelessNodeCount)
    print("  Parent not in feature map:", parentNotInFeatureMap)

readAndAnalyzeFeatures(sys.argv[1],sys.argv[2])