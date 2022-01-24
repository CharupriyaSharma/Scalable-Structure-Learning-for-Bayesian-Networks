from main import readRDABIFfile, readBNfile
import sys

def compute_shd(biffile, BNfile):
    bifmap = readRDABIFfile(biffile)
    bnmap, _ = readBNfile(BNfile)
    additional_edges = 0
    correct_edges = 0
    flipped_edges = 0
    missing_edges = 0
    #print("Comparing GOBNILP solution with ground truth network:")
    #print("GOBNILP solution:", BNfile)
    #print("Ground truth BIF:", groundtruthbiffile)
    for child, v in bifmap.items():
        ground_truth = set(bifmap[child])
        gobnilp = set(bnmap[child])
        additional_edges += len(gobnilp - ground_truth)
        correct_edges += len(ground_truth.intersection(gobnilp))
        missing_edges += len(ground_truth - gobnilp)
        flipped_edges += len([parent for parent in gobnilp - ground_truth if child in bifmap[parent]])
        #print(child,"<-", v, "Ground truth:", ground_truth, "GOBNILP:", gobnilp, " | Added:", gobnilp - ground_truth,
        #      "; Correct:", ground_truth.intersection(gobnilp), "Missing:", ground_truth-gobnilp)
        #print([parent for parent in gobnilp - ground_truth if child in bifmap[parent]])
        #print([parent for parent in ground_truth - gobnilp if child in bnmap[parent]])
    #print("Number of correct edges:", correct_edges)
    #print("Number of additional edges:", additional_edges)
    #print("Number of flipped edges:", flipped_edges)
    #print("Number of missing edges:", missing_edges)
    return additional_edges + missing_edges - flipped_edges

print(compute_shd(sys.argv[1],sys.argv[2]))