import sys
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier
from sklearn.ensemble import ExtraTreesClassifier, RandomForestClassifier, RandomForestRegressor


def readBif(biffile, fmap):
    file1 = open(biffile, "r")
    lines1 = file1.readlines()
    g1={}
    j = -1
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
    
        print(p1.intersection(p2))
        

names = [#"Nearest Neighbors",
         #"Linear SVM", "RBF SVM", "Gaussian Process",
         "DecisionTree",
         "RandomForestClassifier",
         "RandomForestRegressor",
         #"Neural Net", "AdaBoost",
         #"Naive Bayes", "QDA"
         ]

methods = [
    #KNeighborsClassifier(3),
    #SVC(kernel="linear", C=0.025),
    #SVC(gamma=2, C=1),
    #GaussianProcessClassifier(1.0 * RBF(1.0)),
    DecisionTreeClassifier(max_depth=5),
    RandomForestClassifier(criterion="entropy"),
    RandomForestRegressor(),

    #MLPClassifier(alpha=1, max_iter=1000),
    #AdaBoostClassifier(),
    #GaussianNB(),
    #QuadraticDiscriminantAnalysis()
    ]

data = np.genfromtxt(sys.argv[2], delimiter=',',skip_header=2)
data = np.delete(data, (0), axis=0)
nrow = np.shape(data)[0]
ncol = np.shape(data)[1]
nfeatures = int(sys.argv[3])
prefix = (sys.argv[4])


for name, clf in zip(names, methods):
    print(name)
    w = open(prefix +"_"+  name, "w")
    features={}
    for c in range(0,ncol):
        y = data[:,c]
        X = np.delete(data, c, axis=1)
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)
 #       ax = plt.subplot(len(datasets), len(methods) + 1, i)
        clf.fit(X_train, y_train)
        score = clf.score(X_test, y_test)
        feat = np.argsort(-1*clf.feature_importances_)[:nfeatures]
        feat = [f+1 if f>c else f for f in feat]
        features[c]=feat
        w.writelines(', '.join(str(e) for e in feat) + "\n")
    readBif(sys.argv[1],features)
    w.close()

        #print(f)

        
        



