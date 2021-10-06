import sys
import os
import h2o
from h2o.estimators.random_forest import H2ORandomForestEstimator
h2o.init()

k = int(sys.argv[2])

for subdir, dirs, files in os.walk(sys.argv[1]):
    for file in files:
        print (os.path.join(subdir, file))
        filepath = subdir + os.sep + file

        if filepath.endswith(".csv"):
            print(filepath)
            prefix = os.path.splitext(filepath)[0]
            data = h2o.import_file(filepath)
#nfeatures = int(sys.argv[2])
#prefix = sys.argv[3]


            
            
#dropping cols with unary variables
            for i in data.names :
            
                if len(data[i].table()[1]) == 1 :
                    data = data.drop(i, axis=1)
                    print(i)
                ctr+=1

            n = data.shape[1]
            nfeatures = n 
            print(n)
            train, valid = data.split_frame(ratios = [.8], seed = 1234)
            #making variables categorical
            for i in range(0,n):
                data[i] = data[i].ascharacter()

            


            names = [
                "DecisionTree",
                "RandomForest"
            ]

            methods = [
                H2ORandomForestEstimator(ntrees = 1, mtries = n-1, sample_rate=1, seed = 1234),
                H2ORandomForestEstimator(mtries = n-5, seed = 1234)
            ]

            for name, clf in zip(names, methods):
                print(name)
                w = open(prefix +"_"+  name, "w")
                w1 = open(prefix +"_"+  name+ "_scores", "w")
                for c in range(0,n):
                    predictors = data.drop(c, axis=1).columns
                    response = data.names[c]
                
                    clf.train(x = predictors, y = response, training_frame = train, validation_frame = valid)
                    features = clf.varimp(True)['variable'][0:nfeatures]
                    scores =  clf.varimp(True)['percentage'][0:nfeatures]
                    features = [i.split(i[0])[1] for i in features]
                    features = [str(int(i)-1) for i in features]
                    
                    scores = [str(i) for i in scores]
                    w.writelines(', '.join(features) + "\n")
                    w1.writelines(', '.join(scores) + "\n")

                w.close()
                w1.close()

