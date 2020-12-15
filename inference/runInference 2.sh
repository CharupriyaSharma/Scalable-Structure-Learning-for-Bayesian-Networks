multiBifFile=$1
testDataFile=$2
numBifs=$(cat $multiBifFile| grep "network"| wc -l)

for ((i=1; i<$numBifs; i++))
do
java -jar BIFExtractor.jar $i $multiBifFile
Rscript InferenceAllNodes.R temp.bif $testDataFile temp
cat temp >> $3
rm temp*

done
