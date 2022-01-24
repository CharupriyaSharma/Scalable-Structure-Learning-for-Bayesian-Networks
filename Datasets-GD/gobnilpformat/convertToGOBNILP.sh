for i in 1000 2000 5000
do
	file=$1"/"$1"_"$i".csv"
	headerfile=$1"/3.1.csv"
	echo $file
	echo $headerfile
	echo "$(tail -n +2 $file)" > $file

	sed -i ''  's/,/ /g' $file
	header="$(cat $headerfile | tail -1)"
	sed -i '' '1s/^/'"$header"'\'$'\n/' $file
	header="$(cat $headerfile | head -1)"
        sed -i '' '1s/^/'"$header"'\'$'\n/' $file
	
done
