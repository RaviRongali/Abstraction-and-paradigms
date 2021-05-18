#Testing Script

if [ $# -lt 2 ]; then 
	echo "Usage gParser.sh <your-file.rkt> <test-cases-file>"
	exit 1
fi

if [ -f "$1" ] && [ -f "$2" ]; then
	fileName="$1"
	testFile="$2"
	tstCase=""
	count1=0
	count2=0
else
	echo "Enter correct filenames"
	exit 1
fi

cat $fileName > tmp
echo "" >> tmp
echo "(require racket/cmdline)" >> tmp
echo "(define-namespace-anchor a) (define ns (namespace-anchor->namespace a)) (define string-to-eval (command-line #:args (s) s))" >> tmp
echo "(eval (read (open-input-string string-to-eval)) ns)" >> tmp

while read -r tstCase; do
	actOut=$(./aParser "$tstCase" 2> /dev/null)
	tmpOut=$(racket tmp "$tstCase" 2> /dev/null)

	printf "Your Output:\n$tmpOut\nCorrect Output:\n$actOut\n"

	if [ "$actOut" = "$tmpOut" ]; then
		echo "Testcase Passed!"
		count1=$(($count1 + 1))
		if [ "$actOut" = "" ]; then
			echo "(BUT) Probably Incorrect Testcase"
		fi
	
	else
		echo "Tescase Failed :("
	fi

	echo ""
echo "$count1"
done < $testFile

rm tmp
