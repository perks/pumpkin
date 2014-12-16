#!/bin/bash

#Set colors for output
yellow='\033[0;33m'
green='\033[0;32m'
red='\033[0;31m'
default='\033[0m'

# Set time limit for all operations
ulimit -t 30

testlog=runtests.log
rm -f $testlog

error=0
globalerror=0

#Prints errors to console
PrintError() {
    if [ $error -eq 0 ] ; then
      echo -e "${red}FAILED${default}"
      error=1
    fi
    #echo "  $1"
}

# Compare <output> <expected>
Compare() {
    echo diff -Bw $1 $2 1>&2
    diff -Bw "$1" "$2" 1>&2 || {
    PrintError "$1 differs"
    echo -e "FAILED $1 differs from $2" 1>&2
    }
}

# Check <testfile>
Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.pk//'`

    echo -n "$basename..."

    echo 1>&2
    echo "----- $basename results -----" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.out" &&
    generatedfiles="$generatedfiles ${basename}.js" &&
    ./../pmkn -c ./$@ 2>&1 > ${basename}.js &&
    node ./${basename}.js > ${basename}.out &&
    Compare ${basename}.out ${basename}.txt

    # Report the status and clean up the generated files
    if [ $error -eq 0 ] ; then
      rm -f $generatedfiles
      echo -e "${green}OK${default}"
      echo "--------- SUCCESS ---------" 1>&2
    else
      echo "--------- FAILURE ---------" 1>&2
      globalerror=$error
    fi
}

echo -e "${yellow}----------------------"
echo -e " Testing all files..."
echo -e "----------------------${default}"

echo -e "--------------\n Test Results\n--------------" > $testlog

shift `expr $OPTIND - 1`

#Check all files
files="test-*.pk"
for file in $files
do
  Check $file $@ 2>> $testlog
done

exit $globalerror
