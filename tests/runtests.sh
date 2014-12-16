#!/bin/bash

#Set colors for output
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

# Compare <outfile> <reffile>
# Compares the outfile with reffile.
Compare() {
    echo diff -b $1 $2 1>&2
    diff -b "$1" "$2" 1>&2 || {
    PrintError "$1 differs"
    echo -e "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
      PrintError "$1 failed on $*"
      return 1
    }
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.pk//'`

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

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
      echo "###### SUCCESS" 1>&2
    else
      echo "###### FAILED" 1>&2
      globalerror=$error
    fi
}

shift `expr $OPTIND - 1`

files="test-*.pk"

for file in $files
do
    case $file in
  *test-*)
      Check $file $@ 2>> $testlog
      ;;
  *)
      echo "unknown file type $file"
      globalerror=1
      ;;
    esac
done

exit $globalerror
