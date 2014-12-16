#!/bin/bash

blue='\033[0;33m'
green='\033[0;32m'
red='\033[0;31m'
default='\033[0m' # No Color

echo -e "${blue}-----------------------------"
echo -e " Testing all javscript files"
echo -e "-----------------------------${default}"

echo -e "Test Results\n------------\n" > jsresults.txt

for f in *.js;
do echo -en "Testing $f..."
out="$(node ./$f 2>&1)"
if [[ $out == *"Error"* ]]
then
  echo -e $out >> jsresults.txt
  echo >> jsresults.txt
  echo -e "${red}$out ${default} in $f"
else
  echo -en " ${green}complete.${default}\n"
fi

done

echo -e "${green}Testing complete.${default}"