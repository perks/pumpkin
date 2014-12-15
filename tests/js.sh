#!/bin/bash

green='\033[0;32m'
red='\033[0;31m'
default='\033[0m' # No Color

echo -e "Compiling all files to javascript..."

for f in *.pk;
do out="$(./../pmkn -c ./$f 2>&1 > ${f%.pk}.js)"
if [[ $out == *"Fatal error"* ]]
then
  echo -e "${red}$out ${default} in $f"
fi
echo -e "Compiled $f to javascript."
done

echo -e "${green}Compilation complete.${default}"