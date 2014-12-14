green='\033[0;32m'
red='\033[0;31m'
default='\033[0m' # No Color

echo -e "Testing all files..."

echo -e "COMPLETED TESTS\n----------" > results.txt

for f in *.pk;
do echo -e "\nTesting $f..." >> results.txt
out="$(./../pmkn -c ./$f 2>&1 > /dev/null)"
if [[ $out == *"Fatal error"* ]]
then
  echo -e $out >> results.txt
  echo -e "${red}$out ${default} in $f"
fi
echo -e "Testing of $f complete."  >> results.txt
done

echo -e "${green}Tests complete.${default}"