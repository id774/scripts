#!/bin/sh

# ex)
# $ find ./app ./lib ./config ./db -type f -name "*" -exec ~/scripts/remove_space_eol.sh {} \;

while [ $# -gt 0 ]
do
  mv $1 $1.tmp
  sed -e 's/[[:blank:]]*$//' $1.tmp > $1
  rm $1.tmp
  echo 'Removed WhiteSpaceEOL from '$1
  shift
done
