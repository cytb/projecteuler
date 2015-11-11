#!/bin/sh

get_filename(){
  FILE_NO="$(printf '%04d' $1)" 
  FILE_EXISTING=$(ls -1 | egrep "\\-${FILE_NO}\\.hs\$")

  if [ -n "$FILE_EXISTING" ]
  then 
    echo "$(basename "${FILE_EXISTING}" .hs)"
  else
    echo "$(date +%Y-%m-%d)-${FILE_NO}"  
  fi
}

FILE_SOURCE_PREFIX=""

if   [ $# -lt 1 ]
then
  exit
elif [ $# -gt 1 ]
then
  FILE_SOURCE_PREFIX="$(get_filename "$1")"
  shift
fi

FILE_PREFIX="$(get_filename "$1")"

if [ -n "${FILE_SOURCE_PREFIX}" ] && ! [ -f "${FILE_PREFIX}.hs" ]
then
  cp -n "${FILE_SOURCE_PREFIX}.hs" "${FILE_PREFIX}.hs"
fi

while (vim "${FILE_PREFIX}.hs" && ghc -O3 "./${FILE_PREFIX}.hs" -o "./${FILE_PREFIX}.out"  && "./${FILE_PREFIX}.out")
do
  :
done


