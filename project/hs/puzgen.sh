#/usr/bin/sh
echo "$1 $2"
for i in `seq $1`; do
  for j in `seq $2`; do
    printf "."
  done
  echo
done
