#!/bin/sh

for d in $(find $1 -name "*_temp.*")
do
  rm $d
done

for d in $(find $1 -name "*.u")
do
  rm $d
done

for d in $(find $1 -name "*_after.*")
do
  rm $d
done

if [ -f $1/garbage.c ]
then
    rm $1/garbage.c
fi
