#!/bin/sh

DIRNAME=$1

for FILENAME in $(ls ./rawdata/$DIRNAME)
do
  FULLNAME=$(echo "./rawdata/""$DIRNAME"/$FILENAME)
  if [ $(stat -f "%A" $FULLNAME) -ne 644 ]
  then
    rm $FULLNAME
    echo "$FULLNAME"" removed (incorrect permissions)"
  elif [ $(du $FULLNAME | cut -f1) -eq 0 ]
  then
    rm $FULLNAME
    echo "$FULLNAME"" removed (empty file)"
  fi
done
