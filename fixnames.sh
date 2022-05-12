#!/bin/sh

DIRNAME=$1

for FILENAME in $(ls ./data/$DIRNAME)
do
  ORIGNAME=$(echo "./data/""$DIRNAME"/$FILENAME)
  if [ $(echo $FILENAME | wc -c) -eq 11 ]
  then
    FILENUMBER=$(echo $FILENAME | cut -c 6)
    NEWNAME=$(echo "./data/""$DIRNAME"/${FILENAME/_$FILENUMBER.csv/_0$FILENUMBER.csv})
    mv "$ORIGNAME" "$NEWNAME"
    echo "$ORIGNAME"" renamed to ""$NEWNAME"
  else
    echo "$ORIGNAME"" unchanged"
  fi
done

