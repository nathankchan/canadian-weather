#!/bin/sh

DIRNAME=$1

for FILENAME in $(ls ./data/$DIRNAME)
do
  FILEYEAR=$(echo $FILENAME | cut -c -4)
  FILEMONTH=$(echo $FILENAME | cut -c 6-7)
  FILEPOSIX=$(date -jf "%Y %m %d" "$FILEYEAR $FILEMONTH 01" +"%s")
  DATECHECK=$(date -jf "%Y %m %d" -v+1m "$FILEYEAR $FILEMONTH 01" +"%s")
  FULLNAME=$(echo "./data/""$DIRNAME"/$FILENAME)
  
  if [ $(stat -f "%B" "$FULLNAME") -lt $DATECHECK ]
  then
    if [ $(date +"%s") -gt $FILEPOSIX ] 
    then
    FILEURL=$(echo "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=${DIRNAME}&Year=${FILEYEAR}&Month=${FILEMONTH}&Day=14&timeframe=1&submit=Download+Data")
    rm "$FULLNAME"
    # echo $FILEYEAR $FILEMONTH $(date -jf %s $DATECHECK) "$FULLNAME" "$FILEURL"
    wget -nc -O "$FULLNAME" "$FILEURL"
    echo "$FULLNAME"" was updated"
    fi
  fi
  
done
