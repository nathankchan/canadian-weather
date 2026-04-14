#!/bin/sh

DIRNAME=$1

for FILENAME in $(ls ./rawdata/$DIRNAME)
do
  FILEYEAR=$(echo $FILENAME | cut -c -4)
  FILEMONTH=$(echo $FILENAME | cut -c 6-7)
  FILEPOSIX=$(date -d "$FILEYEAR-$FILEMONTH-01" +"%s")
  DATECHECK=$(date -d "$FILEYEAR-$FILEMONTH-01 +1 month" +"%s")
  FULLNAME=$(echo "./rawdata/""$DIRNAME"/$FILENAME)
  
  if [ $(stat -c "%Y" "$FULLNAME") -lt $DATECHECK ]
  then
    if [ $(date +"%s") -gt $FILEPOSIX ] 
    then
    FILEURL=$(echo "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=${DIRNAME}&Year=${FILEYEAR}&Month=${FILEMONTH}&Day=14&timeframe=1&submit=Download+Data")
    rm "$FULLNAME"
    wget -nc -O "$FULLNAME" "$FILEURL"
    echo "$FULLNAME"" was updated"
    fi
  fi
  
done
