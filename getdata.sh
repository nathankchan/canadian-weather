#!/bin/sh

STATIONID=$(echo $1)
STARTYEAR=$(echo $2)
ENDYEAR=$(echo $3)

RUNDIR=$(pwd)
DATADIR=$(echo $(pwd)"/rawdata/"$STATIONID"/")
[[ ! -d "$DATADIR" ]] && mkdir -p "$DATADIR"

for YEAR in $(seq $STARTYEAR $ENDYEAR)
do
  # echo $YEAR
  for MONTH in $(seq -w 1 12)
  do
    # echo $MONTH
    FILEURL=$(echo "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=${STATIONID}&Year=${YEAR}&Month=${MONTH}&Day=14&timeframe=1&submit=Download+Data")
    FILENAME=$(echo $YEAR"_"$MONTH".csv")
    # echo $FILEURL $FILENAME
    # if wget --spider "$FILEURL" 2>/dev/null
    # then
      cd "$DATADIR"
      wget -nc -O "$FILENAME" "$FILEURL"
      cd "$RUNDIR"
    # fi
  done
done
