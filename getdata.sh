#!/bin/sh

STATIONID=$1
STARTYEAR=$2
ENDYEAR=$3

DATADIR="$(pwd)/rawdata/${STATIONID}/"
mkdir -p "$DATADIR"

for YEAR in $(seq $STARTYEAR $ENDYEAR)
do
  for MONTH in $(seq -w 1 12)
  do
    FILEURL="https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=${STATIONID}&Year=${YEAR}&Month=${MONTH}&Day=14&timeframe=1&submit=Download+Data"
    wget -nc -O "${DATADIR}${YEAR}_${MONTH}.csv" "$FILEURL"
  done
done
