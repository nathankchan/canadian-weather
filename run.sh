#!/bin/sh

# Update station metadata from ECCC inventory
Rscript update_stationlist.R

# Download all files
tail -n +2 stationlist.csv | parallel --will-cite --colsep ',' --progress -v ./getdata.sh {4} {14} {15}

# Remove data for current month (i.e., force current month to update)
# This basically ensures that the database remains up to date
ls rawdata | parallel --will-cite --progress ./update.sh {1}

# Check and delete corrupted files
ls rawdata | parallel --will-cite --progress ./removecorruptfiles.sh {1}

# Redownload all corrupted files and get most recent data
# Note that ./getdata.sh skips download if file already exists
tail -n +2 stationlist.csv | parallel --will-cite --colsep ',' --progress -v ./getdata.sh {4} {14} {15}

# Update combined csv files in ./data
Rscript helper.R
