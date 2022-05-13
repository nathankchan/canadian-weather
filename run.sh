#!/bin/sh

# Download all files
tail -n +2 stationlist.csv | parallel --colsep ',' --progress -v ./getdata.sh {5} {15} {16}

# Remove data for current month (i.e., force current month to update)
# This basically ensures that the database remains up to date
ls data | parallel --progress ./update.sh {1}

# Check and delete corrupted files
ls data | parallel --progress ./removecorruptfiles.sh {1}

# Redownload all corrupted files and get most recent data
# Note that ./getdata.sh skips download if file already exists
tail -n +2 stationlist.csv | parallel --colsep ',' --progress -v ./getdata.sh {5} {15} {16}

# Update combined csv files in ./data
Rscript helper.R
