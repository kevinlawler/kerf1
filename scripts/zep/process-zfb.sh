#!/bin/bash
#
# remember to call chmod +x on this file
#
# chmod +x process-zfb.sh 
#
# then call
#
# ./process-zfb.sh
#
# to go
#
#

FOLDER="."

#note quandl provides ".csv" which actually are psv

OUTFILE="ZFB-full.psv"

#keep header for first, drop for all others
cat        $FOLDER/ZFB-1.csv >  $FOLDER/$OUTFILE
tail -n +2 $FOLDER/ZFB-2.csv >> $FOLDER/$OUTFILE
tail -n +2 $FOLDER/ZFB-3.csv >> $FOLDER/$OUTFILE
tail -n +2 $FOLDER/ZFB-4.csv >> $FOLDER/$OUTFILE
