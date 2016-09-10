#!/bin/bash
#merges jsons into one collective CSV file, dropping the header from 
#subsequent parsed jsons that are appended to the output csv

rm parsed_zika_12days.csv
i=1
for f in $(ls ./zika_hagen_dat/data*txt); do 
Rscript parse_twitter_zika.R $f parsed_zika_12days.csv $i
echo $i
i=$((i+1))
done
