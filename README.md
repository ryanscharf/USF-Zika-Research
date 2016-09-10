# USF-Zika-Research

A collection of utility R scripts based around collecting and parsing Twitter discussion based around the Zika virus.

Current parsed variables include --
timestamp_ms            
datetime                
tweet_id               
text                    
retweet_count           
favorite_count         
expanded_url           
friends_count           
screen_name             
user_id_str             
in_reply_to_screen_name 
in_reply_to_user_id     
rt_screen_name          
rt_screen_id           
full_name               
followers_count        
place_lat               
place_lon               
lat                     
lon                     
mentioned_users         -";" separated multivalued column

mentioned_id            -";" separated multivalued column

hashes                  -";" separated multivalued column

parsed_media_type       -";" separated multivalued column

parsed_media_url        -";" separated multivalued column

# Parse workthrow overview

Collect tweets with Python (tweepy)  or R (streamR) - key is streaming JSON output.

Run parse program as follows:

Rscript parse_twitter_zika.R ./zika_hagen_dat/data_0825.txt sample_output.csv 1 

(1 for header, 2 for no header....used in bulk output)

parse_bulk_json.sh collates the 12 json files into 1 giant CSV file for now, supressing the CSV header output when writing at additional stages.
Need to manually change folder if re-using this script right now, can be cleaner.


