# -*- coding: utf-8 -*-
"""
Created on Wed Sep 15 15:28:08 2021

@author: Arica Schuett
"""
import snscrape.modules.twitter as sntwitter
import pandas as pd


import csv
 

demands =[]
# opening the CSV file
with open('demands.csv', mode ='r')as file:
   
  # reading the CSV file
  csvFile = csv.reader(file)
 
  # displaying the contents of the CSV file
  for lines in csvFile:
        demands.append(lines)
        
        
demands = [i.strip(']') if type(i) == str else str(i) for i in demands]
demands = [i.strip('[') if type(i) == str else str(i) for i in demands]
        
# Group of organization Twitter handles:
groups = (demands)
counter = 0
scrapeCounter = []
for j in groups:
    accountName = j
    scrapeLocation = ("from:%s" % (accountName,), " since:2020-04-24 until:2020-07-31")
    scrapeCounter.append(scrapeLocation)
    #print(scrapeCounter)
    counter = counter+1

# 
  
# initializing list 
test_list = scrapeCounter
  
# using list comprehension + join()
# conversion of list of tuple to list of list 
res = [''.join(i) for i in test_list]
  



# Creating list to append tweet data to
tweets_list1 = []
counter = 0
for j in res:
    inner = j

    # Using TwitterSearchScraper to scrape data and append tweets to list
    # Max Tweets scraped 500
    # Start 1 month prior to GF's murder, end end of July
    for i,tweet in enumerate(sntwitter.TwitterSearchScraper('%s' % (inner)).get_items()):
        if i>500:
            break
        tweets_list1.append([tweet.date, tweet.id, tweet.content, tweet.user.username])
        counter = counter +1
    
    
# Creating a dataframe from the tweets list above 
tweets_df1 = pd.DataFrame(tweets_list1, columns=['Datetime', 'Tweet Id', 'Text', 'Username'])

tweets_df1.to_csv('testtweets.csv')