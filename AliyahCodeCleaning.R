library(tidyverse)
library(lubridate)

###### 2000 ##############
#install.packages("haven")
library(haven)
AliyahData1 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/Aliyah_Tweets - Sheet1.csv")
AliyahData2 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/Aliyah_Tweets - Sheet2.csv")
AliyahData3 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/Aliyah_Tweets - week3.csv")
AliyahData4 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/Aliyah_Tweets - Week4.csv")
AliyahData5 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/Aliyah_Tweets - week5.csv")
AliyahData6 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/Aliyah_Tweets - week6.csv")
AliyahData7 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/Aliyah_Tweets - week7.csv")
AliyahData8 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/Aliyah_Tweets - week8.csv")


colnames(AliyahData1) 
# "Tweet Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code", "Accountability", "Innovation", 
#"Hypocricy", "Justice", "specific", "Batch_Week", "Coder_Initials", "Memos" 
colnames(AliyahData1)[1] ="Tweet.Id"
AliyahData1 <- subset(AliyahData1, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))

colnames(AliyahData2)
#"Tweet Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code", "Specific", "Batch_Week", "Coder_Initials", "Memos"
colnames(AliyahData2)[1] ="Tweet.Id"
AliyahData2 <- subset(AliyahData2, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))

colnames(AliyahData3)
#"Tweet.Id", "Datetime", "Text", "Username", "Date", "Time", "Demand(Y/N)", "Process_Code", "Specific", "Batch_Week", "Coder_Initials", "Memos" 
AliyahData3 <- subset(AliyahData3, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))

#AliyahData3[ , c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code")]

colnames(AliyahData4)
#"Tweet.Id","Datetime", "Text", "Username", "Date", "Time", "Demand(Y/N)", "Process_Code", "Specific",  "Batch_Week", "Coder_Initials", "Memos"
AliyahData4 <- subset(AliyahData4, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))
#AliyahData4[ , c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code")]


colnames(AliyahData5)
#"...1", "Tweet.Id", "X", "Datetime", "Text", "Username","Date", "Time", "Demand(Y/N)", "Process_Code" ,"Specific", "Batch_Week", "Coder_Initials", "Memos" 
#AliyahData5[ , c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code")]
AliyahData5 <- subset(AliyahData5, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))

colnames(AliyahData6)
#"...1", "Tweet.Id", "X", "Datetime", "Text", "Username","Date", "Time", "Demand(Y/N)", "Process_Code",  "Specific", "Batch_Week", "Coder_Initials", "Memos"
#AliyahData6[ , c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code")]
AliyahData6 <- subset(AliyahData6, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))

colnames(AliyahData7)
#1] "...1",  "Tweet.Id", "X", "Datetime", "Text", "Username", "Date", "Time", "Process Code", "Coder Initials", "Memos"   
AliyahData7[ , "Demand(Y/N)"] <- NA
colnames(AliyahData7)[9] ="Process_Code"
AliyahData7 <- subset(AliyahData7, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))
#AliyahData7[ , c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code")]

colnames(AliyahData8)
# [1] "Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code", "Specific", "Batch_Week", "Coder_Initials"  "Memos" 
AliyahData8 <- subset(AliyahData8, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))
#AliyahData8[ , c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code")]

AliyahData <- rbind(AliyahData1, AliyahData2, AliyahData3, AliyahData4, AliyahData5, AliyahData6, AliyahData7, AliyahData8)

NumTweets <- dim(AliyahData)[1] #1614
NumUniqueTweets <-length(unique(AliyahData$Tweet.Id))

Duplicates <- NumUniqueTweets-NumTweets #242

AliyahData[!duplicated(AliyahData[c('Tweet.Id')]), ]
dim(AliyahData)

length(unique(AliyahData$Tweet.Id))

AliyahData[!duplicated(AliyahData[c('team')]), ]
duplicated(AliyahData)
sum(duplicated(AliyahData$Tweet.Id))
# remove pure duplicates
AliyahData$Tweet.Id[duplicated(AliyahData$Tweet.Id)]

AliyahData[!duplicated(AliyahData$Tweet.Id), ]

dim(AliyahData)
AliyahData[order(-AliyahData$Tweet.Id),]
