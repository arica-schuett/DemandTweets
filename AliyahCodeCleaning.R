library(tidyverse)
library(lubridate)


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

# remove pure duplicates
AliyahData = unique(AliyahData)
AliyahData <- AliyahData[!duplicated(AliyahData$Tweet.Id), ]

# add Coder Initials
Coder <- rep(c("AMC"), each=dim(AliyahData[1]) )

Coder <- data.frame(Coder)

AliyahData <-cbind(AliyahData, Coder)
#AliyahData <- rbind(AliyahData1, AliyahData2, AliyahData3, AliyahData4, AliyahData5, AliyahData6, AliyahData7, AliyahData8, Coder)


#### Lucy Codes
LucyData1 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/LucyTweets - Week1.csv")
LucyData2 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/LucyTweets - Week2.csv")
LucyData3 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/LucyTweets - Week3.csv")
LucyData4 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/LucyTweets - Week4.csv")
LucyData5 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/LucyTweets - Week5.csv")


LucyData1 <- subset(LucyData1, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))
LucyData2 <- subset(LucyData2, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))
LucyData3 <- subset(LucyData3, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))
LucyData4 <- subset(LucyData4, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))
LucyData5 <- subset(LucyData5, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))

LucyData <- rbind(LucyData1, LucyData2, LucyData3, LucyData4, LucyData5)

NumTweets <- dim(LucyData)[1] #1614
NumUniqueTweets <-length(unique(LucyData$Tweet.Id))

LucyData = unique(LucyData)
LucyData <- LucyData[!duplicated(LucyData$Tweet.Id), ]

# add Coder Initials
Coder <- rep(c("LC"), each=dim(LucyData[1]) )

Coder <- data.frame(Coder)

LucyData <-cbind(LucyData, Coder)



## Arica Coces
AricaData1 = read_csv("/Users/aricaschuett/Documents/AliyahCodes/AricaWeek1 - Week1.csv")

AricaData1 <- subset(AricaData1, select = c("Tweet.Id", "Datetime", "Text", "Username", "Demand(Y/N)", "Process_Code"))
AricaData = unique(AricaData1)
AricaData <- AricaData[!duplicated(AricaData$Tweet.Id), ]

# add Coder Initials
Coder <- rep(c("ANS"), each=dim(AricaData[1]) )
Coder <- data.frame(Coder)
AricaData <-cbind(AricaData, Coder)



## Complete Tweets
TweetsFull <- rbind(AliyahData, LucyData, AricaData)
write.csv(TweetsFull, file = "/Users/aricaschuett/Documents/AliyahCodes/TweetsFull.csv")

TweetsFullNoDupes <- TweetsFull[!duplicated(TweetsFull$Text), ]

write.csv(TweetsFullNoDupes, file = "/Users/aricaschuett/Documents/AliyahCodes/TweetsFullNoDupes.csv")

