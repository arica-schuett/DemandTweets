# wordcloud Demands Codes Process 
# Arica Schuett
# July 21, 2022

#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2")
library(wordcloud2)
#install.packages("tm")
library(tm)#Create a vector containing only the text
#install.packages("qdapRegex")
library(qdapRegex) #removing URLS
#install.packages("webshot")
library(tidyverse)
library(csv)

ProcessCodes <- read_csv("/Users/aricaschuett/Documents/AliyahCodes/TweetsFullNoDupes.csv")
AliyahSummerPCs <- read_csv("/Users/aricaschuett/Documents/AliyahCodes/Aliyah Process Codes - Sheet1.csv")
#AliyahSummerPCs <- AliyahSummerPCs[AliyahSummerPCs$...9 == "foreign language", ]
#AliyahSummerPCs <- AliyahSummerPCs[AliyahSummerPCs$...9 != "foreign langugage", ]
#AliyahSummerPCs <- AliyahSummerPCs[AliyahSummerPCs$...9 != "i don't think this tweet is relevent so i marked it as NA", ]

colnames(AliyahSummerPCs)[colnames(AliyahSummerPCs) == "...9"] <- "Notes"

SummerPCs <- select(AliyahSummerPCs, -c(Notes))

PCs <- rbind(SummerPCs, ProcessCodes)

write.csv(PCs, "/Users/aricaschuett/Documents/AliyahCodes/PCs.csv")


Test <- select(PCs, -c(unique(PCs$Tweet.Id)))
 # Create Process Code Wordcloud
text <- ProcessCodes$Process_Code# Create a corpus  

# continue cleaning the text
text <- 
  text %>%
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp")) 

# Convert the data into a summary table
textCorpus <- 
  Corpus(VectorSource(text)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
#textCorpus <- filter(freq >= 3)
#textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)

# ing vergs reg ex
install.packages("stringr")
library(stringr)

words <- as.character(textCorpus$word)

text %>% 
  str_extract_all("ING") %>%
  unlist()

test <-str_view_all(text, "ing")

# build wordcloud 
wordcloudProcess <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloudProcess


# Create Text Wordcloud
text <- ProcessCodes$Text # Create a corpus  

# continue cleaning the text
text <- 
  text %>%
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp")) 

# Convert the data into a summary table
textCorpus <- 
  Corpus(VectorSource(text)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
#textCorpus <- filter(freq >= 3)
textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)


# build wordcloud 
wordcloudTweets <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloudTweets

