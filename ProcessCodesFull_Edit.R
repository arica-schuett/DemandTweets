# Process Codes 

library(tidyverse)
#install.packages("wordcloud")
library(wordcloud)
library(wordcloud2)

#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2")
library(wordcloud2)
#install.packages("tm")
library(tm)#Create a vector containing only the text
#install.packages("qdapRegex")
library(qdapRegex) #removing URLS
library(csv)

# read in as dataframe for coder AMC
AMC1 <- read.csv("/Users/aricaschuett/Documents/Process Codes/Aliyah Process Codes - Sheet1.csv")
AMC2 <- read.csv("/Users/aricaschuett/Documents/Process Codes/Aliyah_Tweets - Sheet1.csv")
AMC3 <- read.csv("/Users/aricaschuett/Documents/Process Codes/Aliyah_Tweets - Sheet2.csv")
AMC4 <- read.csv("/Users/aricaschuett/Documents/Process Codes/Aliyah_Tweets - week3.csv")
AMC5 <- read.csv("/Users/aricaschuett/Documents/Process Codes/Aliyah_Tweets - Week4.csv")
AMC6 <- read.csv("/Users/aricaschuett/Documents/Process Codes/Aliyah_Tweets - week5.csv")
AMC7 <- read.csv("/Users/aricaschuett/Documents/Process Codes/Aliyah_Tweets - week6.csv")
AMC8 <- read.csv("/Users/aricaschuett/Documents/Process Codes/Aliyah_Tweets - week7.csv")
AMC9 <- read.csv("/Users/aricaschuett/Documents/Process Codes/Aliyah_Tweets - week8.csv")

AMC1 <- AMC1[,!names(AMC1) %in% c("X", "Demand.Y.N.")]
AMC2 <- AMC2[,!names(AMC2) %in% c("Accountability", "Innovation", "Hypocricy", "Justice", "specific", "Batch_Week", "Demand.Y.N.")]
AMC3 <- AMC3[,!names(AMC3) %in% c("Specific", "Batch_Week", "Demand.Y.N.")]
AMC4 <- AMC4[,!names(AMC4) %in% c("Date", "Time", "Specific", "Batch_Week", "Demand.Y.N.")]
AMC5 <- AMC5[,!names(AMC5) %in% c("Date", "Time", "Specific", "Batch_Week", "Demand.Y.N.")]
AMC6 <- AMC6[,!names(AMC6) %in% c("Date", "Time", "Specific", "Batch_Week", "X.1", "X", "Demand.Y.N.")]
AMC7 <- AMC7[,!names(AMC7) %in% c("Date", "Time", "Specific", "Batch_Week",  "X.1", "X", "Demand.Y.N.")]
AMC8 <- AMC8[,!names(AMC8) %in% c("Date", "Time", "X", "X.1", "Demand.Y.N.")]
AMC9 <- AMC9[,!names(AMC9) %in% c("Specific", "Batch_Week", "Demand.Y.N.")]

# Rename or add columns for AMC
AMC1 <- rename(AMC1, "Memos" = "X.1")
AMC1 <- rename(AMC1, "Coder_Initials" = "Coder")
AMC8 <- rename(AMC8, "Coder_Initials" = "Coder.Initials")
AMC8 <- rename(AMC8, "Process_Code" = "Process.Code")

#dataframe without DemandY.N. column
AMC_PC<- rbind(AMC1, AMC2, AMC3, AMC4, AMC5, AMC6, AMC7, AMC9)

# There are 242 duplicate tweets for Kappa scores
length(AMC_PC$Username)-length(unique(AMC_PC$Tweet.Id))

# Aliyah coded 39 different accounts
length(unique(AMC_PC$Username))

# Aliyah used 710 different process codes
length(unique(AMC_PC$Process_Code))


# read in as dataframe for coder LC
LC1 <- read.csv("/Users/aricaschuett/Documents/Process Codes/LucyTweets - LucyWeek1.csv")
LC2 <- read.csv("/Users/aricaschuett/Documents/Process Codes/LucyTweets - Week2.csv")
LC3 <- read.csv("/Users/aricaschuett/Documents/Process Codes/LucyTweets - Week3.csv")
LC4 <- read.csv("/Users/aricaschuett/Documents/Process Codes/LucyTweets - Week4.csv")
LC5 <- read.csv("/Users/aricaschuett/Documents/Process Codes/LucyTweets - Week5.csv")


# Reduce to the shared columns
LC1 <- LC1[,!names(LC1) %in% c("Date", "Time", "Specific", "Keywords", "Batch_Week")]
LC2 <- LC2[,!names(LC2) %in% c("Date", "Time", "Specific", "Batch_Week")]
LC3 <- LC3[,!names(LC3) %in% c("X.1", "Date",  "Time", "Date", "X",  "Batch_Week", "Keywords")]
LC4 <- LC4[,!names(LC4) %in% c("Date", "Time", "Keywords", "Batch_Week", "X", "X.1")]
LC5 <- LC5[,!names(LC5) %in% c("Specific", "Batch_Week")]

# #dataframe without DemandY.N. column
LC_PC <- rbind(LC1, LC2, LC3, LC4, LC5)

# There are 3 duplicate tweets for Kappa scores
length(LC_PC$Username)-length(unique(LC_PC$Tweet.Id))

# Lucy coded 23 different accounts
length(unique(LC_PC$Username))

# Lucy used 476 different process codes
length(unique(LC_PC$Process_Code))

# total process codes
LC_PC <- LC_PC[,!names(LC_PC) %in% c("Demand.Y.N.")]

# Read in Arica's codes
PC_dupes <- read.csv("/Users/aricaschuett/Documents/Process Codes/PCs - PCs.csv")
AS1 <- read.csv("/Users/aricaschuett/Documents/Process Codes/AricaWeek1 - AricaWeek1.csv")
AS2 <- read.csv("/Users/aricaschuett/Documents/Process Codes/ASCodes - completed.csv")
AS3 <- read.csv("/Users/aricaschuett/Documents/protest/NovCodes-completed-AS.csv")


AS1 <- AS1 %>% select(Tweet.Id, Date, Time, Text, Username, Process_Code, Coder_Initials, Memos)
AS2 <- AS2 %>% select(Tweet.Id,Date, Time, Text, Username, Process_Code, Coder_Initials, Memos)
AS3 <- AS3 %>% select(Tweet.Id, Date, Time, Text, Username, Process_Code, Coder_Initials, Memos)

AS_PC <-rbind(AS1, AS2, AS3)

# LC and AMC process codes
LC_AMC_PCs <- rbind(AMC_PC, LC_PC)

LC_AMC_PCs[c('Date', "Time")] <- str_split_fixed(LC_AMC_PCs$Datetime, ' ', 2)

LC_AMC_PCs <- LC_AMC_PCs %>% select(Tweet.Id, Date, Time, Text, Username, Process_Code, Coder_Initials, Memos)

PCs <- rbind(LC_AMC_PCs, AS_PC)

rm(AMC_PC, AMC1, AMC2, AMC3, AMC4, AMC5, AMC6, AMC7, AMC8, AMC9, AS_PC, AS1, AS2, AS3, 
   LC_AMC_PCs, LC_PC, LC1, LC2, LC3, LC4, LC5, PC_dupes)


# Unique codes in PCs
length(unique(PCs$Tweet.Id))

# Make uppercase in Process codes
PCs$Process_Code <- toupper(PCs$Process_Code)

# Remove obs with missing Process Codes to double check if they are duplicates or
# systemically excluded and code if necessary
PCs <- mutate(PCs, 
              Missing = ifelse(Process_Code == "" | Process_Code == "." | is.na(Process_Code), 1, 0))



# Remove Obs with missing PC1
PCs <- PCs[complete.cases(PCs[ , c('Process_Code')]), ]
PCs <- PCs[!(PCs$Process_Code == "" | PCs$Process_Code == "."),]
PCs <- PCs[!(PCs$Process_Code == "" | PCs$Process_Code == "."),]
PCs <- PCs[!(PCs$Process_Code == "?"),]
PCs <- PCs[!(PCs$Process_Code == " "),]

# Separate by ";"
PCs[c('PC1', 'PC2', 'PC3', 'PC4')] <- str_split_fixed(PCs$Process_Code, ';', 4)


# Remove whitespace before text 
PCs$PC1 <- (trimws(PCs$PC1))
PCs$PC2 <- (trimws(PCs$PC2))
PCs$PC3 <- (trimws(PCs$PC3))
PCs$PC4 <- (trimws(PCs$PC4))


write.csv(PCs, file = "/Users/aricaschuett/Documents/Process Codes/PCs4-19.csv")

# Apply Cities & States #####
PCs <- mutate(PCs, 
              City = ifelse(Username == "AlgebraProject", "Baltimore, MD",
                            ifelse(Username == "justcitydetroit", "Detroit, MI",
                                   ifelse(Username == "BALTLegal", "Baltimore, MD",
                                          ifelse(Username == "BLM_Nashville", "Nashville, TN",
                                                 ifelse(Username == "indy10people", "Indianapolis, IN",
                                                        ifelse(Username == "blm_memphis", "Memphis, TN",
                                                               ifelse(Username == "BLMLA", "Los Angeles, CA",
                                                                      ifelse(Username == "BLM_216", "Cleveland, OH",
                                                                             ifelse(Username == "BLMSacramento", "Sacramento, CA",
                                                                                    ifelse(Username == "BlackVisionsMN", "Minneapolis, MN",
                                                                                           ifelse(Username == "blm_sd", "San Diego, CA",
                                                                                                  ifelse(Username == "BlmTulsa", "Tulsa, OK",
                                                                                                         ifelse(Username == "BLMPhilly", "Philadelphia, PA",
                                                                                                                ifelse(Username == "BLMHOU", "Houston, TX",
                                                                                                                       ifelse(Username == "DetroitBLM", "Detroit, MI",
                                                                                                                              ifelse(Username == "CharlotteBLMTip", "Charlotte, NC",
                                                                                                                                     ifelse(Username == "denver_blm", "Denver, CO",
                                                                                                                                            ifelse(Username == "Blmmemphis", "Memphis, TN",
                                                                                                                                                   ifelse(Username == "BLMSeattleKC", "Seattle, WA",
                                                                                                                                                          ifelse(Username == "SeattleBLM", "Seattle, WA",
                                                                                                                                                                 ifelse(Username == "blmlasvegas", "Las Vegas, NV",
                                                                                                                                                                        ifelse(Username == "mkeprotests", "Milwaukee, WI",
                                                                                                                                                                               ifelse(Username == "hi4blacklives", "Honolulu, HI",
                                                                                                                                                                                      ifelse(Username == "BLMLouisville", "Louisville, KN",
                                                                                                                                                                                             ifelse(Username == "BLMPittsburgh", "Pittsburgh, PA",
                                                                                                                                                                                                    ifelse(Username == "TucsonBlm", "Tucson, AZ",
                                                                                                                                                                                                           ifelse(Username == "BLMLincoln", "Lincoln, NE",
                                                                                                                                                                                                                  ifelse(Username == "BLMLBC", "Long Beach, CA",
                                                                                                                                                                                                                         ifelse(Username == "209forBLM", "Stockton, CA",
                                                                                                                                                                                                                                ifelse(Username == "blmokc", "Oklahoma, OK",
                                                                                                                                                                                                                                       ifelse(Username == "blmsaPDX", "Portland, OR", 0))))))))))))))))))))))))))))))))

#### Non-City Dummy for National Organizations#####
PCs <- mutate(PCs, 
              NonCity = ifelse(City == 0, 1, 0))

# Full List of Process Codes ######
#create an observation for each process code. There will be multiple observations 
# for tweets with multiple process codes. 

PC_Index1Test <- PCs %>% dplyr::select("Tweet.Id", "Date", "Time", "Text", "PC1", "Username", "City", 
                                       "NonCity", "Memos", "Missing", "Coder_Initials")
PC_Index2Test <- PCs %>% dplyr::select("Tweet.Id", "Date", "Time", "Text", "PC2", "Username", "City", 
                                       "NonCity", "Memos", "Missing", "Coder_Initials")
PC_Index3Test <- PCs %>% dplyr::select("Tweet.Id", "Date", "Time", "Text", "PC3", "Username", "City", 
                                       "NonCity", "Memos", "Missing", "Coder_Initials")
PC_Index4Test <- PCs %>% dplyr::select("Tweet.Id", "Date", "Time", "Text", "PC4", "Username", "City", 
                                       "NonCity", "Memos", "Missing", "Coder_Initials")

colnames(PC_Index1Test) <- c("Tweet.Id", "Date", "Time", "Text", "PC", "Username", "City", 
                             "NonCity", "Memos", "Missing", "Coder_Initials")
colnames(PC_Index2Test) <- c("Tweet.Id", "Date", "Time", "Text", "PC", "Username", "City", 
                             "NonCity", "Memos", "Missing", "Coder_Initials")
colnames(PC_Index3Test) <- c("Tweet.Id", "Date", "Time", "Text", "PC", "Username", "City", 
                             "NonCity", "Memos", "Missing", "Coder_Initials")
colnames(PC_Index4Test) <- c("Tweet.Id", "Date", "Time", "Text", "PC", "Username", "City", 
                             "NonCity", "Memos", "Missing", "Coder_Initials")

PCs_Test <- rbind(PC_Index1Test, PC_Index2Test, PC_Index3Test, PC_Index4Test)
PCs <- PCs_Test[!(PCs_Test$PC == ""),]
rm(PC_Index1Test, PC_Index2Test, PC_Index3Test, PC_Index4Test, PCs_Test)


#PCs complete
length(unique(PCs$PC)) ### There are 2125 unique process codes

# sort PC Index by PC remove odd ones
PCs <- PCs[order(PCs$PC, decreasing = FALSE), ]
PCs <- PCs[!(PCs$PC == " "),]
PCs <- PCs[!(PCs$PC == ""),]
PCs <- PCs[!(PCs$PC == "."),]
PCs <- PCs[!(PCs$PC == "/"),]

#Write all PCs
#write.csv(PCs, file = "/Users/aricaschuett/Documents/Process Codes/PCs_All4-14.csv")


length(unique(PCs$PC))  # There are 2123 unique process codes

# Change Asking for to "requesting" to use 1 verb
PCs$PC[PCs$PC=="ASKING FOR PARTICIPATION" ]<-"REQUESTING PARTICIPATION"
PCs$PC[PCs$PC=="ASKING FOR PARTICIPATION" ]<-"REQUESTING PARTICIPATION"
PCs$PC[PCs$PC=="ASKING FOR PARTICIPATION" ]<-"REQUESTING PARTICIPATION"
PCs$PC[PCs$PC=="ASKING FOR PARTICIPATION" ]<-"REQUESTING PARTICIPATION"
PCs$PC[PCs$PC=="ASKING FOR PARTICIPATION" ]<-"REQUESTING PARTICIPATION"
PCs$PC[PCs$PC=="ASKING FOR PARTICIPATION" ]<-"REQUESTING PARTICIPATION"


# MERGING CATEGORIES #####
PCs <- mutate(PCs, 
              PC = ifelse(PC == "ASKING FOR PARTICIPATION", "REQUESTING PARTICIPATION",
                                   ifelse(PC == "ASKING FOR DONATION", "REQUESTING DONATION",
                                          ifelse(PC == "ASKING FOR DONATIONS", "REQUESTING DONATION",
                                                 ifelse(PC == "REQUESTING DONATIONS", "REQUESTING DONATION", 
                                                        ifelse(PC == "ASKING FOR PROTESTERS TO REMAIN PEACEFUL", "REQUESTING PEACEFUL PROTEST", # ENCOURAGING PEACEFUL PROTEST
                                                               ifelse(PC == "ASKING PROTESTORS TO REMAIN PEACEFUL", "REQUESTING PEACEFUL PROTEST", 
                                                                      ifelse(PC == "ASKING TO SIGN A PETITION", "PETITIONING", 
                                                                             ifelse(PC == "ASKING FOR COMMUNITY VOLUNTEERS", "REQUESTING LOCAL SUPPORT", 
                                                                                    ifelse(PC == "ASKING FOR COMMUNITY SUPPORT", "REQUESTING LOCAL SUPPORT", 
                                                                                           ifelse(PC == "ASKING FOR SUPPORT", "REQUESTING SUPPORT", 
                                                                                                  ifelse(PC == "ASKING FOR MORE SUPPORT", "REQUESTING SUPPORT",
                                                                                                         ifelse(PC == "ASKING FOR PARTICIAPTION", "REQUESTING PARTICIPATION",
                                                                                                                ifelse(PC == "ASKING FOR PETITION SIGNATURES", "PETITIONING",
                                                                                                                       ifelse(PC == "CALLING ATTENTION TO ABUSES OF POWER IN POLICING", "SPOTLIGHTING POLICE VIOLENCE", ###
                                                                                                                              ifelse(PC == "CALLING ATTENTION TO BLACK TRANS LIVES", "SPOTLIGHTING BLACK TRANS LIVES",
                                                                                                                                     ifelse(PC == "CALLING ATTENTION TO LETHAL POLICE ENCOUNTERS", "SPOTLIGHTING POLICE VIOLENCE",
                                                                                                                                            ifelse(PC == "CALLING ATTENTION TO POLICE VIOLENCE", "POTLIGHTING POLICE VIOLENCE",
                                                                                                                                                   ifelse(PC == "CALLING ATTENTION TO VIOLENCE AGAINST PROTESTERS FROM POLICE", "SPOTLIGHTING POLICE VIOLENCE AT PROTESTS",
                                                                                                                                                          ifelse(PC == "CALLING ATTENTION TO VIOLENCE EXPERIENCED BY  MARGINALIZED GROUPS", "SPOTLIGHTING POLICE VIOLENCE",
                                                                                                                                                                 ifelse(PC == "CALLING ATTENTION TO THE INTERSECTIONALITY", "SPOTLIGHTING INTERSECTIONALITY",
                                                                                                                                                                        ifelse(PC == "CALLING ATTENTION TO SYSTEMIC ROLE OF POWERFUL COMPANIES", "SPOTLIGHTING CORPORATE POWER",PC))))))))))))))))))))))
length(unique(PCs$PC_Reduced1))  # There are 2113 unique process codes
length(unique(PCs$PC)) # There are 2123 unique PCs originally 

write.csv(PCs, file = "/Users/aricaschuett/Documents/Process Codes/PCS_FOR_REGEX4-14.csv")


## Read in with updates #####
PCs <- read.csv("/Users/aricaschuett/Documents/Process Codes/PCS_FOR_REGEX4-14.csv")
length(unique(PCs$PC)) # There are 2123 unique PCs originally 


# Seperate into Verbs and Subjects
PCs[c('PC_Verbs', "PC_Subjects")] <- str_split_fixed(PCs$PC, ' ', 2)

# Correct spelling for verbs ####
#PCs$PC_Verbs[PCs$PC_Verbs=="RREPORTING"]<-"REPORTING"
#PCs$PC_Verbs[PCs$PC_Verbs=="REPRORTING"]<-"REPORTING"
#PCs$PC_Verbs[PCs$PC_Verbs=="REPROTING"]<-"REPORTING"

#PCs$PC_Verbs[PCs$PC_Verbs=="N/A"]<-NA

#PCs$PC_Verbs[PCs$PC_Verbs=="ADVERTISING"]<-"ANNOUNCING"
#PCs$PC_Verbs[PCs$PC_Verbs=="ANNOUNING"]<-"ANNOUNCING"
#PCs$PC_Verbs[PCs$PC_Verbs=="APPOLGIZING"]<-"APOLOGIZING"
#PCs$PC_Verbs[PCs$PC_Verbs=="APPOLOGIZING"]<-"APOLOGIZING"
#PCs$PC_Verbs[PCs$PC_Verbs=="ASSSOCIATING"]<-"ASSOCIATING"
#PCs$PC_Verbs[PCs$PC_Verbs=="CELBRATING"]<-"CELEBRATING"
#PCs$PC_Verbs[PCs$PC_Verbs=="CLARIFYNG"]<-"CLARIFYING"
#PCs$PC_Verbs[PCs$PC_Verbs=="CONDEMING"]<-"CONDEMNING"
#PCs$PC_Verbs[PCs$PC_Verbs=="DEMAND"]<-"DEMANDING"
#PCs$PC_Verbs[PCs$PC_Verbs=="ECOURAGING"]<-"ENCOURAGING"
#PCs$PC_Verbs[PCs$PC_Verbs=="ENCORAGING"]<-"ENCOURAGING"
#PCs$PC_Verbs[PCs$PC_Verbs=="ENCOURAING"]<-"ENCOURAGING"
#PCs$PC_Verbs[PCs$PC_Verbs=="URDGING"]<-"ENCOURAGING"
#PCs$PC_Verbs[PCs$PC_Verbs=="URGING"]<-"ENCOURAGING"
#PCs$PC_Verbs[PCs$PC_Verbs=="EXPERSSING"]<-"EXPRESSING"
#PCs$PC_Verbs[PCs$PC_Verbs=="INDENTIFYING"]<-"IDENTIFYING"
#PCs$PC_Verbs[PCs$PC_Verbs=="CONNECTING"]<-"LINKING"
#PCs$PC_Verbs[PCs$PC_Verbs=="ORGANIZING"]<-"ORGANIZING"
#PCs$PC_Verbs[PCs$PC_Verbs=="OUTLINING"]<-"ORGANIZING"
#PCs$PC_Verbs[PCs$PC_Verbs=="PLEAING"]<-"PLEADING"
#PCs$PC_Verbs[PCs$PC_Verbs=="PRESSUING"]<-"PRESSURING"
#PCs$PC_Verbs[PCs$PC_Verbs=="PRESURING"]<-"PRESSURING"
#PCs$PC_Verbs[PCs$PC_Verbs=="PROMTING"]<-"PROMOTING"
#PCs$PC_Verbs[PCs$PC_Verbs=="QUTOING"]<-"QUOTING"
#PCs$PC_Verbs[PCs$PC_Verbs=="SHAREING"]<-"SHARING"
#PCs$PC_Verbs[PCs$PC_Verbs=="ELEVATING"]<-"SPOTLIGHTING"


#PCs$PC_Verbs[PCs$PC_Verbs=="IDENTIFYINF"]<-"IDENTIFYING"
#PCs$PC_Verbs[PCs$PC_Verbs=="IDENFITYING"]<-"IDENTIFYING"

#PCs$PC_Verbs[PCs$PC_Verbs=="HONOURING"]<-"HONORING"

#PCs$PC_Verbs[PCs$PC_Verbs=="EXPRESSSING"]<-"EXPRESSING"
#PCs$PC_Verbs[PCs$PC_Verbs=="ENCOURGING"]<-"ENCOURAGING"
#PCs$PC_Verbs[PCs$PC_Verbs=="DISCUSS"]<-"DISCUSSING"
#PCs$PC_Verbs[PCs$PC_Verbs=="DENOUCNING"]<-"DENOUNCING"
#PCs$PC_Verbs[PCs$PC_Verbs=="DENOUCNIGN"]<-"DENOUNCING"
#PCs$PC_Verbs[PCs$PC_Verbs=="DENOUCNIGN"]<-"DENOUNCING"
#PCs$PC_Verbs[PCs$PC_Verbs=="CALLIGN"]<-"CALLING"
#PCs$PC_Verbs[PCs$PC_Verbs=="ANNOUCNING"]<-"ANNOUNCING"
#PCs$PC_Verbs[PCs$PC_Verbs=="ADIVISING"]<-"ADVISING"
#PCs$PC_Verbs[PCs$PC_Verbs=="REPEAT"]<-"REPEATING"
#PCs$PC_Verbs[PCs$PC_Verbs=="DISUCCING"]<-"DISCUSSING"
#PCs$PC_Verbs[PCs$PC_Verbs=="CRITIQUIING"]<-"CRITIQUING"
#PCs$PC_Verbs[PCs$PC_Verbs=="COMDEMNING"]<-"CONDEMNING"

#PCs$PC_Verbs[ PCs$PC_Verbs=="ADVERTISING"]<-"SHARING"

#PCs$PC_Verbs[ PCs$PC_Verbs=="ADVISING"]<-"ENCOURAGING"

#PCs$PC_Verbs[ PCs$PC_Verbs=="URGING"]<-"ENCOURAGING"
# Correct Spelling for Subjects #####
#PCs$PC_Subjects[PCs$PC_Subjects==" A SUPPORTIVE CELEBRITY"]<-"CELEBRITY"

length(unique(PCs$PC)) # There are 2123 unique PCs originally 


####  1 ENCOURAGING PEOPLE TO CONTACT LOCAL OFFICIALS ######
PCs$PC_2[ PCs$PC=="ENCOURAGING PROTESTERS TO CONTACT LOCAL OFFICIALS"|
          PCs$PC=="ENCOURAGING PROTESTERS TO ATTEND CITY COUNCIL MEETINGS"|
          PCs$PC=="ENCOURAGING PEOPLE TO ATTEND CITY COUNCIL MEETINGS"|
          PCs$PC=="ENCOURAGING PROTESTERS TO ATTEND CITY COUNCIL COMMITTEE MEETING"|
          PCs$PC=="ENCOURAGING PEOPLE TO CONTACT THEIR OFFICIALS"|
          PCs$PC=="ASKING PEOPLE TO CONTACT THEIR OFFICIALS"|
          PCs$PC=="CONTACTING LOCAL GOVERNMENT"|
          PCs$PC=="EXPLAINING HOW TO MOST EFFECTIVELY GET THE EMAIL THOUGH SPAM FILTERS"|
          PCs$PC=="CONTACTING LOCAL POLICE FORCE"|
          PCs$PC=="ENCOURAGING CONTACTING LOCAL OFFICIALS"|
          PCs$PC=="ENCOURAGING PEOPLE TO CONTACT GOV"|
          PCs$PC=="ENCOURAGING PEOPLE TO CONTACT MAYOR"|
          PCs$PC=="OFFERING OFFICIAL CONTACT INFORMATION"|
          PCs$PC=="POSTING OFFICIAL CONTACT INFORMATION"|
          PCs$PC=="SHARING CITY COUNCIL MEMBER CONTACT INFORMATION"| 
          PCs$PC=="ENCOURAGING PEOPLE TO CALL ELECTED OFFICIALS"| 
          PCs$PC=="ENCOURAGING CALLS TO ELECTED OFFICIALS"|
          PCs$PC=="ENCOURAGING INDIVIDUALS TO CALL MAYOR AND CITY COUNCIL AND MAKE DEMANDS"|
          PCs$PC=="ENCOURAGING INDIVDIUALS TO CALL ELECTED OFFICIALS ABOUT RECENT POLICE SHOOTING"|
          PCs$PC=="ENCOURAGING CALLS TO MAYOR AND POLICE"|
          PCs$PC=="ENCOURAGING PEOPLE TO CALL LOCAL OFFICIALS"|
          PCs$PC=="ENCOURAGING PEOPLE TO CALL THEIR CITY COUNCIL MEMBER BEFORE MEETING"|
          PCs$PC=="REQUESTING SUPPORTERS TO CALL ELECTED OFFICIALS"|
          PCs$PC=="SHARING SCRIPT FOR CALLING CITY COUNCIL"|
          PCs$PC=="ENCOURAGING PEOPLE TO CONCTACT ELECTED OFFICIALS"|
          PCs$PC=="ENCOURAGING PEOPLE TO PRESSURE OFFICIALS"|
          PCs$PC=="PROVIDING TEMPLATE TO MESSAGE LOCAL OFFICIALS"|
          PCs$PC=="SHARING CITY COUNCIL MEETING TIMES"|
          PCs$PC=="ENCOURAGING OTHERS TO DEMAND CITY COUNCIL DEFUND THE POLICE"|
          PCs$PC=="ENCOURAGING PEOPLE TO REACH OUT TO CITY COUNCIL AND MAYOR TO BAN NO KNOCK WARRANTS"|
          PCs$PC=="INVITING SUPPORTERS TO CITY COUNCIL MEETING"|
          PCs$PC=="PETITIONING CITY COUNCIL"| # ALSO IN ONLINE PARTICIPATION
          PCs$PC=="REPORTING COUNCIL MEETING"|
          PCs$PC=="SHARING LA POLICE COMISSION RECORDING BECUASE ZOOM REACHED CAPACITY"|
          PCs$PC=="DEMANDING DIVESTMENT FROM POLICE"|
          PCs$PC=="DEMANDING CIVILIAN OVERSIGHT OF POLICE"|
          PCs$PC=="DESCRIBING INSTANCE OF POLICE BRUTALITY"|
          PCs$PC=="DEMANDING DEMILITARIZATION OF POLICE"|
          PCs$PC=="DENYING THE POSSIBILITY OF POLICE REFORM"|
          PCs$PC=="REQUESTING EVIDENCE OF MISCONDUCT FROM POLICE"|
          PCs$PC=="ARGUING THAT MORE POLICE DOES NOT EQUAL MORE SAFETY"|
          PCs$PC=="CALLING OUT POLICE BRUTALITY"|
          PCs$PC=="SHARING LOCAL GOV MEETING TIMES AND LOCATIONS"|
          PCs$PC=="CRITICIZING POLICE HYPOCRISY"|
          PCs$PC=="PETITIONING IN LOUISVILLE"|
          PCs$PC=="SHARING DA CANDIDATE FORUM"|
          PCs$PC=="SHARING DA PUBLIC FORUM"|
          PCs$PC=="SHARING PETITITON FOR COUNCIL ACTION"|
          PCs$PC=="SHARING PUBLIC MEETING"|
          PCs$PC=="PETITIONING RECALL OF COUNTY ATTORNEY"|
          PCs$PC=="REPORTING THAT DEFUND PLEDGE AS GIVEN TO COUNCIL MEMBERS"|
          PCs$PC=="SHARING CITY COUNCIL MEETING STREAM"|
          PCs$PC=="SHARING HOW TO FIND YOUR COUNCIL MEMBER"|
          PCs$PC=="TELLING PROTESTERS TO GO TO COUNCIL MEETINGS" ]<-"ENCOURAGING PEOPLE TO CONTACT LOCAL OFFICIALS" 

####  2 SHARING PROTEST EVENT #####
PCs$PC_2[ PCs$PC=="ANNOUNCING A FUTURE PROTEST"|
          PCs$PC=="ANNOUCNING A PROTEST"  |
          PCs$PC=="ANNOUCNING PROTEST"  |	
          PCs$PC=="ANNOUNCING AN EVENT"  |	
          PCs$PC=="ANNOUNCING UPCOMING PROTEST"  |
          PCs$PC=="ANNOUNING SCHEDULED PROTEST"  |
          PCs$PC=="ANNOUNCING INTENT TO PROTEST" |
          PCs$PC=="ANNOUNCING PLANNED PROTEST"|
          PCs$PC=="ANNOUNCING FUTURE PROTESTS"|
          PCs$PC=="ANNOUNCING A PROTEST"|
          PCs$PC=="ANNOUNCING PROTEST"|
          PCs$PC=="ANNOUNCING SCHEDULED PROTEST"|
          PCs$PC=="REPORTING ON PROTEST EVENTS"|
          PCs$PC=="SHARING LOCATION OF PROTEST"|
          PCs$PC=="RECOUNTING EVENTS OF RECENT PROTEST"|
          PCs$PC=="INVITING TO PROTEST"|
          PCs$PC=="PROVIDING INFORMATION ON ORGANIZED PROTEST"|
          PCs$PC=="PROMOTING PROTEST WTH LOCATION AND DETAILS"|
          PCs$PC=="REPORTING BEGINNING OF PROTEST"|
          PCs$PC=="SHARING LOCATION AND TIME OF PROTEST"|
          PCs$PC=="ORGANIZING A PROTEST"|
          PCs$PC=="ORGANIZING PROTEST"|
          PCs$PC=="PROMOTING A PROTEST"|
          PCs$PC=="PROMOTING ACTIVITIES"|
          PCs$PC=="PROMOTING CAMPAIGNS"|
          PCs$PC=="PROMOTING EVENT"|
          PCs$PC=="PROMOTING MARCH HONORING VICTIMS"|
          PCs$PC=="PROMOTING NATIONAL M4BL EVENT WITH FOCUS ON MINNEAPOLIS"|
          PCs$PC=="PROMOTING ORGANIZER EVENTS"|
          PCs$PC=="PROVIDING INFORMATION ON ORGANIZED PROTESTS"|
          PCs$PC=="PROVIDING INFORMATION ON PROTESTS"|
          PCs$PC=="PROVIDING INFORMATION ON SCHEDULED PROTEST"|
          PCs$PC=="SHARING EVENT"|
          PCs$PC=="SHARING EVENTS ON ORGANIZATION WEBSITE"|
          PCs$PC=="SHARING EVENTS PLANNED FOR THE NEXT FEW DAYS"|
          PCs$PC=="SHARING FACEBOOK GROUP FOR PROTEST ACTIVITIES"|
          PCs$PC=="SHARING LOCAL EVENTS"|
          PCs$PC=="SHARING PUBLIC EVENT"|
          PCs$PC=="PROMOTING PEACEFUL PROTESTING"|
          PCs$PC=="PROMOTING SCHEDULED PROTEST"|
          PCs$PC=="PROMOTING SCHEDULED PROTESTS"| 	#PROMOTING SUBURBAN PROTESTS
          PCs$PC=="PROMOTING SUBURBAN PROTESTS"| #REPORTING ON FUTURE ORGANIZED PROTESTS
          PCs$PC=="REPORTING ON FUTURE ORGANIZED PROTESTS" |
          PCs$PC=="SHARING UPCOMING PROTEST EVENTS" |
          PCs$PC=="REPORTING SCHEDULED PROTEST" |
          PCs$PC=="REPORTING UPCOMING PROTEST" |
          PCs$PC=="ANNOUNCING PLANS FOR ACTION"]<-"SHARING PROTEST EVENT" #ANNOUNCING FUTURE PROTESTS

####  3 UPDATING PROTEST ACTIVITIES #####
PCs$PC_2[ PCs$PC=="ANNOUNCING PROTEST CHANGES"|
          PCs$PC=="UPDATING LOCATION OF PROTEST"  |
          PCs$PC=="REPORTING ON MARCH"|
          PCs$PC=="OFFERING PROTEST INFORMATION"|
          PCs$PC=="OFFERING PROTEST LIVE FEED"|
          PCs$PC=="UPDATING FOLLOWERS ON STATUS OF SCHEDULED PROTEST"  |
          PCs$PC=="UPDATING PROTEST EVENTS"  |
          PCs$PC=="UPDATING PROTEST LOCATION"  |
          PCs$PC=="UPDATING PROTEST LOCATIONS"  |
          PCs$PC=="UPDATING PROTEST PARTICIPATION NUMBERS"  |
          PCs$PC=="UPDATING PROTEST STATUS"  |
          PCs$PC=="UPDATING PROTEST TIME"  |
          PCs$PC =="WARNING PROTESTORS OF DANGER"|
          PCs$PC =="WARNING POTENTIAL PROTESTORS"|
          PCs$PC =="UPDATING ON PROTEST ACTIVITIES"|
          PCs$PC =="REPORTING LOCATION OF PROTESTERS"|
          PCs$PC =="REPORTING ON POLICE ACTION DURING PROTEST"|
          PCs$PC =="REPORTING ON PROTEST"|
          PCs$PC =="WARNING PROTESTORS"|
          PCs$PC =="REPORTING END OF PROTEST"|
          PCs$PC =="REPORTING ON PROTESTS"|
          PCs$PC =="REPORTING ON LOCATION OF PROTESTERS"|
          PCs$PC =="REPORTING ON PROTEST ACTIVITIES"|
          PCs$PC =="REPORTING ON PROTEST ARRESTS"|
          PCs$PC =="REPORTING ON PROTEST SUCCESS"|
          PCs$PC =="REPORTING ON PROTESTER INJURIES"|
          PCs$PC =="REPORTING PEACEFUL PROTEST"|
          PCs$PC =="REPORTING PEACEFUL PROTESTING"|
          PCs$PC =="REPORTING RESCHEDULED PROTEST"|
          PCs$PC =="REPORTING STATUS OF PROTEST"|
          PCs$PC =="SHARING IMAGES FROM PROTEST"|
          PCs$PC =="SHARING PROTEST LOCATION AND TIME"|
          PCs$PC =="SHARING PROTEST LOCATIONS"|
          PCs$PC =="UPDATING ON PROTEST LOCATIONS"|
          PCs$PC =="SHARING ANOTHER PROTEST LOCATION"|
          PCs$PC =="SHARING THREE PROTEST LOCATIONS AND TIMES"|
          PCs$PC =="SHARING PROTEST TIME AND LOCATION"|
          PCs$PC =="UPDATING ON PROTEST LOCATIONS"|
          PCs$PC =="WARNING OF COUNTER PROTESTERS"|
          PCs$PC =="WARNING PROTESTERS OF COPS LOCATION AND POSSIBLE INTENTIONS"|
          PCs$PC =="ADVISING PROTESTERS TO NOT GO TO A SITE ON PRIVATE PROPERTY	INTERESTING"|
          PCs$PC =="AGGREGATING PROTEST UPDATES"|
          PCs$PC == "SHARING PROTEST LOCATION" |
          PCs$PC =="ANNOUNCING END OF PROTEST"|
          PCs$PC =="ASKING FOR HELP SHARING PROTEST EVENTS"|
          PCs$PC =="CELEBRATING LARGE PROTEST TURNOUT"|
          PCs$PC =="COMMEMORATING END OF PROTEST"|
          PCs$PC =="CORRECTING PREVIOUS REPORT DURING PROTESTS"|
          PCs$PC =="DISCOURAGING PROTESTING ON PRIVATE PROPERTY"|
          PCs$PC =="ADVISING PROTESTERS TO NOT GO TO A SITE ON PRIVATE PROPERTY"|
          PCs$PC =="UPDATING ON PROTEST ACTIVITES"|
          PCs$PC =="SHARING PROTEST UPDATE"|
          PCs$PC=="REPORTING MOVEMENT OF PROTESTERS" |
          PCs$PC=="REPORTING NUMBERS OF PROTESTERS" |
          PCs$PC=="REPORTING OF PROTEST EVENTS" |
          PCs$PC=="REPORTING ON ACTIVITIES OF PROTESTERS" |
          PCs$PC=="REPORTING ON ACTIONS OF POLICE AT PROTEST" | 
          PCs$PC=="REPORTING ON ATTEMPTS TO KEEP PROTESTS PEACEFUL" |
          PCs$PC=="REPORTING ON CANCELLED PROTEST DUE TO WEATHER" |
          PCs$PC=="REPORTING ON CLASHES BETWEEN PROTESTERS AND POLICE IN ANOTHER CITY" |
          PCs$PC=="REPORTING ON DANGEROUS CONDITIONS FOR PROTESTERS" |
          PCs$PC=="REPORTING ON DEESCALATION AT PROTEST" |
          PCs$PC=="REPORTING ON HIGH PROTEST TURNOUT" |
          PCs$PC=="REPORTING ON ORGANIZED PROTEST" |
          PCs$PC=="REPORTING ON POTENTIAL ESCALATION BETWEEN POLICE AND PROTESTERS" |
          PCs$PC=="REPORTING ON PROTEST CROWDS" |
          PCs$PC=="REPORTING ON PROTEST ROUTE" |
          PCs$PC=="REPORTING ON PROTEST TURNOUT" |
          PCs$PC=="SHARING TRANSPORTATION CHANGES"|
          PCs$PC=="REPORTING ON RESCHEDULED PROTESTS" |
          PCs$PC=="REPORTING ON SCHEDULED PROTEST" |
          PCs$PC=="REPORTING ON STATUS OF PROTEST" |
          PCs$PC=="REPORTING ON SUCCESS OF PROTEST" |
          PCs$PC=="REPORTING ON TENSION BETWEEN POLICE AND PROTESTERS" |
          PCs$PC=="REPORTING ON THE PROTESTS" |
          PCs$PC=="REPORTING ON THEIR WHEREABOUTS DURING A PROTESTS" |
          PCs$PC=="REPORTING POLICE PRESENCE AT PROTEST" |
          PCs$PC=="REPORTING PROTESTERS CHANTING" |
          PCs$PC=="REPORTING TENSIONS BETWEEN PROTESTERS AND POLICE" |
          PCs$PC=="REPORTING THAT A PROTEST IS WRAPPING UP" |
          PCs$PC=="REPORTING THAT SUBURBAN PROTESTS WERE WELL ATTENDED" |
          PCs$PC=="RESPONDING TO QUESTIONS ABOUT PROTESTS" |
          PCs$PC=="SHARING A PROTEST ORGANIZED BY ANOTHER GROUP" |
          PCs$PC=="SHARING HASHTAGS FOR PROTEST UPDATES" |
          PCs$PC=="SHARING IMAGE OF PROTESTER" |
          PCs$PC=="SHARING IMAGES FROM PROTESTS" |
          PCs$PC=="SHARING IMAGES OF PEACEFUL PROTESTS" |
          PCs$PC=="SHARING PHOTOS FROM RECENT PROTESTS" |
          PCs$PC=="SHARING PHOTOS OF LARGE PROTESTS" |
          PCs$PC=="SHARING PHOTOS OF PROTEST AND THANKING FOR PARTICIPATION" |
          PCs$PC=="SHARING PICTURES OF PEACEFUL PROTESTERS" |
          PCs$PC=="SHARING PROJECTIONS OF PROTEST SLOGANS ON BUILDINGS" |
          PCs$PC=="SHARING PROTEST ACTIVITIES" |
          PCs$PC=="SHARING PROTEST AT MAYORS HOUSE" | # mayors house
          PCs$PC=="SHARING PROTEST DETAILS" |
          PCs$PC=="SHARING PROTEST UPDATES" |
          PCs$PC=="CLARIFYING THAT THEY ARE NOT HOSTING AN EVENT" |
          PCs$PC=="PROMISING TO OFFER UPDATES" |
          PCs$PC=="OFFERING STATEMENTS ABOUT PROTEST" |
          PCs$PC=="OFFERING UPDATES ON ACTION" |
          PCs$PC=="PROMISING TO KEEP USERS UPDATED" |
          PCs$PC=="PROMISING USEFUL INFORMATION" |
          PCs$PC=="PROVIDING PHOTOS FROM RECENT PROTEST" |
          PCs$PC=="PROVIDING UPDATES ON PROTESTS" |
          PCs$PC=="PROVIDING VIDEO AND PHOTO EVIDENCE FROM RIOTING" |
          PCs$PC=="RECOUNTING EVENTS AT RECENT PROTEST" |
          PCs$PC=="RECOUNTING STATEMENT FROM PROTESTERS" |
          PCs$PC=="RECOUNTING TENSION BETWEEN POLICE AND PROTESTERS" |
          PCs$PC=="REPORTING ACTIVITY OF PROTESTERSM" |
          PCs$PC=="REPORTING AN ARREST" |
          PCs$PC=="REPORTING DEATH" |
          PCs$PC=="REPORTING DEATHS" |
          PCs$PC=="REPOSTING INFORMATION FROM FOLLOWERS" |
          PCs$PC=="SHARING IMAGE OF PEAFECUL PROTEST TURNED VIOLENT" |
          PCs$PC=="SHARING INSTAGRAM HANDLE FOR MORE INFORMATION" |
          PCs$PC=="SHARING NEGOTIATION TO HAVE POLICE LEAVE PRIMARY PROTEST AREA" |
          PCs$PC=="SHARING NEWS ABOUT PORTLAND PROTEST IN SPANISH" |
          PCs$PC=="SHARING NEWS VIDEO OF MASSIVE PROTEST" |
          PCs$PC=="STATING THAT THEY WILL SHARE MORE ABOUT AN EVENT WHEN THEY HAVE PERMISSION FROM THE PEOPLE AFFECTED" |
          PCs$PC=="CLARIFYING THE PRIMARY AGGRESSOR IN PROTEST" |
          PCs$PC=="SHARING PROTESTER MEETING TIME AND LOCATION" |
          PCs$PC=="TELLING PROTESTORS TO LEAVE" |
          PCs$PC=="SHARING VIDEO OF PROTESTER CHANTING" |
          PCs$PC=="SHARING WAYS TO STAY CONNECTED TO PROTEST EVENTS" |
          PCs$PC=="UPDATING PROTEST" |
          PCs$PC=="UPDATING ON POLICE BEHAVIOR AT PROTEST" |
          PCs$PC=="STATING THAT POLICE ARE INCITING VIOLENCE ACROSS THE COUNTRY" |
          PCs$PC=="REPORTING AN EVENT" |
          PCs$PC=="REPORTING BEGINNING OF SPEECHES AT PROTEST" |
          PCs$PC=="REPORTING EFFORTS FOR VOTER REGISTRATION AT PROTEST" |
          PCs$PC=="REPORTING LACK OF UPDATES" |
          PCs$PC=="REPORTING LOCATION OF PROTESTER" |
          PCs$PC=="REPORTING NONVIOLENT POLICE BEHAVIOR" |
          PCs$PC=="REPORTING ON DANGEROUS WEATHER" |
          PCs$PC=="REPORTING ON DANGEROUS WEATHER CONDITIONS" |
          PCs$PC=="REPORTING ON DEFACEMENT OF BLM MEMORIAL" |
          PCs$PC=="REPORTING ON ESCALATING VIOLENCE AT PROTEST" |
          PCs$PC=="REPORTING ON EVENT" |
          PCs$PC=="REPORTING ON LOCATION OF RIOT POLICE" |
          PCs$PC=="REPORTING ON MEALS PROVIDED FOR PROTESTERS" |
          PCs$PC=="REPORTING ON NATIONAL GUARD" |
          PCs$PC=="REPORTING ON POLICE ARRESTS" |
          PCs$PC=="REPORTING ON POLICE BRUTALITY" |
          PCs$PC=="REPORTING ON POLICE WHEREABOUTS" |
          PCs$PC=="REPORTING ON POTENTIAL ESCALATION OF VIOLENCE" |
          PCs$PC=="REPORTING ON POTENTIAL ESCALATION TO VIOLENCE" |
          PCs$PC=="REPORTING ON STATE PROTEST RESPONSE" |
          PCs$PC=="REPORTING PEOPLE DANCING AT PROTEST" |
          PCs$PC=="REPORTING RISING TENSIONS" |
          PCs$PC=="REPORTING USE OF TEAR GAS" |
          PCs$PC=="ASSURING FOLLOWERS OF THE ACCURACY OF PROTEST FLIERS" |
          PCs$PC=="BROADCASTING A LIVE EVENT" |
          PCs$PC=="CLARIFYING EVENT INFORMATION" |
          PCs$PC=="CLARIFYING EVENT INFORMATION" |
          PCs$PC=="REPORTING VIOLENCE AT PROTESTS" |
          PCs$PC=="CROWDSOURCING PROTEST UPDATES"|
          PCs$PC=="REPORTING VIOLENCE DIRECTED AT PROTESTERS FROM BYSTANDER" |
          PCs$PC=="UPDATING FOLLOWERS" ]<-"UPDATING PROTEST ACTIVITIES" #UPDATING PROTEST EVENTS



####  4 REQUESTING DONATION #####
PCs$PC_2[ PCs$PC=="FUNDRAISING FOR SEVERAL NONPROFITS"|
          PCs$PC=="FUNDRAISING BY SELLING TSHIRTS"|
          PCs$PC=="FUNDRAISING FOR BLACK TRANS YOUTH"|
          PCs$PC=="FUNDRAISING FOR BLM"|
          PCs$PC=="FUNDRAISING FOR TRANSITIONAL HOUSING FOR TRANS PEOPLE"|
          PCs$PC=="REQUESTING DONATIONS AND SUPPORT FOR THOSE RELEASED FROM JAIL"|
          PCs$PC=="REQUESTING DONATIONS FOR ACTIVISTS"|
          PCs$PC=="REQUESTING DONATIONS FOR BAIL"|
          PCs$PC=="REQUESTING DONATIONS FOR BLACK TRANS WOMAN  WHO WAS ATTACKED"|
          PCs$PC=="REQUESTING DONATIONS FOR BLACK-TRANS VICTIM OF VIOLENCE"|
          PCs$PC=="REQUESTING DONATIONS FOR BLM CLE"|
          PCs$PC=="REQUESTING DONATIONS FOR GAS MASKS"|
          PCs$PC=="REQUESTING DONATIONS FOR INJURED PROTESTER"| #REQUESTING DONATIONS FOR NFP
          PCs$PC=="REQUESTING DONATIONS FOR NFP"|
          PCs$PC=="REQUESTING DONATIONS FOR ORPHANS"|
          PCs$PC=="REQUESTING DONATIONS FOR PROTESTERS"|
          PCs$PC=="REQUESTING DONATIONS FOR PROTESTERS IN ANOTHER CITY"|
          PCs$PC=="REQUESTING DONATIONS FOR TAYLOR GOFUNDME"|
          PCs$PC=="REQUESTING DONATIONS OF FOOD"|
          PCs$PC=="REQUESTING DONATIONS TO REBUILD DAMAGED BUSINESSES"|
          PCs$PC=="SEEKING HELP FOR FAMILY OF VICTIM OF POLICE VIOLENCE"|
          PCs$PC=="SHARING WAYS TO PARTICIPATE THAT AREN'T PROTEST" |
          PCs$PC=="ASKING FOR DONATION" |
          PCs$PC=="PROMOTING MEMORIAL FUND" |
          PCs$PC=="PROMOTING MUTUAL AID" |
          PCs$PC=="SHARING FUNDRAISING EFFORT" |
          PCs$PC=="SHARING PROCEEDS FROM VIDEO FOR BLMLA" |
          PCs$PC=="SHARING WHAT DONATIONS ARE USED FOR" |
          
          PCs$PC=="ASKING FOR DONATIONS" |
          PCs$PC=="ASKING FOR DONATIONS AT DROPOFF SITES" |
          PCs$PC=="ASKING FOR DONATIONS FOR VULNERABLE INDIVIDUAL" |
          PCs$PC=="OFFERING DONATION SUGGESTIONS" |
          PCs$PC=="CLARIFYING HOW TO DONATE TO RECIPIENT BECUASE CASH APP ISNT WORKING" |
          PCs$PC=="SHARING CASHAPP OF WOMAN WHO CREATED A VIRAL VIDEO OF COP BEAHVIOR" |
          PCs$PC=="PROMOTING LOUISVILLE BAILFUND" |
          PCs$PC=="INSTRUCTING WHERE TO DONATE" |
          PCs$PC=="ASKING USERS TO SUPPORT PROTESTORS"|	
          PCs$PC=="ASKING TO SUPPORT VICTIM'S FAMILY"|
          PCs$PC=="ASKING WHERE DONATIONS GO" | 
          PCs$PC=="ASKING WHERE TO SEND SUPPORT" | 
          PCs$PC=="CLARIFYING HOW TO DONATE TO BLACK TRANS WOMAN WHO WAS ATTACKED" | 
          PCs$PC=="CLARIFYING HOW TO DONATE TO RECIPIENT BECUASE CASH APP ISNT WORKING" |
          PCs$PC=="REQUESTING DONATIONS"|
          PCs$PC=="FUNDRAISING FOR SEVERAL NONPROFITS"|
          PCs$PC=="ASKING FOR DONATION" |
          PCs$PC=="ASKING FOR DONATIONS" |
          PCs$PC=="REQUESTING DONATIONS" |
          PCs$PC=="WELCOMING DONATIONS" |
          PCs$PC=="CALLING FOR RESOURCES TO MINNEAPOLIS PROTESTERS" |
          PCs$PC=="SHARING DONATION DROPOFF SITE"|
          PCs$PC == "ASKING FOR DONATION"|
          PCs$PC == "SHARING DONATION LINKS"|
          PCs$PC == "SHARING DONATION LOCATION FOR MINNEAPOLIS PROTESTERS"|
          PCs$PC == "REQUESTING ACCESSIBILITY DONATIONS FOR DISABLED PROTESTERS"|
          PCs$PC == "ASKING FOR FOOD DONATIONS"]<-"REQUESTING DONATION"

####  5 PROVIDING LEGAL SUPPORT #####
PCs$PC_2[ PCs$PC=="OFFERING LEGAL SUPPORT"|
          PCs$PC=="OFFERING LEGAL SUPPORT TO PROTESTORS" |
          PCs$PC=="PROMISING ACCURATE LEGAL ADVICE"]<-"PROVIDING LEGAL SUPPORT" 


####  6 CONDEMNING VIOLENT PROTEST SUPPRESSION #####
PCs$PC_2[ PCs$PC=="CONDEMNING USE OF FORCE ON CROWD" |
          PCs$PC=="CONDEMNING THE USE OF TEAR GAS AT PROTESTS" |
          PCs$PC=="CRITICIZING STATE RESPONSE TO PROTEST" |
          PCs$PC=="CRITICIZING STATE PROTEST RESPONSE" |
          PCs$PC=="CALLING OUT BRUTALITY AGAINST PROTESTERS" |
          PCs$PC=="CALLING OUT BRUTALITY" |
          PCs$PC=="DEMANDING DIFFERENT WAYS TO MANAGE PROTESTS" |
          PCs$PC=="CRITIQUING POLICING TACTICS" |
          PCs$PC=="REPORTING POLICE VIOLENCE TOWARDS PROTESTERS" |
          PCs$PC=="REPORTING THAT PEACEFUL PROTESTER WAS ATTACKED BY POLICE" |
          PCs$PC=="REPORTING THAT POLICE ARE SEIZING AND DENYING PPE" |
          PCs$PC=="REPORTING THAT POLICE ARE TRYING TO DIVIDE THE PROTESTERS BY LABELING SOME AS GOOD AND OTHERS AS BAD" |
          PCs$PC=="REPORTING THAT TRUMP WISHES TO USE MILITARY FORCE AGAINST PROTESTERS" |
          PCs$PC=="SHARING NEWS ARTICLE ABOUT LOCAL POLICE TEARGASING PROTESTERS" |
          PCs$PC=="SHARING POLICE INFLICTED PROTEST INJURIES TO YOUTH PROTESTERS" |
          PCs$PC=="SHARING POLICE PEPPER SPRAING PEACEFUL PROTESTERS" |
          PCs$PC=="SHARING POLICE TACTIC" |
          PCs$PC=="SHARING PROTESTERS AT COUNTY JAIL" |
          PCs$PC=="SHARING VIDEO OF MILITARIZED POLICE" |
          PCs$PC=="SHARING VIDEO OF POLICE TEAR GASING PROTESTERS" |
          PCs$PC=="DEMANDING JUSTICE FOR A FALLEN PROTESTOR" |
          PCs$PC=="REPORTING ON VIOLENCE AGAINST PROTESTORS" | 
          PCs$PC=="CRITICIZING POLICE BEHAVIOR/ REPORTING ON PROTEST EVENTS" |
          PCs$PC=="REPORTING ON POLICE VIOLENCE TOWARDS PROTESTERS" | 
          PCs$PC=="CALLING OUT BRUTALITY AGAINST PROTESTERS" |
          PCs$PC=="ADDRESSING POLICE BRUTALITY AGAINST PROTESTERS" |
          PCs$PC=="ADDRESSING VIOLENCE AGAINST PROTESTERS" ]<-"CONDEMNING VIOLENT PROTEST SUPPRESSION" 

####  7 HONORING A VICTIM OF POLICE VIOLENCE ####
PCs$PC_2[ PCs$PC=="HONOURING A VICTIM" |
          PCs$PC=="MEMORIALIZING A VICTIM" |
          PCs$PC=="REMEMBERING VICTIMS"  |
          PCs$PC=="MEMORIALIZING"  |
          PCs$PC=="REMEMBERING VIOLENCE"  |
          PCs$PC=="EXPRESSING SUPPORT FOR VICTIM"  |
          PCs$PC=="HONORING LOST LIFE"  |
          PCs$PC=="HONORING MCCLAIN"  |
          PCs$PC=="HONORING THOSE WHO HAVE PASSED"  |
          PCs$PC=="HONORING VICTIMS OF RACISM"  |
          PCs$PC=="MEMORIALIZING TONY MCDADE"  |
          PCs$PC=="DEMANDING BLACK WOMEN VICTIMS ARE REMEMBERED"  |
          PCs$PC=="ENCOURAGING PROTESTERS TO MEMORIALIZE VICTIMS OF POLICE BRUTALITY"  |
          PCs$PC=="INVOKING TAYLOR AND MCATEE"  |
          PCs$PC=="COMMENTING ON THE CONSTANT ENSLAUGHT OF POLICE VICTIMS"  |
          PCs$PC=="MEMORIALIZING "  |
          PCs$PC=="HONORING BREONNA TAYLOR"  |
          PCs$PC=="HONORING TAYLOR"  |
          PCs$PC=="REPORTING MURDER"  |
          PCs$PC=="REPORTING MURDER AND HARASSMENT"  |
          PCs$PC=="REPORTING UNJUST DEATH"  |
          PCs$PC=="SHARING BILLBOARD HONORING TAYLOR"  |
          PCs$PC=="SHARING BREONNA TAYLORS BIRTHDAY"  |
          PCs$PC=="SHARING IMAGE AND STORY OF PROTESTER SHOT IN THE HEAD WITH RUBBER BULLET"  |
          PCs$PC=="SHARING MEME MEMORIALIZING ERIC GARNER"  |
          PCs$PC=="SHARING VIGIL HONORING TAYLOR"  |
          PCs$PC=="SHOWING SUPPORT FOR VICTIM'S FAMILY"  |
          PCs$PC=="WISHING BREONNA TAYLOR A HAPPY BIRTHDAY"  |
          PCs$PC=="REMEMBERING INDIA KAGER"  |
          PCs$PC=="LISTING INCIDENTS OF POLICE BRUTALITY, HONORING VICTIMS"  |
          PCs$PC=="RECOGNIZING LOST LIFE"  |
          PCs$PC=="RECOUNTING LOSS OF LIVES"  |
          PCs$PC=="SHARING NAMES OF BLACK AMERICANS KILLED BY POLICE"  |
          PCs$PC=="REMEMBERING VICTIM" | 
          PCs$PC=="REMEMBERING VICTIM'S NAME"|
          PCs$PC=="HONORING A VICTIM" |
          PCs$PC=="HONORING LOCAL VICTIM" |
          PCs$PC=="HONORING TAYLORS BIRTHDAY" |
          PCs$PC=="MEMORIALIZING THOSE KILLED BY POLICE" |
          PCs$PC=="MOBILIZING IN THE NAME OF TAYLOR" |
          PCs$PC=="DEMANDING JUSTICE FOR A POLICE BRUTALITY VICTIM" |
          PCs$PC=="CELEBRATING LIFE OF VICTIM" |
          PCs$PC=="CENTERING VICTIMS" |
          PCs$PC=="CALLING FOR JUSTICE" |
          PCs$PC=="DEMANDING JUSTICE FOR LOCAL VICTIMS" |
          PCs$PC=="DEMANDING JUSTICE FOR A VICTIM OF POLICE BRUTALITY" |
          PCs$PC=="DEMANDING JUSTICE FOR VICTIM OF POLICE BRUTALITY" |
          PCs$PC=="DEMANDING JUSTICE FOR VICTIMS AND THEIR FAMILIES" |
          PCs$PC=="DEMANDING JUSTICE FOR VICTIMS OF POLICE BRUTALITY" |
          PCs$PC=="HONORING VICTIM IN ANOTHER COUNTRY" |
          PCs$PC=="INVOKING LOCAL VICTIM" |
          PCs$PC=="HONORING VICTIMS"  |
          PCs$PC=="HONORING VICTIM" |
          PCs$PC=="HONORING POLICE BRUTATLITY"|
          
          PCs$PC=="HONORING A VICTIM OF POLICE BRUTALITY"|
          PCs$PC=="MEMORIALIZING VICTIM" |
          PCs$PC=="HONORING A POLIC BRUTALITY VICTIM"  |
          PCs$PC=="DEMANDING JUSTICE FOR VICTIM" |
          PCs$PC=="DEMANDING JUSTICE FOR VICTIMS"|
          PCs$PC=="HONORING VICTIMS OF POLICE BRUTALITY" |
          PCs$PC=="HONORING VICTIMS OF POLICE VIOLENCE"|
          PCs$PC=="HONORING POLICE BRUTALITY VICTIMS" |
          PCs$PC=="IDENTIFYING VICTIM"  |
          PCs$PC=="NAMING VICTIMS OF POLICE VIOLENCE" |
          PCs$PC=="MEMORIALIZING VICTIMS"|
          PCs$PC=="NAMING VICTIMS OF POLICE VIOLENCE" |
          PCs$PC=="REMEMBERING AND HONORING VICTIM"|
          PCs$PC=="REPORTING ON REMEBERANCE OF VICTIM" |
          PCs$PC=="REPORTING PROTESTERS HONORING VICTIMS"  |
          PCs$PC =="MEMORIALIZING"|
          PCs$PC == "HONORING A POLICE BRUTALITY VICTIM"]<-"HONORING A VICTIM OF POLICE VIOLENCE" 

####  8 IMAGINING FUTURES######
PCs$PC_2[PCs$PC=="BUILDING A BETTER FUTURE" |
         PCs$PC=="CALLING FOR POLICE ALTERNATIVES" |
         PCs$PC=="CALLING FOR VISONARY LEADERSHIP" | 
         PCs$PC=="DISCUSSING THE NEEDS OF A SUCCESSFUL MOVEMENT" | 
         PCs$PC=="DISCUSSING THE WAY TO ACHIEVE SUCCESS" | 
         PCs$PC=="DOUBTING THE EFFICACY OF POLICING" | 
         PCs$PC=="DOUBTING THE NECESSITY OF POLICE" | 
         PCs$PC=="ENCOURAGING ALTERNATIVES TO POLICE" | 
         PCs$PC=="ENCOURAGING CREATIVE THINKING" | 
         PCs$PC=="ENCOURAGING PEOPLE TO HAVE VISION" | 
         PCs$PC=="ENVISIONING A BETTER FUTURE" | 
         PCs$PC=="IMAGINING A WORLD WITHOUT POLICE" ]<-"IMAGINING FUTURES"


####  9 SEEKING LEGAL HELP ######
PCs$PC_2[PCs$PC=="SEEKING LEGAL EXPERTS" |
         PCs$PC=="SEEKING LEGAL HELP" |
         PCs$PC=="SEEKING LEGAL PROFFESIONALS" | 
         PCs$PC == "SEEKING ATTORNEYS FOR PROTESTORS"]<-"SEEKING LEGAL HELP"

####  10 ABOLISHING PRISON #####
PCs$PC_2[PCs$PC=="ABOLISHING PRISON COMPLEX" |
         PCs$PC=="ABOLISHING THE PRISON INDUSTRIAL COMPLEX" |
         PCs$PC=="DEMANDING PRISON ABOLITION"|
         PCs$PC=="EDUCATING ON PRISON ABOLITION"|
         PCs$PC=="SUPPORTING PRISON ABOLITION MOVEMENT"|
         PCs$PC=="ENDORSING ABOLITION"|
         PCs$PC=="PROMOTING PRISON ABOLITION"|
         PCs$PC=="DEMANDING DEMOLISHING A JAIL"|
         PCs$PC=="DEMANDING DEMOLISHING A PRISON"|
         PCs$PC=="CRITICIZING RELIANCE ON INCARCERATION"|
         PCs$PC == "ABOLITION"]<-"DEMANDING PRISON ABOLITION"  


####  11 DEMANDING PRISONER RELEASE #####
PCs$PC_2[PCs$PC=="CRITICIZING THE TREATMENT OF INMATES DURING COVID" |
         PCs$PC=="DEMANDING THE RELEASE OF A PRISONER" |
         PCs$PC=="DEMANDING RELEASE OF A PRISONER" |
         PCs$PC=="PETITIONING THE RELEASE OF FORMER BPP ACTIVIST IN NY" |
         PCs$PC=="CALLING FOR PRISONER RELEASES BECUASE OF COVID" |
         PCs$PC=="CALLING FOR RELEASE OF POLITICAL PRISONER WITH COVID" |
         PCs$PC=="CALLING ON COMMIISSIONER ANNUCCI TO REALSE ANTHONY BOTTOM WHO HAS COVID." |
         PCs$PC=="CALLING ON GOVERNOR FOR RELEASE OF ANTHONY BOTTOM WITH COVID" ]<-"DEMANDING PRISONER RELEASE"  


####  12 DEMANDING REMOVING POLICE FROM SCHOOLS #####
PCs$PC_2[PCs$PC=="CRITICIZING POLICE PRESENCE IN SCHOOLS" |
         PCs$PC=="DEMANDING REMOVING POLICE FROM SCHOOLS" |
         PCs$PC=="DEMANDING SCHOOL DISTRICT END CONTRACT WITH POLICE" |
         PCs$PC=="ENCOURAGING END OF SCHOOL RESOURCE OFFICERS" |
         PCs$PC=="PRESSURING PUBLIC SCHOOLS TO CUT TIES WITH POLICE" |
         PCs$PC=="REPORTING ON POLICE PRESECNE IN SCHOOLS" ]<-"DEMANDING REMOVING POLICE FROM SCHOOLS"  

####  13 DEMANDING ABORTION ACCESS #####
PCs$PC_2[PCs$PC=="ADVOCATING FOR ABORTION ABORTION PILL ACCESS" |
         PCs$PC=="ANNOUNCING A LAWSUIT FOR GREATER ACCESS TO ABORTION PILLS" |
         PCs$PC=="ANNOUNCING EFFORTS TO EXPAND ABORTION CARE"|
         PCs$PC=="COLLABORATING WITH ALCU FOR ABORTION PILL ACCESS"|
         PCs$PC=="DISCUSSING LACK OF ABORTION ACCESS"|
         PCs$PC=="DOUBLING DOWN ON CALLS FOR ABOLITION"]<-"DEMANDING ABORTION ACCESS"  

####  14 VIDEO OF POLICE VIOLENCE #####
#PCs$PC_Subjects[ PCs$PC_Subjects=="VIDEO OF POLICE BRUTATLITY" |
#                   PCs$PC_Subjects=="VIDEO OF POLICE TEAR GASING PROTESTERS" |
#                   PCs$PC_Subjects == "VIDEO OF ELDERLY PROTESTER SHOVED BY POLICE" | #POLICE FOOTAGE
#                   PCs$PC_Subjects == "VIDEO OF AGGRESSIVE POLICE BEHAVIOR" |
#                   PCs$PC_Subjects == "VIDEO EVIDENCE OF POLICE VIOLENCE" |
#                   PCs$PC_Subjects == "VIDEO EVIDENCE OF POLICE BRUTALITY" |
#                   PCs$PC_Subjects == "POLICE PEPPER SPRAING PEACEFUL PROTESTERS" |
#                   PCs$PC_Subjects == "POLICE FOOTAGE"]<-"VIDEO OF POLICE VIOLENCE"

####  15 CELEBRATING POLICY VICTORY #####
PCs$PC_2[ PCs$PC=="SHARING ACCOMPLISHMENTS OF PROTESTERST IN OTHER CITIES" | #SHARING REPARATIONS IN EVANSTON
          PCs$PC=="SHARING REPARATIONS IN EVANSTON" | #ANNOUNCING CITY PLANS TO DEFUND POLICE
          PCs$PC=="ANNOUNCING CITY PLANS TO DEFUND POLICE"]<-"CELEBRATING POLICY VICTORY" 


####  16 ACCUSING ELECTED OFFICIALS OF DISHONESTY #####
PCs$PC_2[PCs$PC=="ACCUSING ELECTED OFFICIAL OF SLOWWALKING POLICE TRANSPARENCY"]<- "ACCUSING ELECTED OFFICIALS OF DISHONESTY" 


####  17 ACCUSING POLICE OF DISHONESTY ####  
PCs$PC_2[PCs$PC=="ADDRESSING POLICE BRUTALITY"|
         PCs$PC=="ACCUSING POLICE OF SECRECY"]<- "ACCUSING POLICE OF DISHONESTY" 

####  18 ACCUSING POLICE OF DISHONESTY ####  
PCs$PC_2[PCs$PC=="ASKING ALLIES TO PROTECT MINORITIES"]<- "ADVISING ALLIES" 

####  19 ENCOURAGING PROTEST SAFETY ####
PCs$PC_2[PCs$PC=="ENCOURAGING SAFE PROTESTING"|
         PCs$PC=="SHARING SAFE PROTESTING TIPS"|
         PCs$PC=="ENCOURAGING PROTESTERS TO BE SAFE"|
         PCs$PC=="ENCOURAGING SAFETY"|
         PCs$PC=="SHARING COVID TESTING INFO"|
         PCs$PC=="SHARING FREE COVID TESTING SITES"|
         PCs$PC=="OFFERING FREE COVID TESTING"|
         PCs$PC=="SHARING COVID TESTING SITE"|
         PCs$PC=="SHARING FREE COVID TESTING SITE"|
         PCs$PC=="RELAYING SAFETY GUIDELINES"|
         PCs$PC=="ENCOURAGING COVID TESTING"|
         PCs$PC=="ENCOURAGING SAFETY WHILE RECORDING OFFICERS"|
         PCs$PC=="ADVISING MASK WEARING"|
         PCs$PC=="ADVISING PROTESTERS TO WEAR MASKS"|
         PCs$PC=="ENCOURAGING PROTESTERS TO FIND SAFETY"|
         PCs$PC=="ENCOURAGING PROTESTERS TO STAY SAFE"|
         PCs$PC=="PROVIDING SAFETY GUIDELINES FOR PROTESTERS"|
         PCs$PC=="SHARING PROTEST SAFETY INFORMATION"|
         PCs$PC=="ENCOURAGING COVID SAFETY DURING PROTEST"|
         PCs$PC=="ENCOURAGING PROTESTS TO STAY SAFE"|
         PCs$PC=="ENCOURAGING SAFETY AT PROTEST"|
         PCs$PC=="SHARING SAFE PROTEST TIPS"|
         PCs$PC=="WARNING PROTESTERS TO STAY SAFE"|
         PCs$PC=="PROVIDING COVID TESTS FOR PROTESTERS"|
         PCs$PC=="WARNING PROTESTS OF DANGER"|
         PCs$PC=="WARNING PROTESTERS TO LEAVE A DANGEROUS AREA"|
         PCs$PC=="WARNING PROTESTERS TO AVOID AREA WITH COUNTER PROTESTERS"|
         PCs$PC=="WARNING PROTESTERS TO AVOID A PROTEST NOT ORGANIZED BY AN OFFICIAL BLM"|
         PCs$PC=="WARNING PROTESTERS THAT POLICE VIOLENCE IS LIKELY"|
         PCs$PC=="WARNING PROTESTERS"|
         PCs$PC=="ENCOURAGING PROTESTERS SAFETY"|
         PCs$PC=="WARNING PROTESTERS OF  DANGER"|
         PCs$PC=="WARNING PROTESTERS THAT AN EVENT IS NOT AFFILIATED WITH THEM BUT THEY SUPPORT THE ORGANIZER"|
         PCs$PC=="WARNING PROTESTERS THAT POLICE VIOLENCE IS LIKELY"|
         PCs$PC=="WARNING OF POSSIBLE RIGHT WING COUTNER PROTESTERS"|
         PCs$PC=="PROMOTING A PEACEFUL RALLY"|
         PCs$PC=="WARNING OF UNDOCUMENTED PEOPLE OF ICE PRESENCE"|
         PCs$PC=="WARNING PEOPLE TO AVOID NONENCRYPTED TECH FOR ORGANIZING"|
         PCs$PC=="WARNING PROTESTERS OF DANGER"|
         PCs$PC=="ENCOURAGING COMMUNITY SAFETY"|
         PCs$PC=="WARNING AGAINST REVEALING PROTESTOR IDENTITIES"|
         PCs$PC=="STATING THAT PROTEST WILL BE PEACEFUL"|
         PCs$PC=="SHARING VIDEO OF PEACEFUL PROTESTERS"|
         PCs$PC=="ENCOURAGING COVID PERCAUTIONS"|
         PCs$PC=="SHARING COVID PRECAUTIONS"|
         PCs$PC=="REQUESTING COVID PROTOCOLS"|
         PCs$PC =="ENCOURAGING SAFE PROTEST"|
         PCs$PC=="ENCOURAGING COVID PERCAUTIONS"|
         PCs$PC=="REPORTING ON DANGEROUS CONDITIONS FOR PROTESTERS" |
         PCs$PC=="WISHING SAFETY FOR PROTESTERS"]<- "ENCOURAGING PROTEST SAFETY" 

####  20 REQUESTING PARTICIPATION ####
PCs$PC_2[PCs$PC=="ASKING FOR PARTICIPATION"|
         PCs$PC=="ENCOURAGING PROTEST PARTICIPATION"|
         PCs$PC=="ENCOURAGING PEOPLE TO COME TO PROTEST"|
         PCs$PC=="ENCOURAGING PEOPLE TO GO OUT AND PROTEST"|
         PCs$PC=="CALLING FOR ACTION, USING SYMBOLS"|
         PCs$PC=="ADVISING ON JOINING REVOLUTIONARY ORGANIZATIONS"|
         PCs$PC=="APPRECIATING PARTICIPATION"|
         PCs$PC=="DEMANDING BLACK PEOPLE DO RADICAL THINGS TO DISMANTLE RACISM"|
         PCs$PC=="EDUCATING ON WAYS TO GET INVOLVED IN THE MOVEMENT"|
         PCs$PC=="ENCOURAGING ADVOCACY"|
         PCs$PC=="PROMOTING MOVEMENT AND CAMPAIGN"|
         PCs$PC=="QUOTING ANGELA DAVIS"|
         PCs$PC=="QUOTING ANGELA DAVIS ON ABOLITION"|
         PCs$PC=="QUOTING HUEY NEWTON"|
         PCs$PC=="QUOTING PUBLIC COMMENT"|
         PCs$PC=="QUOTING STATEMENT"|
         PCs$PC=="SHARING MEME OF ANTIPOLICE PROTEST SLOGANS"|
         PCs$PC=="RECRUITING VOLUNTEERS TO HELP WITH PROTEST EFFORT"|
         PCs$PC=="SHARING ENTHOUSIASTIC PROTESTER MEDIC"|
         PCs$PC=="SHARING MLK QUOTE"|
         PCs$PC=="EXPRESSING PERSISTENCE"|
         PCs$PC=="DEMANDING CONTINUED ACTION"|
         PCs$PC=="DISPELLING PROTEST MYTHS"|
         PCs$PC=="EXPRESSING INTENT TO PROTEST"|
         PCs$PC=="IDENTIFYING ONGOING NEED FOR ACTIVISM"|
         PCs$PC=="RALLYING PEOPLE"|
         PCs$PC=="COMMITTING TO MORE PROTEST"|
         PCs$PC=="DEMANDING ONGOING ACTIVISM"|
         PCs$PC=="DRAWING ATTENTION TO MINNEAPOLIS PROTESTS"|
         PCs$PC=="EXPRESSING DETERMINATION"|
         PCs$PC=="INSPIRING QUOTE"|
         PCs$PC=="PROMISING TO FIGHT FOR JUSTICE"|
         PCs$PC=="SHARING A QUOTE"|
         PCs$PC=="SHARING INSPIRING QUOTE"|
         PCs$PC=="SHARING LARGE DONATION TO BLACKOUT CAMPAIGN"|
         PCs$PC=="REPORTING ON POSITIVE EXPERIENCE OF PROTESTERS"|
         PCs$PC=="INSPIRING ACTION"|
         PCs$PC=="SUPPORTING THE MOVEMENT"|
         PCs$PC=="SHARING PROTEST SLOGAN"|
         PCs$PC=="PRAISING COLLECTIVE EFFORT"|
         
         PCs$PC=="SHARING QUOTE BY THE DAUGHTER OF GEORGE FLOYD"|
         PCs$PC=="SHARING REPORTERS ENTHOUSIASM FOR THE NAMING OF BLM PLAZA"|
         PCs$PC=="SHARING SONG YOU GON LOSE YOUR JOB"|
         PCs$PC=="SHARING SUPPORTIVE PROTESTERS"|
         PCs$PC=="SHARING WAYS TO PARTICIPATE"|
         PCs$PC=="SHOWING WAYS TO SUPPORT THE MOVEMENT WITHOUT PROTESTING"|
         PCs$PC=="STATING CONDITOINALLY THAT CHILDREN ARE WELCOME AT PROTEST"|
         PCs$PC=="TWEETING ABOUT THE IMPORTANCE OF PROTEST"|
         PCs$PC=="USING LYRICS AS SLOGAN"|
         PCs$PC=="SHARING DA CANDIDATE FORUM"|
         PCs$PC=="ENCOURAGING GREATER PERSISTANCE AMONG PROTESTERS"|
         PCs$PC=="ENCOURAGING MOVEMENT SUPPORTERS"|
         PCs$PC=="ENCOURAGING POLICE TO JOIN PROTEST"|
         PCs$PC=="ENCOURAGING PROTESTORS TO STAY TUNED"|
         PCs$PC=="EXPRESSING EXCITEMENT AT HOW MANY PROTESTS THERE ARE"|
         PCs$PC=="EXPRESSING GRATITUTDE FOR THE ACTIVISM OF THEIR ORGANIZERS"|
         PCs$PC=="EXPRESSING NEED FOR FUTURE MOBILIZATION"|
         PCs$PC=="EXPRESSING NEED FOR PERSISTENCE"|
         PCs$PC=="EXPRESSING SOLIDAIRTY"|
         PCs$PC=="EXPRESSING SUPPORT"|
         PCs$PC=="EXPRESSING SUPPORT FOR THE MOVEMENT"|
         PCs$PC=="GATHERING PICTURES OF PROTEST FROM PARTICIPANTS"|
         PCs$PC=="INSPIRING PROTESTERS"|
         PCs$PC=="LOOKING FOR VOLUNTEERS TO TRANSCRIBE TALK"|
         PCs$PC=="CALLING FOR PROTEST PARTICIPATION"|
         PCs$PC=="SHARING A FAMOUS QUOTE"|
         PCs$PC=="INVITING ALLIES TO PROTEST"|
         PCs$PC=="ENCOURAGING PARTICIPATION"|
         PCs$PC=="SHARING WAYS TO HELP LOCALLY"|
         PCs$PC=="SHARING HOW TO SUPPORT CHAPTER"|
         PCs$PC=="SHARING INFORMATION ABOUT HOW TO PARTICIPATE IN PROTEST"|
         PCs$PC=="EXPLAINING HOW PEOPLE CAN PARTICIPATE"|
         PCs$PC=="ASKING FOR SUPPORT"|
         PCs$PC=="ASKING FOR AUDIENCE OPINION"|
         PCs$PC=="ASKING FOR COLLABORATION"|	
         PCs$PC=="ASKING FOR PROTEST SUPPORT "|
         PCs$PC=="DEMANDING REPARATIONS ASKING FOR PARTICIPATION"|
         PCs$PC=="ASKING FOR PARTICIPAITON"|
         PCs$PC=="ASKING FOR MORE SUPPORT "|
         PCs$PC=="ASKING FOR COMMUNITY VOLUNTEERS"|
         PCs$PC=="ASKING FOR COMMUNITY SUPPORT"|
         PCs$PC=="ASKING FOR COLLABORATION "|
         PCs$PC=="ASKING FOR ALLY SUPPORT"|
         PCs$PC=="ASKING FOR SUPPORT"|
         PCs$PC=="SHARING WAYS TO MOBILIZE"|
         PCs$PC=="ASKING PEOPLE TO TAKE ACTION "| 
         PCs$PC=="ASKING FOR COMMUNITY SUPPORT"|
         PCs$PC=="ASKING FOR COMMUNITY VOLUNTEERS"|
         PCs$PC=="ASKING FOR MORE SUPPORT"|
         PCs$PC=="ASKING FOR PARTICIPAITON"|
         PCs$PC=="ASKING FOR PETITION SIGNATURES"|
         PCs$PC=="ASKING FOR PROTEST SUPPORT"|
         PCs$PC=="ASKING FOR PROTEST UPDATES"|
         PCs$PC=="ASKING PEOPLE TO TAKE ACTION"|
         PCs$PC=="ASKING USERS TO SUPPORT PROTESTORS"|
         PCs$PC=="POLITICIAN ENCOURAGING PROTEST"|
         PCs$PC=="REQUESTING PARTICIPATION"|
         PCs$PC=="SHARING IMAGES OF PEACEFUL PROTESTERS"|
         PCs$PC=="INVITING PEOPLE TO PROTEST"|
         PCs$PC=="PRAISING PROTESTERS"|
         PCs$PC=="SUPPORTING PROTESTERS"|
         PCs$PC=="SUPPORTING BLM PROTESTORS"|
         PCs$PC=="AFFIRMING THE RIGHT TO PEACEFUL PROTEST"|
         PCs$PC=="ENCOURAGING MOBILIZATION"|
         PCs$PC=="ENCOURAGING PROTEST"|
         PCs$PC=="REQUESTING SUPPORT FOR  CAMPAIGN"|
         PCs$PC=="ENCOURAGING PEACEFUL PROTEST"|
         PCs$PC=="ENCOURAGING PROTESTER SOLIDARITY"|
         PCs$PC=="ENCOURAGING TRADITIONAL ACTIVISM"|
         PCs$PC=="ENCOURAGING A BOYCOTT"|
         PCs$PC=="ENCOURAGING ALLIES TO PARTICIPATE  IN THE REVOLUTION"|
         PCs$PC=="ENCOURAGING ALLYSHIP"|
         PCs$PC=="ENCOURAGING CONTINUED PROTEST"|
         PCs$PC=="ENCOURAGING INVOLVEMENT"|
         PCs$PC=="ENCOURAGING PEACEFUL PROTESTING"|
         PCs$PC=="ENCOURAGING PEOPLE TO PARTICIPATE IN PROTEST"|
         PCs$PC=="ENCOURAGING SOLIDARITY"|
         PCs$PC=="DISCUSSING STRATEGY FOR ABOLISHING POLICE"|
         PCs$PC=="ENCOURAGING CONTINUED SUPPORT"|
         PCs$PC=="ENCOURAGING FOR COMMUNITY SUPPORT"|
         PCs$PC=="ENCOURAGING PROTESTERS WITH PICTURE OF BURNED OUT CARS"|
         PCs$PC=="ENCOURAGING RESILIENCE"|
         PCs$PC=="ENCOURAGING UNITY"|
         PCs$PC=="SHARING A FAMOUS QUOTE"|
         PCs$PC=="THANKING SUPPORTERS"|
         PCs$PC=="SHARING MOBILIZING QUOTE"|
         PCs$PC=="ENCOURAGING PROTESTERS"|
         PCs$PC=="PROMOTING PHONE BANKING"|
         PCs$PC=="SHARING MLK QUOTE TO ENCOURAGE AND VALIDATE PROTEST"|
         PCs$PC=="ENCOURAGING PARTICIPATION IN PROTEST"]<- "ENCOURAGING PARTICIPATION" 


####  21 DEMANDING BAN ON NO KNOCK WARRENTS ####  
PCs$PC_2[PCs$PC=="DEMANDING END OF NO KNOCK WARRENTS"|
         PCs$PC=="DEMANDING ELIMINATION OF NO KNOCK WARRENTS"|
         PCs$PC=="DISCUSSING BANNING NO KNOCK WARRANTS"|
         PCs$PC=="DEMANDING END OF NO KNOCK WARRANTS"|
         PCs$PC=="EXPRESSING INTENT TO ACT, ASKING FOR PARTICIPATION"|
         PCs$PC=="DEMANDING END NO KNOCK WARRENTS"| 
         PCs$PC=="DEMANDING AN END TO NO-KNOCK WARRANTS"|
         PCs$PC=="SHARING POLLING DATA THAT SHOWS LACK OF SUPPORT FOR NO KNOCK WARRENTS"]<- "DEMANDING BAN ON NO KNOCK WARRENTS" 

####  22 ADVISING PROTESTERS ####
PCs$PC_2[PCs$PC=="CATEGORIZING PROTEST TIPS"|
         PCs$PC=="OFFERING PROTEST AFTERCARE TIPS"|
         PCs$PC=="OFFERING PROTEST DRESSING TIPS"|
         PCs$PC=="OFFERING PROTEST ETIQUETTE TIPS"|
         PCs$PC=="OFFERING PROTEST PREPARATION TIPS"|
         PCs$PC=="SHARING NONVIOLENT TIPS TO DEAL WITH POLICE RIOT CONTROL TACTICS"|
         PCs$PC=="SHARING PROTEST TIPS"|
         PCs$PC=="ADVISING ALLIES"|
         PCs$PC=="DISCOURAGING PROTEST UPDATES"|
         PCs$PC=="EXPRESSING UNCERTAINTY ABOUT PROTEST TRAJECTORY"|
         PCs$PC=="EXPRESSING UNCERTAINTY ABOUT UNORGANIZED PROTESTS"| # CRITICZING PROTESTERS
         PCs$PC=="INSTRUCTING NOT TO PROTEST"|
         PCs$PC=="ASKING PROTESTORS TO REMAIN PEACEFUL"|
         PCs$PC=="DIRECTING PEOPLE TOWARD SPCIFIC ACTIONS"|
         PCs$PC=="DIRECTING USERS TO DIFFERENT ACCOUNT"|
         PCs$PC=="DISASSOCIATING FROM AN EVENT"|
         PCs$PC=="WARNING THAT PUSHING FOR CHANGE MUST BE INTENTIONAL AND NOT PASSIVE"|
         PCs$PC=="ASKING TO CEASE DONATIONS"|
         PCs$PC=="ENCOURAGING PHOTOGRAPHS OF COUNTER PROTESTERS WHO ARE VIOLENT"|
         PCs$PC=="ENCOURAGING REPROTS OF PEOPLE WHO ARE UNDERMINING PROTESTS"|
         PCs$PC=="ENCOURAGING THE REPORT OF POLICE ACIVITY"|
         PCs$PC=="ASKING PEOPLE TO NOT TAG THIS ACCOUNT WITH MAYOR OR POLICE"|
         PCs$PC=="ENCOURAGING PROTEST DOCUMENTATION" |
         PCs$PC=="SHARING TIPS FOR PROTESTERS"|
         PCs$PC=="OFFERING PROTEST INTRUCTIONS"|
         PCs$PC=="ENCOURAGING CARPOOLS FOR PROTESTERS"|
         PCs$PC=="GIVING PROTEST MEETING INSTRUCTIONS"|
         PCs$PC=="HELPING PEOPLE FIND PROTESTS"|
         PCs$PC=="SHARING PARKING INFORMATION FOR PROTEST"|
         PCs$PC=="SHARING PROTEST TIPS"| 
         PCs$PC=="SHARING POLICE TACTICS"|
         PCs$PC=="CALLING ON PROTESTERS TO REST AND PREPARE FOR MORE ACTIVITY"| 
         PCs$PC=="ASKING FOR SILENCE DURING PROTEST"| 
         PCs$PC=="ENCOURAGING CARPOOLING TO PROTEST"|
         PCs$PC=="PROVIDING INFORMATION ON PROTEST ETIQUETTE"|
         PCs$PC=="OFFERING PROTEST INSTRUCTIONS"|
         PCs$PC=="OFFERING PROTEST INSTRUCTION"|
         PCs$PC=="ENCOURAGING PEOPLE TO FOLLOW THIS ACCOUNT FOR LOCAL PROTEST INFORMATION"]<- "ADVISING PROTESTERS" 

####  23 SHARING UPDATES FROM OTHER CITIES ####
PCs$PC_2[PCs$PC=="SHARING PROTEST ACTIVITES IN OTHER CITY"|
         PCs$PC=="REPORTING ON ACTION OF PROTESTER"|
         PCs$PC=="REPORTING ON PEACEFUL PROTEST EVENTS"| 
         PCs$PC=="SHARING PROTEST IN OTHER CITY"|
         PCs$PC=="SHARING PROTEST SUPRESSION IN OTHER CITY"|
         PCs$PC=="SHARING PROTEST IN AMSTERDAM"|
         PCs$PC=="REPORTING ON PROTEST EVENTS IN ANOTHER CITY"|
         PCs$PC=="REPORTING ON PROTESTS IN ANOTHER CITY"|
         PCs$PC=="SHARING PROTEST ACTIVITES IN ANOTHER CITY"|
         PCs$PC=="SHARING PROTEST EVENTS IN ANOTHER CITY"|
         PCs$PC=="SHARING PROTEST EVENTS IN OTHER CITY"|
         PCs$PC=="SHARING PROTEST UPDATES IN ANOTHER CITY"|
         PCs$PC=="SHARING PROTESTS IN AMSTERDAM"|
         PCs$PC=="SHARING PROTESTS IN LONDON"|
         PCs$PC=="SHARING PROTESTS IN PARIS"|
         PCs$PC=="SHARING PROTEST ACTIVITY IN ANOTHER CITY"|
         PCs$PC=="SHARING PROTEST ACTIVITIES IN ANOTHER CITY"|
         PCs$PC=="INTERNATIONAL PARTICIPATION"|
         PCs$PC=="REPORTING ON VIOLENT PROTEST EVENTS IN ANOTHER CITY"|
         PCs$PC=="SHARING AUSTRAILIAN PROTESTS"|
         PCs$PC=="SHARING CHARLOTTE AREA PROTEST"|
         PCs$PC=="SHARING FRENCH PROTEST FOR A FRENCH VICTIM OF POLICE VIOLENCE"|
         PCs$PC=="SHARING INTERNATIONAL PROTESTS"|
         PCs$PC=="SHARING LOCATION AND TIME OF DEMONSTRATION"|
         PCs$PC=="SHARING PROTESTER SUCCESS IN MINNEAPOLIS"|
         PCs$PC=="SHARING PROTESTER SUCCESSES IN OTHER CITIES"|
         PCs$PC=="CITING POLICY IN OTHER CITIES"|
         PCs$PC=="SHARING PROTEST ACTIVITIES IN ANOTHER COUNTRY"|
         PCs$PC=="SHARING PROTEST EVENTS IN OTHER COUNTRY"|
         PCs$PC=="SHARING PROTEST UPDATES FROM ANOTHER COUNTRY"|
         PCs$PC=="SHARING PROTESTERS IN ANOTHER COUNTRY CHALLENGING LEADERS"|
         PCs$PC=="TWEETING PROTEST ACTIVITY IN ANOTHER CITY"]<- "SHARING UPDATES FROM OTHER CITIES" 


####  24 CALLING ON OFFICIALS ####  
PCs$PC_2[PCs$PC=="CALLING ON AN OFFICIAL"|
         PCs$PC=="TWEETING AT POLITICIAN"|
         PCs$PC=="ADDRESSING A REPRESENTATIVE"| 
         PCs$PC=="ANNOUNCING POLICY CHANGE"|
         PCs$PC=="CALLING REPRESENTATIVE"|
         PCs$PC=="CALLING ON OFFICIALS"|
         PCs$PC=="CALLING ON AN ELECTED OFFICIAL TO ACT"|
         PCs$PC=="CALLING REPRESENTATIVES"|
         PCs$PC=="CALLING ON ELECTED OFFICIALS"| 
         PCs$PC=="TAGGING ELECTED OFFICIALS"|
         PCs$PC=="DEMANDING ACTION NOT PROMISES"|
         PCs$PC=="ENCOURAGING ACTIONS BY POLITICIANS NOT JUST WORDS"| ## ACTIONS NOT WORDS
         PCs$PC=="ENCOURAGING COUNCIL MEMBERS TO SUPPORT BIPOC EXPLICITLY"|
         PCs$PC=="ENCOURAGING LOCAL OFFICIALS TO INVEST IN VIOLENCE PREVENTION WITH AUTHORITIES"]<- "CALLING ON OFFICIALS" 

####  25 DEMANDING A POLICY ####
#PCs$PC_2[]<- "DEMANDING A POLICY" 

####  26 THANKING SUPPORTERS ####
PCs$PC_2[PCs$PC=="THANKING A SUPPORTER OF THE MOVEMENT"|
         PCs$PC=="THANKING ACTIVIST FOR SUPPORTING ORGANIZATION"|
         PCs$PC=="THANKING AND SUPPORTING PROTESTERS"|
         PCs$PC=="THANKING CELEBRITIES FOR SHOWING THEIR SUPPORT"|
         PCs$PC=="THANKING CHAPTER ORGANIZER"|
         PCs$PC=="THANKING DONORS"|
         PCs$PC=="THANKING FOLLOWER"|
         PCs$PC=="THANKING FOLLOWERS"|
         PCs$PC=="THANKING FOLLOWERS FOR JOINING LIVE STREAM"|
         PCs$PC=="THANKING FOLLOWERS FOR PROTEST UPDATE"|
         PCs$PC=="THANKING FOLLOWERS FOR SUPPORT"|
         PCs$PC=="THANKING FOR SUPPORT"|
         PCs$PC=="THANKING MEDIA SOURCE FOR GOOD COVERAGE"|
         PCs$PC=="THANKING MOVEMENT SUPPORTERS"|
         PCs$PC=="THANKING ORGANIZATIONAL PARTNERS"|
         PCs$PC=="THANKING PEOPLE FOR COMING OUT TO COVID TESTING"|
         PCs$PC=="THANKING PEOPLE FOR SOLIDARITY"|
         PCs$PC=="THANKING POLITICIAN FOR SUPPORT"|
         PCs$PC=="THANKING PROTESTERS FOR PARTICIPATION"|
         PCs$PC=="THANKING PROTESTORS"|
         PCs$PC=="THANKING SOMEONE FOR PROTEST UPDATED"|
         PCs$PC=="THANKING SUPPORTERS"|
         PCs$PC=="THANKING SUPPORTIVE POLITICIANS"|
         PCs$PC=="THANKING TEEN GIRL ORGANIZERS"|
         PCs$PC=="THANKING THOSE WHO SUPPORTED PROTESTERS WITH WATER AND PPE"|
         PCs$PC=="SPOTLIGHTING INDIVIDUAL ACTIVISTS"| # PRAISING BELOW
         PCs$PC=="PRAISING ACTIVIST LEADERSHIP"|
         PCs$PC=="SUPPORTING BLACK YOUTH ORGANIZERS"|
         PCs$PC=="EXPRESSING APPRECIATION FOR AN AWARD"|
         PCs$PC=="PRAISING COMMUNITY MOBILIZATION"|
         PCs$PC=="PRAISING PROTEST TURNOUT"|
         PCs$PC=="GIVING THANKS"|
         PCs$PC=="HIGHLIGHTING A PERON"|
         PCs$PC=="HIGHLIGHTING AN ACTIVIST"|
         PCs$PC=="HONORING A PERSON"|
         PCs$PC=="HONORING ORGANIZIERS AND EDUCATORS IN THE COMMUNITY"|
         PCs$PC=="SUPPORTING ACTIVISTS"|
         PCs$PC=="SUPPORTING LIKE-MINDED ORGANIZATIONS"|
         PCs$PC=="SUPPORTING NONVIOLENT ACTIVISM"|
         PCs$PC=="SUPPORTING PEACEFUL PROTEST"|
         PCs$PC=="SUPPORTING POLITICAL MOVEMENT"|
         PCs$PC=="SUPPORTING YOUNG ACTIVISTS"|
         PCs$PC=="SUPPORTING YOUTH PROTESTERS"|
         PCs$PC=="VALORIZING PROTESTERS"|
         PCs$PC=="SHARING VIDEO OF ILHAN OMAR SPEACH AT PROTEST"|
         PCs$PC=="EXPRESSING SOLIDARITY"]<- "THANKING SUPPORTERS" # THANKING, PRAISE, SUPPORT, SOLIDARITY


####  27 DEMANDING  ACCOUNTABILITY ####
PCs$PC_2[PCs$PC=="DEMANDING JUSTICE AND ACCOUNTABILITY"| 
         PCs$PC=="DENOUNCING LACK OF ACCOUNTABILITY"| 
         PCs$PC=="EXPRESSING CONCERNS ABOUT POLICE CONTROL ON POLICY"| 
         PCs$PC=="EXPRESSING CONCERNS ABOUT POLICE DATA AND STORAGE"| 
         PCs$PC=="CALLING FOR THE FIRING OF LOCAL OFFICERS"| 
         PCs$PC=="DEMANDING ACCOUTABILITY FROM POLICE DEPARTMENTS"| 
         PCs$PC=="DEMANDING ARREST OF BREONNA TAYLOR'S KILLER'S"| 
         PCs$PC=="DEMANDING ARREST OF BREONNA TAYLOR'S KILLERS"| 
         PCs$PC=="DEMANDING ARREST OF OFFICERS WHO KILLED TAYLOR AND MCATEE"| 
         PCs$PC=="DEMANDING ARREST OF OFFICERS WHO KILLED TAYOLOR"| 
         PCs$PC=="DEMANDING ARREST OF POLICE OFFICERS WHO KILLED BREONNA TAYLOR"| 
         PCs$PC=="DEMANDING CONVICTION AND ARRESTS OF POLICE OFFICERS"| 
         PCs$PC=="DEMANDING END TO IMPUNITY"| 
         PCs$PC=="DEMANDING FIRING OFFICERS WHO KILLED TAYLOR"| 
         PCs$PC=="DEMANDING GREATER POLICE TRANSPARNCENY"| 
         PCs$PC=="DEMANDING INVESTIGATION INTO DEATHS"| 
         PCs$PC=="DEMANDING JUSTICE AND ACTION FOR BREONNA TAYLOR"| 
         PCs$PC=="DEMANDING JUSTICE FOR"| 
         PCs$PC=="DEMANDING JUSTICE FOR POLICE BRUTALITY VICTIMS"| 
         PCs$PC=="DEMANDING JUSTICE FOR PRISCILLA SLATER"| 
         PCs$PC=="DEMANDING JUSTICE FOR VICITM"| 
         PCs$PC=="DEMANDING JUSTICE FROM POLICING SYSTEM"| 
         PCs$PC=="DEMANDING THE ARREST OF ALL OFFICERS INVOLVED IN TAYLORS KILLING"| 
         PCs$PC=="DEMANDING THE ARREST OF TAYLORS KILLERS"| 
         PCs$PC=="DEMANDING THE FIRING OF 3 OFFICERS INVOLVED IN THE KILLING OF TAYLOR"| 
         PCs$PC=="DEMANDING THE FIRING OF OFFICERS INVOLVED IN TAYLORS KILLING"| 
         PCs$PC=="DEMANDING THE FIRING OF OFFICERS WHO KILLED TAYLOR"| 
         PCs$PC=="DENOUNCING LACK OF ACCOUNTABILITY IN CORRUPT LAW ENFORCEMENT"|
         PCs$PC=="DEMANDING ACCOUNTABILITY FROM POLICE"|
         PCs$PC=="IDENTIFYING LACK OF ACCOUNTABILITY"| 
         PCs$PC=="PETITIONING ACCOUNTABILITY FOR THE KILLING OF A 14 YEAR OLD BY POLICE IN BRAZIL"|
         PCs$PC=="DEMANDING ACCOUNTABILITY"| 
         PCs$PC=="ENCOURAGING ACCOUNTABILITY"| 
         PCs$PC=="DEMANDING STATE ATTORNEY GENERAL TAKE OVER INVESTIGATION"| 
         PCs$PC=="DEMANDING THE FIRING OF THE OFFICERS WHO KILLED TAYLOR"| 
         PCs$PC=="DEMANDING THE ARREST OF THE OFFICERS WHO KILLED TAYLOR"| 
         PCs$PC=="DEMANDING ARREST OF OFFICER WHO KILLED ARRESTEE"| 
         PCs$PC=="DEMANDING ARREST OF OFFICERS WHO KILLED TAYLOR"| 
         PCs$PC=="DEMANDING ARREST OF TAYLORS KILLERS"| 
         PCs$PC=="DEMANDING ARRESTS"| 
         PCs$PC=="DEMANDING JUSTICE AND ARRESTS"| 
         PCs$PC=="DEMANDING JUSTICE FOR BREONNA TAYLOR"| 
         PCs$PC=="DEMANDING THE ARREST OF OFFICERS WHO KILLED TAYLOR"| 
         PCs$PC=="DEMANDING ANSWERS"| 
         PCs$PC=="DEMANDING ATTENTION"| 
         PCs$PC=="DEMANDING LEGAL ACTION"| 
         PCs$PC=="EXPOSING POLICE BEHAVIOR"| 
         PCs$PC=="POINTING OUT LACK OF ACCOUTABILITY"| 
         PCs$PC=="SHARING THREAD ON WHY THE POLICE WHO KILLED BREONNA TAYLOR COULD HAVE AND SHOULD HAVE BEEN FIRED"| 
         PCs$PC=="STATING MISTRUST IN POLICE AND PUBLIC PROSECUTOR"| 
         PCs$PC=="TWEETING ABOUT ACCOUNTABILTY TO INDIVIDUAL PRECINTS"| 
         PCs$PC=="REPORTING LACK OF DISCIPLINE FOR OFFICERS INVOLVED"| 
         PCs$PC=="CALLING FOR PUNISHMENT FOR OFFICER"| 
         PCs$PC=="DEMANDING POLICE ACCOUNTABILITY"| 	
         PCs$PC=="CALLING FOR JUSTICE FOR VICTIMS OF POLICE VIOLENCE"|
         PCs$PC=="DEMANDING ELECTORAL ACCOUNTABILITY"| 
         PCs$PC=="DEMANDING GREATER PUBLIC ACCOUNTABILITY"| 
         PCs$PC=="PROSCRIBING POLICE ACCOUNTABILITY"] <- "DEMANDING ACCOUNTABILITY"

####  28 DEMANDING REPARATIONS ####
PCs$PC_2[PCs$PC=="REPORTING ON CALLS FOR REPARATIONS"| 
         PCs$PC=="CALLING FOR REPARATIONS"|
         PCs$PC=="ENDORSING REPARATIONS"|
         PCs$PC=="DEMANDING REPARATIONS FROM A CORPORATION"] <- "DEMANDING REPARATIONS"

####  29 CRITIQUING REFORMIST OR TONE DEAF ACTIVISM ####
PCs$PC_2[PCs$PC=="CRITIQUING 8CANTWAIT" |
         PCs$PC=="CRITICIZING REFORM APPROACHES" |
         PCs$PC=="CRITICIZING REFORMIST APPROACHES" |
         PCs$PC=="CRITICIZING THE CALL FOR ACTIVISTS TO WAIT FOR MORE PUBLIC OPINION INFORMATION" |
         PCs$PC=="CRITICIZING THE CLAIM THAT TPD HAS IMPLIMENTED 8CANTWAIT PROTOCOLS" |
         PCs$PC=="CRITICIZING THE NON PROFIT INDUSTRY" |
         PCs$PC=="CRITICIZING THE NON-PROFIT INDUSTRY" |
         PCs$PC=="CELEBRATING REFORM STYLE PROPOSALS SARCASM" |
         PCs$PC=="CLARIFYING THAT THEY DO NOT ENDORSE REFORM" |
         PCs$PC=="CRITICIZING BLACK OUT TUESDAY" |
         PCs$PC=="DISPARAGING 8CANTWAIT" |
         PCs$PC=="EXPRESSING FRUSTERATION AT PROTESTERS DANCING WITH POLICE AT PROTEST" |
         PCs$PC=="PRAISING JULIAN CASTRO FOR SHARING THE 8CANTWAIT REFORMS SARCASM" |
         PCs$PC=="POINTING OUT FLAWS IN REFORMIST THINKING" |
         PCs$PC=="CRITICIZING A FUNDING POLICY" |
         PCs$PC=="CRITICIZING A FUNDING POLICY" |
         PCs$PC=="REFUSING TO COMPLY WITH THE ENEMY" |
         PCs$PC=="REJECTING INCRIMENTALISM" |
         PCs$PC=="SHARING A DECEPTIVE POLICE REFORM POLICY" |
         PCs$PC=="SHARING POLICE QUOTE THAT DEPARTMENT FOLLOWS 8CANTWAIT PROTOCOLS" |
         PCs$PC=="STATING OPPOSITION TO INCRIMENTALISM" |
         PCs$PC=="CRITIQUING THE BLACKOUT CAMPAIGN" |
         PCs$PC=="CRITIQUING AN INSTITUTIONALIST PERSPECTIVE" |
         PCs$PC=="POINTING TO THE ABSURDITY OF DEI WORK IN POLICE DEPARTMENTS"|
         PCs$PC=="CRITICIZING THE BLACKOUT CAMPAIGN FOR OBSTRUCTING PROTESTER NEWS AND INFORMATION SHARING"] <- "CRITIQUING REFORMIST OR TONE DEAF ACTIVISM"



####  30 PRESENTING THE CASE FOR REPARATIONS ####
PCs$PC_2[PCs$PC=="SHARING REPARATIONS INFORMATION"| 
         PCs$PC=="EDUCATING ON REPARATIONS"|
         PCs$PC=="EDUCATING ON REPARATIONS AND DONATIONS"|
         PCs$PC=="EQUATING REPARATIONS TO EXISTING POLICIES"|
         PCs$PC=="INFORMING REPARATIONS  AND HR BILL"|
         PCs$PC=="LEGITIMIZING REPARATIONS AS A POLICY"|
         PCs$PC=="LEGITIMIZING REPARATIONS AS POLICY"|
         PCs$PC=="LINKING REPARATIONS TO OTHER POLICIES"|
         PCs$PC=="REFRAMING REPARATIONS AS JUSTICE"|
         PCs$PC=="SHARING CASE FOR REPARATIONS AND HR 40"|
         PCs$PC=="SHARING EDITORIAL ON REPARATIONS"|
         PCs$PC=="ASSOCIATING REPARATIONS AS A SOLUTION"|
         PCs$PC=="SHARING MEDIA COVERAGE OF REPARATIONS MOVEMENT"|
         PCs$PC=="SHARING NEWS ABOUT EVANSTON REPARATIONS"|
         PCs$PC=="DEMANDING REPARATIONS FROM A CORPORATION"] <- "PRESENTING THE CASE FOR REPARATIONS"

####  31 DEMANDING POLICE REFORM ####
PCs$PC_2[PCs$PC=="DEMANDING POLICY CHANGE"|
           PCs$PC=="INFORMING PEOPLE ABOUT LOCAL LAWS AND BILLS UNDER REVIEW"|
           PCs$PC=="SEEKING POLICY CHANGE"|
           PCs$PC=="DEMANDIING LOCAL POLICY CHANGE"|
           PCs$PC=="SHARING COVERAGE OF BREONNAS LAW"|
           PCs$PC=="DEMANDING END OF NO KNOCK WARRENTS"|
           PCs$PC=="DEMANDING ELIMINATION OF NO KNOCK WARRENTS"|
           PCs$PC=="DISCUSSING BANNING NO KNOCK WARRANTS"|
           PCs$PC=="DEMANDING END OF NO KNOCK WARRANTS"|
           PCs$PC=="EXPRESSING INTENT TO ACT, ASKING FOR PARTICIPATION"|
           PCs$PC=="DEMANDING END NO KNOCK WARRENTS"| 
           PCs$PC=="DEMANDING AN END TO NO-KNOCK WARRANTS"|
           PCs$PC=="SHARING POLLING DATA THAT SHOWS LACK OF SUPPORT FOR NO KNOCK WARRENTS"| # DEMANDING BAN ON NO KNOCK WARRENTS; N ==22
           PCs$PC=="ADVOCATING FOR HR 40" |
         PCs$PC=="CALLING FOR FEEDBACK ON LOCAL POLICE BEHAVIOR"| 
         PCs$PC=="DENOUNCING LACK OF ACCOUNTABILITY"| 
         PCs$PC=="DENOUNCING LACK OF ACCOUNTABILITY IN CORRUPT LAW ENFORCEMENT"|
         PCs$PC=="IDENTIFYING LACK OF ACCOUNTABILITY"| 
         PCs$PC=="CALLING FOR LEGISLATIVE JUSTICE THROUGH BREONNA'S LAW"| 
         PCs$PC=="CALLING FOR NO COPS AT PRIDE"| 
         PCs$PC=="CALLING FOR POLICE REFORM"| 
         PCs$PC=="CHALLENGING LAPD TO NEGOTIATE WITH BLM DEMANDS"| 
         PCs$PC=="CRITICIZING QUALIFIED IMMUNITY"| 
         PCs$PC=="DEMANDING A VARIETY OF LOCAL PROGRESSIVE ACTIONS"| 
         PCs$PC=="DEMANDING BODYCAM RULES"| 
         PCs$PC=="DEMANDING CHANGE IN LAW ENFORCEMENT"| 
         PCs$PC=="DEMANDING MAKING RACIAL PROFILING ILLEGAL"| 
         PCs$PC=="DEMANDING PASSING OF BREONNAS LAW"| 
         PCs$PC=="CALLING ON SENATORS TO VOTE NO ON HB918"| 
         PCs$PC=="CALLING FOR STATE CONSTITUTIONAL CHANGES"| 
         PCs$PC=="PETITIONING ACCOUNTABILITY FOR THE KILLING OF A 14 YEAR OLD BY POLICE IN BRAZIL"|
         PCs$PC=="PROSCRIBING POLICE ACCOUNTABILITY"] <- "DEMANDING POLICE REFORM"


####  32 CONDEMNING LOCAL POLICE ####
PCs$PC_2[PCs$PC=="DEMANDING POLICY CHANGE"|
         PCs$PC=="CRITICIZING POLICE BEHAVIOR"| 
         PCs$PC=="CRITICIZING POLICE"|
         PCs$PC=="CONJECTURING THAT IT IS EASIER TO FIRE OFFICERS THAN IT MAY APPEAR"|
         PCs$PC=="CRITICIZING POLICE PROTEST RESPONSE"|
         PCs$PC=="EXPRESSING DISDAIN FOR POLICE"|
         PCs$PC=="LISTING POLICE BRUTALITY INCIDENTS"|
         PCs$PC=="REPORTING ON POLICE USE OF VIOLENT TACTICS"|
         PCs$PC=="ACCUSING POLICE OF DISHONESTY"|
         PCs$PC=="ENOUNCING POLICE FUNDING"|
         PCs$PC=="CRITICIZING MINNEAPOLIS POLICE"|
         PCs$PC=="LINKING POLICE AGGRESSION TO ANTI-BLACKNESS"| 
         PCs$PC=="DESCRIBING AN INSTANCE OF POLICE BRUTALITY"|
         PCs$PC=="ENCOURAGING DISTRUST OF POLICE"|
         PCs$PC=="LINKING POLICE AGGRESSION TO ANTI-BLACKNESS"|
         PCs$PC=="POSTING THAT POLICE ARE AIMING AT PROTESTERS HEADS"|
         PCs$PC=="SHARING VIDEO OF AGGRESSIVE POLICE BEHAVIOR"|
         PCs$PC=="ACCUSING POLICE OF LYING ABOUT PROTESTERS"|
         PCs$PC=="ARGUING THAT POLICE ARE A DEGENERATE RESOURCE"|
         PCs$PC=="CALLING ATTENTION TO POLICE VIOLENCE"|
         PCs$PC=="CALLING ATTENTION TO VIOLENCE AGAINST PROTESTERS FROM POLICE"|
         PCs$PC=="CALLING FOR AN END TO POLICE BRUTALITY"|
         PCs$PC=="CRITIQUING MYTHIFICATION OF POLICE"|
         PCs$PC=="DEMANDING A RESTRUCTURING OF POLICE OVERSIGHT"|
         PCs$PC=="DEMANDING CRIMINAL CHARGES FOR POLICE"|
         PCs$PC=="EMANDING END TO POLICE VIOLENCE"|
         PCs$PC=="DEMANDING POLICE REFORM"|
         PCs$PC=="DENOUNCING POLICE BRUTALITY"|
         PCs$PC=="DENOUNCING POLICE CHIEF"|
         PCs$PC=="CALLING ATTENTION TO ABUSES OF POWER IN POLICING"|
         PCs$PC=="CALLING ATTENTION TO LETHAL POLICE ENCOUNTERS"|
         PCs$PC=="CRITICIZING LAPD CHIEF"|
         PCs$PC=="IDENTIFYING POLICE AS A PROBLEM"|
         PCs$PC=="DENOUNCING POLICE DONATIONS"|
         PCs$PC=="DENOUNCING POLICE VIOLENCE"|
         PCs$PC=="DISCOURAGING CALLING POLICE ON POC"|
         PCs$PC=="DENOUNCING POLICE VIOLENCE AT PROTEST"|
         PCs$PC=="DESCRIBING INSTANCE OF POLICE VIOLENCE"|
         PCs$PC=="DISCOURAGING COOPERATION WITH POLICE"]<- "CONDEMNING LOCAL POLICE" 


####  33 CRITICIZING OFFICIALS ####
PCs$PC_2[PCs$PC=="CONDEMNING STATE OFFICIALS"|
         PCs$PC=="CONDEMNING LOCAL OFFICIALS"|
         PCs$PC=="CONDEMNING GOVERNMENT OFFICIALS"|
         PCs$PC=="COMPARING FAILURES OF ELECTED OFFICIALS"|
         PCs$PC=="ACCUSING ELECTED OFFICIALS OF DISHONESTY"|
         PCs$PC=="POINTING OUT OFFICIALS GOING BACK ON PROMISES"|
         PCs$PC=="REPORTING THAT MAYOR AND OTHER ELECTED OFFICIALS IGNORED PEACEFUL ACTIVISTS GATHERED AT CITY HALL"|
         PCs$PC=="THREATENING OFFICIALS"|
         PCs$PC=="CALLING ON TRUMP TO RESIGN"|
         PCs$PC=="CRITICIZING THE JUSTICE SYSTEM"|
         PCs$PC=="SHAMING A POLICITIAN"|
         PCs$PC=="DENONCING LOCAL GOVERNMENT"|
         PCs$PC=="DENOUNCING LOCAL GOVERNMENT"|
         PCs$PC=="CRITICIZING A POLICY"|
         PCs$PC=="CRITICIZING LOCAL REPRESENTATIVES"|
         PCs$PC=="DEMANDING ACTION FROM REPRESENTATIVES"|
         PCs$PC=="DENOUNCING LOCAL REPRESENTATIVES"|
         PCs$PC=="THREATENING OFFICIALS, DEMANDING DEFUNDING POLICE"|
         PCs$PC=="CRITICIZING ELECTED OFFICIAL"|
         PCs$PC=="CONDEMNING LOCAL OFFICIALS; SHARING PROTEST UPDATES"|
         PCs$PC=="CRITICIZING AN OFFICIAL"|
         PCs$PC=="SEVERIG TIES WITH UNIVERSITY"|
         PCs$PC=="CRITICIZING POLITICIANS"|
         PCs$PC=="CRITICIZING POLITICIANS FOR THEIR SILENCE"|
         PCs$PC=="DENOUNCING POLITICIANS"|
         PCs$PC=="CRITICIZING CITY COUNCIL"|
         PCs$PC=="CRITICIZING CLEVELAND FOR NOT ALLOWING PUBLIC COMMENT DURING CITY COUNCIL MEETINGS"|
         PCs$PC=="CRITICIZING LOCAL CITY COUNCIL MEMBER"|
         PCs$PC=="MOCKING CITY COUNCIL MEMBER"|
         PCs$PC=="REPORTING THAT CITY COUNCIL HAS IGNORED THEIR POLICY PROPOSALS"|
         PCs$PC=="CRITICIZING GOVERNOR"|
         PCs$PC=="CRITICIZING THE MAYOR FOR THEIR COMMITTMENT TO THE BLACK COMMUNITY"|
         PCs$PC=="CRITICIZING MAYOR FOR SAYING HE WILL DO SOMETHING"|
         PCs$PC=="BLAMING DEATH OF DAVID MCATEE ON MAYOR CROWD SUPPRESSION"|
         PCs$PC=="BLAMING MAYOR FOR VIOLENCE"|
         PCs$PC=="BLAMING MAYOR PREEMPTIVELY FOR VIOLENCE"|
         PCs$PC=="CALLING THE GOV AND MAYOR FAILURES"|
         PCs$PC=="CALLING THE GOV AND MAYOR FAILURES"|
         PCs$PC=="CONDEMNING THE MAYOR FOR CALLING NATIONAL GUARD ON PROTESTERS"|
         PCs$PC=="CRITICIZING MAYOR"|
         PCs$PC=="CRITICIZING MAYOR AND POLICE CHEIF FOR TAKING SYMBOLIC KNEE"|
         PCs$PC=="CRITICIZING MAYOR FREY"|
         PCs$PC=="CRITICIZING THE MAYOR"|
         PCs$PC=="DEMANDING MAYOR DO SOMETHING ABOUT THE OFFICERS WHO KILLED BREONNA TAYLOR"|
         PCs$PC=="ENCOURAGING DEMANDS ON THE MAYOR AND CITY COUNCIL TO DEFUND POLICE"|
         PCs$PC=="DIRECTING DEMANDS AT MAYOR"|
         PCs$PC=="ENCOURAGING DEMANDS ON THE MAYOR AND CITY COUNCIL TO INVEST IN COMMUNITY"|
         PCs$PC=="ENCOURAGING MAYOR TO FIRE COPS THAT KILLED TAYLOR AND MCATEE"|
         PCs$PC=="ENCOURAGING PEOPLE TO DEMAND THE MAYOR CUT THE POLICE BUDGET AND GIVE MORE MONEY TO COMMUNITIES"|
         PCs$PC=="PRESSURING MAYOR TO APOLOGIZE TO FAMILY OF VICTIM KILLED BY POLICE"|
         PCs$PC=="PRESSURING MAYOR TO CONDEM DA FOR CONDUCT DURING STEPHON CLARK INVESTIGATION"|
         PCs$PC=="PRESSURING THE MAYOR TO PRESSURE THE CITY MANAGER TO HAVE OFFICERS FIRED"|
         PCs$PC=="REPORTING THAT MAYOR APPROVE POLICE BUDGET INCREASES"|
         PCs$PC=="SAYING THE MAYOR IS NOT LISTENING TO PROTESTER DEMANDS"|
         PCs$PC=="SHARING THAT MAYOR HAS NOT DISCIPLINED OFFICERS WHO HAVE KILLED BLACK PEOPLE"|
         PCs$PC=="SHARING THAT THE MAYOR AUTHORIZED THE NATIONAL GUARD"|
         PCs$PC=="SHOWING THAT MAYOR DOES NOT CARE ABOUT BLACK RESIDENTS"|
         PCs$PC=="TWEETING AT MAYOR"|
         PCs$PC=="REPORTING THAT THEY DELIVERED POLICY TO CITY COUNCIL"|
         PCs$PC=="REPORTING THAT THEY ARE FOLLOWING INSTITUTIONAL METHODS AND STILL BEING IGNORED"|
         PCs$PC=="UNDERMINING TRUST IN THE MAYOR"|
         PCs$PC=="ASSOCIATING GOVERNOR WITH TRUMP"|
         PCs$PC=="CRITIQUING THE GOVERNOR"|
         PCs$PC=="ENCOURAGING PRESSURE ON GOVERNOR BECUASE OF LOCAL DEATH"|
         PCs$PC=="MOCKING GOVERNOR FOR HIS PRAISE OF POLICE"|
         PCs$PC=="UNDERMINING GOVERNORS PRAISE FOR POLICE WITH STORY OF POLICE VIOLENCE"|
         PCs$PC=="WARNING LOCAL OFFICIALS THAT THEY WILL BE SCRUTINIZED LIKE MAYOR FREY"]<- "CRITICIZING OFFICIALS" 

####  34 SHARING PUBLIC TALK ####
PCs$PC_2[PCs$PC=="SHARING PUBLIC TALK WITH ANGELA DAVIS"| 
         PCs$PC=="SHARING VIRTUAL TALK ON POLITICAL ACCOUNTABILITY"|
         PCs$PC=="SHARING A ROUNDTABLE OF BLACK WOMEN"|
         PCs$PC=="CALLING FOR HELP SHARING ANGELA DAVIS TALK"|
         PCs$PC=="EXPRESSING GRATITUDE FOR PUBLIC TALK WITH ANGELA DAVIS"|
         PCs$PC=="PROMOTING PUBLIC TALK WITH ANGELA DAVIS"|
         PCs$PC=="PROMOTING PUBLIC TALK"|
         PCs$PC=="ANNOUNCING A MEDIA APPEARANCE"|
         PCs$PC=="ANNOUNCING A PRESS BRIEFING"|
         PCs$PC=="ANNOUNCING A Q AND A SESSION"|
         PCs$PC=="PROMOTING A PUBLIC TALK"|
         PCs$PC=="PUBLIC TALK ABOUT REPARATIONS, COVID AND KILLINGS OF BLACK PEOPLE"|
         PCs$PC=="PROMOTING TALK WITH PUBLIC INTELECTUAL"| 
         PCs$PC=="SHARING TALK"|
         PCs$PC=="SHARING EDUCATIONAL EVENT"|
         PCs$PC=="SHARING EDUCATIONAL EVENT ABOUT TULSA"|
         PCs$PC=="PROMOTING LIVESTREAM"|
         PCs$PC=="PROMOTING INFORMATIONAL LIVESTREAM ON  REPRODUCTIVE JUSTICE"|
         PCs$PC=="PROMOTING INFORMATIONAL PANEL ON REPRODUCTIVE JUSTICE"|
         PCs$PC=="PROMOTING INFROMATIONAL WEBINAR"|
         PCs$PC=="SHARING LINK FOR LIVESTREAM"|
         PCs$PC=="SHARING Q&A SESSION"|
         PCs$PC=="SHARING TALK ON LOCAL RADIO"|
         PCs$PC=="SHARING TALK ON PRESIDENTAL CANDIDATES AND BLACK POLITICS"|
         PCs$PC=="SHARING TELEVISION TALK"|
         PCs$PC=="SHARING TULSA EDUCATIONAL SPEAKERS"|
         PCs$PC=="SHARING VIDEO OF ANTI-RACISM WORKSHOPS"|
         PCs$PC=="SHARING LIVE STREAM OF TALK WITH LOCAL BLACK FEMALE LEADERS"|
         PCs$PC=="SHARING LIVE STREAM OF VIRTUAL TOWNHALL WITH NATIONAL ORGANIZATIONS"|
         PCs$PC=="SHARING VIRTUAL TOWNHALL WITH NATIONAL ORGANIZATION"|
         PCs$PC=="SHARING DISCUSSION AT TOWNHALL"|
         PCs$PC=="SHARING LIVE STREAM OF VIRTUAL TOWNHALL WITH NATIONAL ORGANIZATIONS"|
         PCs$PC=="SHARING TOWNHALL DETAILS"]<- "SHARING PUBLIC TALK"

####  35 PROMOTING BAIL ####
PCs$PC_2[PCs$PC=="SHARING BAILOUT LIMIT" |
         PCs$PC=="OFFERING BAILOUT INSTRUCTIONS" |
         PCs$PC=="SHARING BAIL RESOURCES" |
         PCs$PC=="CLAIMING TO BE HELPING AN ARRESTED PROTESTER" |
         PCs$PC=="VOWING TO GET ARRESTED ACTIVISTS OUT OF JAIL" |
         PCs$PC=="ANNOUNCING THAT THEY BAIL WHENEVER POSSIBLE" |
         PCs$PC=="DISCUSSING PLANS TO BAIL OUT PRISONERS" |
         PCs$PC=="INFORMING IMPACT OF BAIL ON RACE AND GENDER" |
         PCs$PC=="NOTIFYING THAT BAIL AND DONATION LOCATIONS WILL BE SHARED SOON" |
         PCs$PC=="PROMOTING ARTIST WHOSE PROCEEDS GO TO COMMUNITY BAIL FUND"]<-"PROMOTING BAIL"

####  36 DEMANDING INVEST DIVEST ####
PCs$PC_2[PCs$PC=="CALLING FOR REALLOCATION OF FUNDS" |
         PCs$PC=="DEMANDING POLICE FUNDS BE SPENT ON OTHER PUBLIC RESOURCES" ]<-"DEMANDING INVEST DIVEST"

####  37 REDEFINING RACISM ####
PCs$PC_2[PCs$PC=="ASSOCIATING COMPLACENCY WITH ANTI-BLACKNESS" |
         PCs$PC=="ASSOCIATING THE DEVALUING OF BLACK AMERICAN LIVES WITH ACCEPTABLE AMERICAN CULTURE AND HISTORY" |
         PCs$PC=="CALLING OUT BIGOTRY IN PROGRESSIVE SPACES" |
         PCs$PC=="CALLING OUT COMPLACENCY" |
         PCs$PC=="CALLING OUT DISREGARD FOR BLACK LIVES" |
         PCs$PC=="CALLING OUT DOUBLE STANDARD" |
         PCs$PC=="CALLING OUT HYPOCRISY" |
         PCs$PC=="PROVIDING EXAMPLE OF STRUCTURAL RACISM" |
         PCs$PC=="POINTING OUT INJUSTICE IN HOW SOME ARE TREATEDBY POLICE VS OTHERS" |
         PCs$PC=="CALLING OUT IMPACT OF MICROAGGRESSIONS" |
         PCs$PC=="CALLING OUT RACIST DOUBLE STANDARDS" |
         PCs$PC=="CALLING OUT THOSE WHO REMAIN SILENT" |
         PCs$PC=="CALLING OUT WHITE PRIVILEGE" |
         PCs$PC=="CALLING TIO DISMANTLE WHITE SUPREMACY" |
         PCs$PC=="CHALLENGING PERCEPTIONS OF IRISH DISADVANTAGE" |
         PCs$PC=="COMPARING EXPERIENCES OF BLACK VERSUS WHITE AMERICANS" |
         PCs$PC=="DEFINING JUSTICE" |
         PCs$PC=="DEFINING JUSTICE FOR BLACK WOMEN AS SAFETY FROM VIOLENCE" |
         PCs$PC=="DISCUSSING ANTI-BLACKNESS" |
         PCs$PC=="DISCUSSING LACK OF HEALTHCARE IN MARGINALIZED COMMUNITIES" |
         PCs$PC=="DISCUSSING RACIAL DISPARITIES IN COVID" |
         PCs$PC=="DISCUSSING RACIAL DISPARITIES IN HEALTH" |
         PCs$PC=="DISCUSSING STRUGGLES OF WOMEN OF COLOR" |
         PCs$PC=="DISCUSSING THE DISREGARD FOR BLACK FEMALE POLICE BRUTALITY VICTIMS" |
         PCs$PC=="DISCUSSING VULNERABILITIES EXPOSED BY COVID-19 PANDEMIC" |
         PCs$PC=="EDUCATING ON BLACK MORTALITY" |
         PCs$PC=="EDUCATING ON ECONOMIC RACISM" |
         PCs$PC=="EDUCATING ON MASS INCARCERATION AND POLICE BRUTALITY" |
         PCs$PC=="EDUCATING ON RACIST HISTORY" |
         PCs$PC=="EDUCATING ON SYSTEMIC RACISM" |
         PCs$PC=="EXISTING WHILE BLACK" |
         PCs$PC=="EXPLAINING DANGER OF WHITE SUPREMACY PRESENT IN BUREAUCRACY" |
         PCs$PC=="IDENTIFYING A SYSTEMIC RACISM ISSUE" |
         PCs$PC=="IDENTIFYING ANTI-BLACK RACISM" |
         PCs$PC=="IDENTIFYING DISPARITIES" |
         PCs$PC=="IDENTIFYING DISPARITIES IN HEALTHCARE" |
         PCs$PC=="IDENTIFYING NEGLECT OF BLACK LIVES" |
         PCs$PC=="IDENTIFYING RACIST POLICIES" |
         PCs$PC=="CONDEMNING DOUBLE STANDARD" |
         PCs$PC=="CONDEMNING ERASURE OF BLACK WOMEN" |
         PCs$PC=="CONDEMNING HIPOCRASY" |
         PCs$PC=="CRITICIZING SYSTEMIC RACISM" |
         PCs$PC=="EXPRESSING A DUTY TO FIGHT" |
         PCs$PC=="EXPRESSING CONCERN OVER GENTRIFICATION AND WHITE SUPREMACY" |
         PCs$PC=="EXPRESSING FRUSTERATION THAT BLACK BEHAVIOR IS SCRUTINIZED UNJUSTLY" ]<-"REDEFINING RACISM"


####  38 ECONOMIC ####
PCs$PC_2[PCs$PC=="ANNOUNCING GRANTS TO BLACK-LED BUSINESSES" |
         PCs$PC=="DEMANDING BETTER POLICIES TO COMBAT HOMELESSNESS" |
         PCs$PC=="EDUCATING ON UNIVERSAL BASIC INCOME" |
         PCs$PC=="PETITIONING FOR FREE PRE SCHOOL IN COUNTY" |
         PCs$PC=="ASSERTING THE CONSTITUTIONAL RIGHTS OF THE UNHOUSED" |
         PCs$PC=="FRAMING HOMELESSNESS AS A DEFACTO CRIME" |
         PCs$PC=="FRAMING HOMELESSNESS AS AN ECONOMIC DRAIN" |
         PCs$PC=="IDENTIFYING A HOMELESSNESS PROBLEM" |
         PCs$PC=="ADVOCATING FOR HOMELESS" ]<-"ECONOMIC"



####  39 DEMANDING DEFUNDING POLICE #####
PCs$PC_2[PCs$PC=="DEMANDING DEFUND THE POLICE" |
         PCs$PC=="DEMANDING DEFUNDING THE POLICE" |
         PCs$PC=="DEMANDING DEFUND POLICE" |
         PCs$PC=="DEMANDING DEFUNDING" |
         PCs$PC=="OFFERING INFORMATION/ DEMANDING DEFUNDING POLICE"|
         PCs$PC=="CHALLENGING CITY COUNCIL TO VOTE AGAINST POLICE FUNDING" |
         PCs$PC=="CALLING FOR ELECTORAL ACTION AGAINST INCREASED JAIL FUNDS" |
         PCs$PC=="DEMANDING DEFUNIDNG POLICE" |
         PCs$PC=="CRITICISING PUBLIC FUNDING FOR POLICE" |
         PCs$PC=="PETITIONING TO DEFUND THE POLICE"|
         PCs$PC == "SHARING MEME TO DEFUND THE POLICE"|
         PCs$PC == "CALLING FOR DEFUNDING THE POLICE"| 	
         PCs$PC == "DEMANDING DIVESTMENT"| 
         PCs$PC == "DENOUNCING POLICE FUNDING"|
         PCs$PC == "DEMANDING A DEFUNDING POLICE"|
         PCs$PC == "DEMANDING DEFUNDING A DETENTION CENTER"| # local
         PCs$PC == "DEMANDING DEFUNDING THE POLICE WITH REINVESTMENT IN ALTERNATIVE RESOURNCES"| # defund/invest
         PCs$PC == "CALLING FOR DEFUNDING POLICE"|
         PCs$PC == "DEMANDING DEFUNDINGP POLICE"| 
         PCs$PC == "DEMANDING TO DEFUND THE PRISON SYSTEM"|
         PCs$PC == "PETITION TO DEFUND POLICE AND FUND OTHER LOCAL PROGRAMS"| # defund/invest
         PCs$PC == "PROMOTING DEFUND THE POLICE MOVEMENT"|
         PCs$PC == "CRITICIZING MAYOR FOR REFUSING TO DEFUND POLICE"|
         PCs$PC == "DISCUSSING PLANS TO DEFUND POLICE"|
         PCs$PC == "JUSTIFYING DEFUNDING POLICE"|
         PCs$PC == "ENCOURAGING CEASING DONATIONS TO POLICE ORGANIZAITONS"|
         PCs$PC == "IDENTIFYING PRO-MILITARIZATION"|
         PCs$PC == "SHARING IMAGE OF TAGGED BILBOARD THAT READS DEFUND BPD"|
         PCs$PC == "SIGN PETITION TO DEFUND MPD" ]<-"DEMANDING DEFUNDING POLICE" # local

####  40 DEMANDING ABOLISHING POLICE #####  
PCs$PC_2[PCs$PC=="DEMANDING ABOLISHING PRISON COMPLEX"|
         PCs$PC=="DEMANDING ABOLISHING THE PRISON INDUSTRIAL COMPLEX"|
         PCs$PC=="ABOLISHING POLICE"|
         PCs$PC=="DISCUSSING POLICE ABOLITION"|
         PCs$PC=="DISCUSSING POLICING ALTERNATIVES"|
         PCs$PC=="CALLING FOR ABOLITION AND FEMINISM"|
         PCs$PC=="DEMANDING ABOLITION"|
         PCs$PC=="DISCUSSING ABOLISHING POLICE"|
         PCs$PC=="DISCUSSING STRATEGY FOR ABOLISHING POLICE"]<- "DEMANDING ABOLISHING POLICE" 

####  41 CELEBRATING POLICY VICTORY #####
PCs$PC_2[PCs$PC=="SHARING AN EXAMPLE OF DEFUNDING THE POLICE"|
         PCs$PC=="CELEBRATING A VICTORY IN ANOTHER CITY"|
         PCs$PC=="CELEBRATING VICTORY"|
         PCs$PC=="CELEBRATING POLICY CHANGES"|
         PCs$PC=="REPORTING ON PROPOSED REMOVAL OF POLICE FROM SCHOOLS"|
         PCs$PC=="SHOWING AN EXAMPLE OF DEFUNIDNG POLICE"|
         PCs$PC=="DISCUSSING EXAMPLES OF POLICE REFORM"|
         PCs$PC=="PROVIDING EXAMPLES OF WAYS TO MAKE CHANGE"|
         PCs$PC=="SHARING AN EXAMPLE OF DEFUNDING THE POLICE"|
         PCs$PC=="ADDRESSING ISSUE IN ANOTHER CITY"|
         PCs$PC=="ANNOUNCING POLICY CHANGE"|
         PCs$PC=="DISCUSSING STRATEGY FOR ABOLISHING POLICE"]<- "CELEBRATING POLICY VICTORY" 



####  42 CRITIQUING WHITE ALLIES #####
PCs$PC_2[PCs$PC=="ADDRESSING WHITE PEOPLE"|
         PCs$PC=="BLAMING WHITE PEOPLE FOR BUSNESS DISTRUCTION IN SAC"|
         PCs$PC=="CALLING FOR WHITE PEOPLE TO ACKNOWLEDGE SYSTEMIC RACISM"|
         PCs$PC=="CALLING ON WHITE PEOPLE TO COMMIT TO ANTI-RACIST WORK"|
         PCs$PC=="CALLING WHITE ALLIES TO ACTION"| 
         PCs$PC=="CRITIQUING BEHAVIOR OF WHITE PROTESTERS AND WHITE LED ORGANIZATIONS"| 
         PCs$PC=="DEMANDING WHITE ACTIVISTM"|
         PCs$PC=="CRITICIZING NON BLACK PEOPLE"|
         PCs$PC=="CALLING OUT WHITE SUPREMACY"|
         PCs$PC=="DEMANDING WHITE PEOPLE CHANGE THEIR BEHAVIOR"| 
         PCs$PC=="ENCOURAGING WHITE ALLIES TO PARTICIPATE IN PROTEST"|
         PCs$PC=="IDENTIFYING IMPACTS OF WHITE PRIVILEGE"| 
         PCs$PC=="IMPLYING THE VIOLENT PROTESTERS ARE WHITE SUBURBAN KIDS"|
         PCs$PC=="POINTING OUT THE INADEQUACY OF SOME WHITE ALLIES"| 
         PCs$PC=="RECOUNTING EXPERIENCES WITH EDUCATING WHITES"]<- "CRITIQUING WHITE ALLIES" 

####  43 APPOLOGIZING FOR TECHNICAL ISSUES ####  
PCs$PC_2[PCs$PC=="APOLOGIZING FOR NOT POSTING MORE"|
         PCs$PC=="APOLOGIZING FOR TYPO"|
         PCs$PC=="APOLOGIZING FOR TECHNICAL ISSUES"|
         PCs$PC=="CORRECTING FALSE UPDATE"|
         PCs$PC=="CORRECTING MISLEADING INFORMATION"|
         PCs$PC=="APOLOGIZING FOR ORGANIZATIONAL LOGISTICS"]<- "APOLOGIZING FOR TECHNICAL ISSUES" 

####  44 ASKING HOW TO HELP ####  
PCs$PC_2[PCs$PC=="ASKING WHAT PROTESTERS IN MINNEAPOLIS NEED"]<- "ASKING HOW TO HELP"

####  45 CONDEMNING POLICE VIOLENCE ####  
PCs$PC_2[PCs$PC=="CALLING ATTENTION TO VIOLENCE EXPERIENCED BY  MARGINALIZED GROUPS"|
         PCs$PC=="CALLING OUT DISPROPORTIONATE VIOLENCE"|
         PCs$PC=="CITING VIDEO EVIDENCE OF POLICE VIOLENCE"|
         PCs$PC=="POINTING OUT POLICE BRUTALITY"|
         PCs$PC=="RETWEETING VIDEO OF POLICE BRUTATLITY"|
         PCs$PC=="IDENTIFYING PROGRESS"|
         PCs$PC=="REPORTING ON CITY EFFORTS TO DEFUND POLICE"|
         PCs$PC=="REPORTING ON PROGRESS"|
         PCs$PC=="SHARING CIVIC DEVELOPMENTS IN MINNEAPOLIS"|
         PCs$PC=="SHARING LEGISLATION IN ANOTHER STATE"|
         PCs$PC=="SHARING PROGRESS OF LOCAL ACTIVISTS"|
         PCs$PC=="SHARING THAT ALL GEORGE FLOYD ARRESTING OFFICERS WERE FIRED"|
         PCs$PC=="SHARING THAT NEW JERSEY IS CREATING CRISIS INTERVENTION TRAINING"|
         PCs$PC=="SHARING THAT NEW JERSEY IS UPDATING USE OF FORCE POLICY"|
         PCs$PC=="SHARING THAT SEATTLE MAYOR SUSPECTS TEAR GAS FOR 30 DAYS"|
         PCs$PC=="SHARING THE CHARGEST AGAINST CHAUVIN AND OTHER OFFICERS"|
         PCs$PC=="SHARING WHAT PROTEST HAS ACCOMPLISHED IN MINNEAPOLIS"|
         PCs$PC=="CONDEMNING STATE SANCTIONED VIOLENCE"|
         PCs$PC=="DEMANDING END TO POLICE VIOLENCE"|
         PCs$PC=="DETAILING BRUTALITY OF RAYSHARD BROOK'S ENCOUNTER WITH POLICE"|
         PCs$PC=="DETAILING THE DEATH OF KAYLA MOORE"|
         PCs$PC=="DISCUSSING AN INSTANCE OF POLICE BRUTALITY"|
         PCs$PC=="IDENTIFYING DISPROPORTIONATE POLICE FORCE"|
         PCs$PC=="CONDEMNING POLICE RESPONSE TO PROTEST"|
         PCs$PC=="CONDEMNING THE USE OF CROWD CONTROL MATERIALS"|
         PCs$PC=="CHARACTERIZING THE POLICE SYSTEM AS INHERENTLY VIOLENT"|
         PCs$PC=="DENOUNCING PERPETRATOR OF VIOLENCE"|
         PCs$PC=="DENOUNCING VIOLENCE AND ABUSE OF POWER"|
         PCs$PC=="DENOUNCING VIOLENCE AND ABUSE OF POWER"|
         PCs$PC=="EDUCATING ON HISTORY OF RACIAL VIOLENCE"|
         PCs$PC=="EDUCATING ON POLICE VIOLENCE DURING COVID"|
         PCs$PC=="IDENTIFYING PATTERNS OF POLICE VIOLENCE"|
         PCs$PC=="IDENTIFYING REALITIES OF POLICE VIOLENCE"|
         PCs$PC=="IDENTIFYING STATE SANCTIONED VIOLENCE"|
         PCs$PC=="LINKING AMERICAN MILITARY VIOLENCE TO DOMESTIC POLICEV VIOLENCE"|
         PCs$PC=="LINKING POLICE VIOLENCE TO DOMESTIC VIOLENCE"|
         PCs$PC=="LINKING POLICE VIOLENCE TO POLICIES"|
         PCs$PC=="CRITICIZING THE POLICE ACTIONS REGARDING RASHAUD BROOKS"|
         PCs$PC=="EXPOSING POLICE BRUTALITY"|
         PCs$PC=="CALLING OUT EXTREME USE OF FORCE"|
         PCs$PC=="CONDEMNING INHUMANE PRISON SYSTEM"|
         PCs$PC=="EXPRESSING ANGER AT POLICE ACTION"|
         PCs$PC=="SHARING VIDEO EVIDENCE"|
         PCs$PC=="SHARING VIDEO EVIDENCE OF POLICE BRUTALITY"|
         PCs$PC=="SHARING VIDEO OF BLACK WOMAN BEING ACCOSTED BY WHITE WOMAN IN ANOTHER CITY"|
         PCs$PC=="SHARING VIDEO OF ELDERLY PROTESTER SHOVED BY POLICE"|
         PCs$PC=="IDENTIFYING PATTERN OF ABUSE"|
         PCs$PC=="PLEADING FOR JUSTICE FOR BLACK PEOPLE AND END OF VIOLENCE"|
         PCs$PC=="PROVIDING INFORMATION ABOUT INSTANCE OF POLICE VIOLENCE"|
         PCs$PC=="QUESTIONING THE SOURCE OF VIOLENCE AT PROTESTS"|
         PCs$PC=="REMAINING IGNORANT OF POLICE VIOLENC"|
         PCs$PC=="SHARING A VIDEO OF RIOT POLICE ASKING TODDLER IF SHE IS GOING TO SHOOT THEM"|
         PCs$PC=="SHARING LOCAL ACCOUNT OF POLICE VIOLENCE"| 
         PCs$PC=="SHARING LOCAL POLICE VIOLENCE"|
         PCs$PC=="CONDEMNING THE KILLING OF RAYSHARD BROOKS"|
         PCs$PC=="COMMENTING ON LETHAL CONSEQUENCES OF DEHUMANIZATION"|
         PCs$PC=="REPORTING KILLING BY POLICE"|
         PCs$PC=="SHARING A VIDEO OF OFFICERS USING AGGRESSIVE CHOKEHOLD ON YOUNG MAN"|
         PCs$PC=="SHARING NEWS STORY ABOUT RACIAL PROFILING"|
         PCs$PC=="SHARING POLICE AGGRESSION"|
         PCs$PC=="SHARING POLICE FOOTAGE"|
         PCs$PC=="SHARING SEXUAL ASSAULT ALLEGATION AGAINST OFFICER ACCUSED OF KILLING TAYLOR"|
         PCs$PC=="SHARING STORIES OF POLICE MURDERS"|
         PCs$PC=="SHARING THAT 14 YEAR OLD WAS HIT IN EYE WITH RUBBER BULLET"|
         PCs$PC=="WARNING OF GRAPHIC VIDEO CONTENT"|
         PCs$PC=="IDENTIFYING SYSTEMIC PROBLEMS IN LAW ENFORCEMENT"|
         PCs$PC=="SHARING VIDEO OF POLICE VIOLENCE"|
         PCs$PC=="STATING THAT POLICE ARE INCITING VIOLENCE ACROSS THE COUNTRY"]<- "CONDEMNING POLICE VIOLENCE"

####  46 SHARING SUCCESS STORY #####
PCs$PC_2[PCs$PC=="SHARING A SUCCESS STORY"| 
         PCs$PC=="ANNOUNCING SUCCESSES"|
         PCs$PC=="CELEBRATING LEGISLATIVE SUCCESS, AND ACKNOWLEDGING WORK AHEAD"|
         PCs$PC=="RECOGNIZING A SUCCESS"|
         PCs$PC=="REPORTING A SUCCESS STORY"|
         PCs$PC=="REPORTING ON SUCCESS AND PROGRESS"|
         PCs$PC=="REPORTING SUCCESS"|
         PCs$PC=="SHARING THAT CA IS CONSIDERING REPARATIONS"|
         PCs$PC=="REPORTING SUCCESSES"|
         PCs$PC=="SHARING A POLICE DEFUNIDNG SUCCESS STORY"|
         PCs$PC=="CELEBRATING POLICY VICTORY"]<- "SHARING POLICY SUCCESS" 

####  47 ENCOURAGING ONLINE PARTICIATION #####
PCs$PC_2[PCs$PC=="CLARIFYING INSTRUCTIONS FOR HOW TO PARTICIPATE IN ONLINE DEMONSTRATION"| 
         PCs$PC=="SHARING ONLINE COMMUNITY BUILDING ON ZOOM"|
         PCs$PC=="SHARING WAYS TO MOBILIZE ONLINE"|
         PCs$PC=="ASKING FOR ONLINE ACTIVISM"|
         PCs$PC=="ASKING FOR PARTICIPATION IN ONLINE ACTIVISM"|
         PCs$PC=="ENCOURAGING ONLINE ACTIVISM"|
         PCs$PC=="OFFERING HELP ONLINE"| 
         PCs$PC=="SHARING PETITIONS"| 
         PCs$PC=="PETITIONING"| 
         PCs$PC=="ASKING FOLLOWERS FOR THEIR FAVORITE LIVE STREAMING AP"| 
         PCs$PC=="OFFERING LIVE FEED OF PROTESTS"| 
         PCs$PC=="ENCOURAGING VIRTUAL PROTEST"| 
         PCs$PC=="ASKING FOR PETITION SIGNATURES"| 
         PCs$PC=="CALLING FOR PEOPLE TO CHANGE THEIR PROFILE PICTURE TO SHOW SOLIDARITY"| 	
         PCs$PC=="ENCOURAGING FOLLOWERS"| 
         PCs$PC=="ENCOURAGING FOLLOWERS TO FOLLOW AN ACCOUNT"| 
         PCs$PC=="ASKING FOR RETWEETS"| 	
         PCs$PC=="SUPPORTING HASHTAG CAMPAIGN"|
         PCs$PC=="ASKING TO SIGN A PETITION"|
         PCs$PC=="CALLING ON PEOPLE TO PUBLICLY PLEDGE TO CHANGE THEIR BEHAVIOR"|
         PCs$PC=="ENCOURAGING PROTESTERS TO SHARE PROTEST UPDATES"| 
         PCs$PC=="ASKING FOR ONLINE PARTICIPATION"|   
         PCs$PC=="PROMOTING ANT-POLICING ONLINE EVENT"|
         PCs$PC=="PROMOTING ONLINE CAMPAIGN"|
         PCs$PC=="PROVIDING STEPS TO PARTICIPATE IN ONLINE ACTIVISM"|
         PCs$PC=="SHARING LIVESTREAM FOR ONLINE PARTICIPATION"|
         PCs$PC=="SHARING ONLINE COMMUNITY SPACES ON ZOOM"]<- "ENCOURAGING ONLINE PARTICIATION" 

####  48 EXPRESSING GROUP DEMANDS #####
PCs$PC_2[PCs$PC=="SHARING CHAPTER DEMANDS"| 
         PCs$PC=="PETITIONING BLM DEMANDS"| 
         PCs$PC=="ORGANIZING DEMANDS"| 
         PCs$PC=="SHARING DEMANDS"| 
         PCs$PC=="DEFINING MISSION OF CAMPAIGN"| 
         PCs$PC=="SHARING M4BL PLATFORM"| 
         PCs$PC=="SHARING MISSION OF THIS ACCOUNT"|
         PCs$PC=="SHARING PLATFORM OF MOVEMENT FOR BLACK LIVES"| 
         PCs$PC=="STATING MISSION OF ACCOUNT"| 
         PCs$PC=="STATING MISSION OF ORGANIZATION"|
         PCs$PC=="STATING THEIR PURPOSE AS A SUPPORT ORGANIZATION"| 
         PCs$PC=="CLARIFYING THE INTENT OF THE ORGANIZATION"| 
         PCs$PC=="EXPLAINING PROTEST MOTIVATIONS"| 
         PCs$PC=="EXPRESSING DEMANDS"|
         PCs$PC=="RESTATING DEMANDS IN SPECIFIC TERMS"|
         PCs$PC=="CLARIFYING THE INTENT OF ORGANIZATION"|
         PCs$PC=="DISCUSSING INTENTIONS OF ORGANIZATION"|
         PCs$PC=="SHARING CORE VALUES OF ORGANIZATION"|
         PCs$PC=="SHARING DEMANDS IN LINK"|
         PCs$PC=="SHARING DEMANDS OF LOCAL PROTESTERS"|
         PCs$PC=="SHARING DEMANDS OF MINNEAPOLIS OFFICIALS"|
         PCs$PC=="SHARING ENUMERATED DEMANDS"| 
         PCs$PC=="OFFERING PROTEST DEMANDS"]<- "EXPRESSING GROUP DEMANDS" 

####  49 SHARING CELEBRITY PARTICIPATION #####
PCs$PC_2[PCs$PC=="SHARING CELEBRITY AT BLM PROTEST"| 
         PCs$PC=="SHARING CELEBRITY FUNDRAISING FOR BLM"| 
         PCs$PC=="THANKING CELEBRITY FOR SUPPORT"| 
         PCs$PC=="THANKING CELEBRITY"| 
         PCs$PC=="SHARING CELEBRITIES SUPPORTING BLM"| 
         PCs$PC=="SHARING CELEBRITIES DONATING TO BLM"|
         PCs$PC=="SHARING KPOP SUPPORT FOR BLM"]<- "SHARING CELEBRITY PARTICIPATION" 

####  50 MISC #####
PCs$PC_2[PCs$PC=="IDENTIFYING PATTERNS OF ANTI-BLACK POLICING"| 
         PCs$PC=="EXPRESSING DISAPPROVAL OF POLICE"| 
         PCs$PC=="CRITICIZING ANTI PROTEST BLACK CELEBRITIES"| 
         PCs$PC=="ACKNOWLEDGING ONGOING STRUGGLE"|
         PCs$PC=="CALLING OUT FORCED STERILIZATION"| 
         PCs$PC=="ADDRESSING COMMUNITY NEEDS"| 
         PCs$PC=="ADVOCATING FOR LOCAL MOVEMENT"| 
         PCs$PC=="ACKNOWLEDGING DIFFERENT FORMS OF ACTIVISM"| 
         PCs$PC=="ACKNOWLEDGING HISTORICAL PATTERNS"| 
         PCs$PC=="ADVOCATING FOR PRISONERS IN DURAHM"| 
         PCs$PC=="ENCOURAGING REST AND SELF LOVE"| 
         PCs$PC=="CLARIFYING DEMAND"| 
         PCs$PC=="ENCOURAGING SELF CARE AND WELLNESS"| # SELF CARE
         PCs$PC=="SUPPORTING SELF-PRESERVATION"|
         PCs$PC=="HOLDING HEALING SPACE FOR COMMUNITY MEMBERS"|
         PCs$PC=="ASKING POLITICIANS HOW THEY CAN SUPPORT THEM"| 
         PCs$PC=="CALLING FOR JUSTICE BEFORE PEACE"]<- "MISC" 

####  51 CRITICIZING PUBLIC SPENDING #####
PCs$PC_2[PCs$PC=="COMPARING BUDGETS"| 
         PCs$PC=="DISCUSSING CITY BUDGET"| 
         PCs$PC=="CRITICIZING PUBLIC SPENDING"| 
         PCs$PC=="COMPARING POLICE BUDGET TO MILITARY SPENDING OF MULTIPLE COUNTRIES"| 
         PCs$PC=="COMPARING POLICE SPENDING TO OTHER PUBLIC RESOURCES"| 
         PCs$PC=="CRITICIZING STATE SPENDING"| 
         PCs$PC=="SHARING LOCAL POLICE BUDGET"| 
         PCs$PC=="SHARING OTHER WAYS THE CITY COULD SPEND MONEY RATHER THAN ON POLICE"| 
         PCs$PC=="SHARING POLICE BUDGET"| 
         PCs$PC=="SHARING PROPORTION OF BUDGET THAT GOES TO POLICE"| 
         PCs$PC=="SHARING PUBLIC BUDGET"| 
         PCs$PC=="SHARING REPORT OF HOW CITY BUDGET SURPLUS WILL BE SPENT"| 
         PCs$PC=="SHARING WHAT RESIDENTS WOULD RATHER SPEND MONEY ON THAN A JAIL"| 
         PCs$PC=="CRITICIZING THE APPROACH TO INCARCERATIO"]<- "CRITICIZING PUBLIC SPENDING"

####  52 MEETING OFFICIAL #####
PCs$PC_2[PCs$PC=="MEETING WITH MAYOR"| 
         PCs$PC=="POSTING PRESS BRIEFING WITH A MAYOR"|
         PCs$PC=="SHARING LOCAL PRESS CONFERENCE"|
         PCs$PC=="SHARING OFFICIAL STATEMENT"|
         PCs$PC=="SHARING LOCAL PRESS CONFERENCE INFORMATION"|
         PCs$PC=="REPORTING ON DISCUSSION WITH LOCAL POLITICIAN"]<- "MEETING OFFICIAL"

####  53 CRITICIZING MEDIA #####
PCs$PC_2[PCs$PC=="CONDEMNING LACK OF MEDIA COVERAGE OF PROTEST"| 
         PCs$PC=="CRITICIZING JOURNALISTS"| 
         PCs$PC=="CRITICIZING THE MEDIA"| 
         PCs$PC=="CRITICIZING A PUNDIT"| 
         PCs$PC=="CRITICIZING LOCAL NEWS FOR NOT TALKING TO ACTUAL BLM ACTIVISTS"| 
         PCs$PC=="CRITICIZING THE MEDIA FOR PROTEST COVERAGE"]<- "CRITICIZING MEDIA"

####  54 GRIEVING #####
PCs$PC_2[PCs$PC=="ACKNOWLEDGING COLLECTIVE GRIEF"| 
         PCs$PC=="ACKNOWLEDGING COLLECTIVE GRIEF"| 
         PCs$PC=="EXPRESSING COLLECTIVE ANGER"| 
         PCs$PC=="EXPRESSING COLLECTIVE TRAUMA"| 
         PCs$PC=="EXPRESSING COMMUNITY NEEDS AND SHARED TRAUMA"| 
         PCs$PC=="EXPRESSING EMOTIONAL IMPACT OF RACIAL VIOLENCE"| 
         PCs$PC=="EXPRESSING FATIGUE"]<- "GRIEVING" 

####  55 GRIFT #####
PCs$PC_2[PCs$PC=="CRITIQUING SOCCER CLUB FOR SELLING BLM MERCH WITH NO TRANSPARENCY"| 
         PCs$PC=="DISCOURAGING THE PURCHASE OF BLM MERCH FROM OUTLETS OTHER THAN BLM"| 
         PCs$PC=="DISCOURAGING UNOFFICIAL PROTESTS"| 
         PCs$PC=="CRITICIZING OPPORTUNISM?"|
         PCs$PC=="CRITIQUING PERFORMATIVE JUSTICE"| 
         PCs$PC=="CALLING OUT SELF SERVING USE OF SOCIAL MEDIA WITHOUT RECIPROCITY"| 
         PCs$PC=="WARNING OTHERS TO NOT SUPPORT SHAUN KING"| 
         PCs$PC=="ASKING SOCCER CLUB WHERE BLM FUNDRAISING MONEY WILL GO"| 
         PCs$PC=="ASKING THE PUBLIC WHERE THE FUNDS FROM SOCCER CLUB FUNDRAISING SHOULD GO"| 
         PCs$PC=="DISCOURAGING UNORGANIZED PROTESTING"| 
         PCs$PC=="ENCOURAGING INTROSPECTION AMONG PROTESTERS"| 
         PCs$PC=="ENCOURAGING PEOPLE TO EDUCATE THEMSELVES"]<- "GRIFT" 

####  56 CELEBRATING BLACKNESS #####
PCs$PC_2[PCs$PC=="SUPPORTING BLACK YOUTH"|
         PCs$PC=="SHARING PERFORMERS AT CELEBRATING BLACK LIVES EVENT"|
         PCs$PC=="SHARING OLD IMAGE OF BLACK WOMAN UNPHASED BY RIOT POLICE"|
         PCs$PC=="SHARING LOCAL BLACK ARTISTS"|
         PCs$PC=="SHARING BLACK LITERATURE"|
         PCs$PC=="SHARING BLACK CELEBRATION AT PROTEST"|
         PCs$PC=="SHARING BLACK ART"|
         PCs$PC=="CELEBRATION OF BLACK LIVE"|
         PCs$PC=="COLLECTING BLACK OWNED BUSNESS NAMES FOR AN AP"|
         PCs$PC=="COMMEMORATING JUNETEENTH"|
         PCs$PC=="CREATING A BLACK ONLY SPACE"|
         PCs$PC=="EMPHASIZING BLACK JOY"|
         PCs$PC=="CELEBRATING JUNETEENTH"|
         PCs$PC=="SHARING JUNETEENTH EVENT"|
         PCs$PC=="HONORING BLACK PEOPLE"|
         PCs$PC=="SHARING CELBRATION OF BLACK LIVES EVENT"|
         PCs$PC=="CELEBRATING JUNETEENTH"|
         PCs$PC=="UPLIFTING BLACK WOMEN"|
         PCs$PC=="HONORING WOMEN OF COLOR"|
         PCs$PC=="OFFERING SPIRITUAL SUPPORT TO BLACK PEOPLE"|
         PCs$PC=="SHARING BIRTHING AND PARENTING CLASS FOR BLACK WOMEN"|
         PCs$PC=="SHARING JUNETEENTH EVENT"|
         PCs$PC=="SHARING PRAYER FOR ANCESTORIAL CONNECTION AND HEALING"|
         PCs$PC=="SHARING PROTESTERS DANCING TO BEYONCE"|
         PCs$PC=="PROMOTING BLACK AUTHORS"|
         PCs$PC=="ENCOURAGING SUPPORTING BLACK OWNED BUSINESSES"|
         PCs$PC=="ENCOURAGING SUPPORT OF BLACK BUSINESSES"|
         PCs$PC=="ENCOURAGING BLACK SELF-LOVE"|
         PCs$PC=="ENCOURAGING BLACK PEOPLE"|
         PCs$PC=="ENCOURAGING BLACK DISABLED PEOPLE"|
         PCs$PC=="EDUCATING ACTIVISM IN THE BLACK TECH INDUSTRY"|
         PCs$PC=="DEMANDING SUPPORT FOR BLACK ARTISTS"|
         PCs$PC=="CONGRATULATING BLACK BUSINESS OWNERS"|
         PCs$PC=="CELEBRATING BLACK MUSIC"|
         PCs$PC=="CELEBRATING BLACK WOMEN FOR STRENGTH AND MORAL CERTITUDE"|
         PCs$PC=="CALLING FOR BLACK RADICAL UNITY"|
         PCs$PC=="CALLING FOR BLACK CELEBRATION AND HEALING"|
         PCs$PC=="ADVOCATING FOR BLACK LIBERATION"|
         PCs$PC=="HIGHLIGHTING A BLACK PERSON"|
         PCs$PC=="EDUCATING ON BLACK REVOLUTION"]<- "CELEBRATING BLACKNESS "

####  57 INTERSECTIONALIZING MOVEMENT #####
PCs$PC_2[PCs$PC=="SUPPORTING BLACK QUEER PEOPLE"|
         PCs$PC=="SPOTLIGHTING BLACK WOMEN IN THE PROTEST"|
         PCs$PC=="SHARING DISABILITY BLACK RIGHTS LEADERS"|
         PCs$PC=="SEEKING ARTISTS TO COMMEMORATE BLACK TRANS VICTIMS OF VIOLENCE"|
         PCs$PC=="REPORTING DEATHS OF BLACK TRANS WOMEN"|
         PCs$PC=="REMEMBERING LIVES OF BLACK WOMEN"|
         PCs$PC=="ADVOCATING FOR DISABILITY INTERSECTIONALITY"|
         PCs$PC=="PROMISING TO PROTECT BLACK WOMEN"| 
         PCs$PC=="PROMOTING LGBT INTERSECTIONALITY"|
         PCs$PC=="ACKNOWLEDGING SOLIDARITY ACROSS DIFFERENCES"| 
         PCs$PC=="MEMORIALIZING BLACK TRANS VICTIMS OF PATRIARCHICAL VIOLENCE"| 
         PCs$PC=="LINKING TYPES OF CIVIL RIGHTS TO BLACK CIVIL RIGHTS"|
         PCs$PC=="LINKING REPRODUCTIVE JUSTICE TO BLACK LIVES MATTER"|
         PCs$PC=="LINKING BLACK CIVIL RIGHTS WITH PRIDE"|
         PCs$PC=="LIFTING UP NAMES AND STORIES OF BLACK WOMEN"|
         PCs$PC=="INTERSECTIONAL CALL FOR REPRODUCTIVE RIGHTS AND BLACK LIVES"|
         PCs$PC=="HONORING BLACK TRANS WOMEN"|
         PCs$PC=="HONORING BLACK TRANS LIVES"|
         PCs$PC=="FOCUSING ON VIOLENCE TOWARD BLACK TRANS LIVES"|
         PCs$PC=="HONORING BLACK FEMALE VICTIMS OF POLICE BRUTALITY"|
         PCs$PC=="FOCUSING ON THE INTERSECTION OF TRANSPHOBIA AND RACISM IMPACTING BLACK TRANS PEOPLE"|
         PCs$PC=="ENCOURAGING BLACK DISABLED PEOPLE"|
         PCs$PC=="DETAILING ATTACK AGAINST BLACK TRANSGENDER VICTIM"|
         PCs$PC=="COMPARING THE QUEER AND BLACK EXPERIENCE"|
         PCs$PC=="CALLING ATTENTION TO BLACK TRANS LIVES"|
         PCs$PC=="ADVOCATING FOR BLACK LIBERATION"|
         PCs$PC=="CALLING ATTENTION TO THE INTERSECTIONALITY"|
         PCs$PC=="CALLING FOR INTERSECTIONAL ACTIVISM"|
         PCs$PC=="DISCUSSING IMPORTANCE OF INTERSECTIONALITY"|
         PCs$PC=="DISCUSSING INTERSECTIONALITY"|
         PCs$PC=="DRAWING ON INTERSECTIONAL EXAMPLES"|
         PCs$PC=="ENCOURAGING INTERSECTIONALITY"|
         PCs$PC=="EXPRESSING INTERSECTIONAL SOLIDARITY"|
         PCs$PC=="EXPRESSING MISSION OF INTERSECTIONAL ACTIVISM"|
         PCs$PC=="INTERSECTIONAL CALL TO GET RID OF VIOLENCE"|
         PCs$PC=="INTERSECTIONAL LINK TO OTHER PROTETSTS"|
         PCs$PC=="INTERSECTIONALIZING BIRTH CONTROL AND RACE"|
         PCs$PC=="SHARING INTERSECTIONAL BEHAVIOR AT PROTEST"|
         PCs$PC=="SHARING INTERSECTIONAL SUPPORT"|
         PCs$PC=="ACKNOWLEDGING THE ERASURE OF BLACK WOMEN VICTIMS"|
         PCs$PC=="ADDRESSING LACK REPRESENTION OF BLACK WOMEN IN BLM"|
         PCs$PC=="ANNOUNCING EVENT FOR SOLIDARITY WITH BLACK MOTHERS"|
         PCs$PC=="COMBATING NOTION THAT PROTESTERS SHOULD HAVE A PARTICULAR IDENTITY"|
         PCs$PC=="CONVEYING RACIAL SOLIDARITY WITH NATIVE AMERICANS"|
         PCs$PC=="DISCUSSING LACK OF POLICE RESPONSE TO SEXUAL ABUSE"|
         PCs$PC=="DISCUSSING REPRODUCTIVE JUSTICE"|
         PCs$PC=="IDENTIFYING THE STUGGLES OF BLACK WOMANHOOD"|
         PCs$PC=="INCLUDING ALL BLACK LIVES IN BLM MOVEMENT"|
         PCs$PC=="MEMORIALIZING WOMEN"|
         PCs$PC=="NOTING THE BEGINING OF PRIDE MONTH"|
         PCs$PC=="ENCOURAGING SOLIDARITY AMONG POC"|
         PCs$PC=="EXPRESSING SOLIDARITY WITH PALESTINE"|
         PCs$PC=="RECOGNIZING VIOLENCE TOWARDS BLACK WOMEN"|
         PCs$PC=="CELEBRATING PRIDE"|
         PCs$PC=="EDUCATING ON SOUTH ASIAN RESISTANCE"|
         PCs$PC=="PROMOTING DISCUSSION WITH CHRISTIAN AND MUSLIM FAITH LEADERS"|         
         PCs$PC=="DEFENDING TWEET THAT DISABLED PROTESTS ARE INSPIRING"|
         PCs$PC=="SHARING PRIDE PROTEST"|
         PCs$PC=="INCLUDING BLACK WOMEN"|
         PCs$PC=="LINKING PRIDE TO CURRENT PROTESTS VIA MARSHA P JOHNSON"|
         PCs$PC=="LINKING PRIDE TO RIOTS"|
         PCs$PC=="PROMOTING DIVERSITY OF PROTESTERS"|
         PCs$PC=="PROMOTING INTERESECTIONALITY IN ACTIVISM"|
         PCs$PC=="PROMOTING LOCAL DISABILITY LED PROTEST"|
         PCs$PC=="PROMOTING THE DIVERSITY OF PROTESTERS"|
         PCs$PC=="RECOGNIZING DIFFERING LIVED EXPERIENCES"|
         PCs$PC=="REPORTING MURDERS OF QUEER VICTIMS"|
         PCs$PC=="RETWEETING ABOUT GHANA AND THEIR GOLD"|
         PCs$PC=="SHARING INDIAN EXPLANER OF BLM"|
         PCs$PC=="SHARING INTERETHNIC SOLIDARITY"|
         PCs$PC=="SHARING JEWISH SUPPORT FOR PROTESTERS"|
         PCs$PC=="UNIFYING WHO CAN PROTEST"|
         PCs$PC=="SHARING ANTI-RACIST PROVERBS IN ISLAM"|
         PCs$PC=="HONORING BLACK WOMEN"]<- "INTERSECTIONALIZING MOVEMENT"

####  58 SHARING ACCOUNT IDENTITY ####  
PCs$PC_2[PCs$PC=="SHARING ORGANIZER EMAIL"|
         PCs$PC=="INTRODUCING ACCOUNT"|
         PCs$PC=="CLARIFYING PROTEST LEADERS"|
         PCs$PC=="SHARING ORGANIZER ON NATIONAL TV"|
         PCs$PC=="REPLYING TO PERSON THAT THEY ARE THE ORGANIZIERS FOR AN EVENT"|
         PCs$PC=="DENYING THAT A PROTEST LEADER IS PART OF THEIR CHAPTER"|
         PCs$PC=="DISTINGUISHING THIS GROUP FROM ANOTHER IN THE SAME AREA"|
         PCs$PC=="EXPLANATION OF WHO THEY ARE AND WHAT THEY DO"|
         PCs$PC=="SHARING ORGANIZER EMAIL"]<- "SHARING ACCOUNT IDENTITY" 

####  59 WARNING OF BAD ACTORS ####  
PCs$PC_2[PCs$PC=="WARNING OF INFILTRATORS IN MOVEMENT"|
         PCs$PC=="WARNING PROTESTERS TO NOT SPREAD A HASHTAG"|
         PCs$PC =="WARNING OF ILLIGETIMATE ORGANIZERS"|
         PCs$PC=="WARNING OF BAD ACTORS IN PROTESTS"|
         PCs$PC =="WARNING PROTESTERS OF BAD ACTORS"|
         PCs$PC =="EXPOSING PROTEST INTIGATORS"|
         PCs$PC =="DISCUSSING ICE INVOLVEMENT WITH POLICE"|
         PCs$PC =="SHARING CONCERN THAT COUNTER PROTESTERS ARE DISGUISED AS POLICE"|
         PCs$PC=="WARNING USERS OF A SUSPICIOUS ACCOUNT"]<- "WARNING OF BAD ACTORS" 

####  60 CONDEMNING RIOT ####  
PCs$PC_2[PCs$PC=="EXPRESSING DISCONTENT FOR RIOTING"]<- "CONDEMNING RIOT" 

####  61 CONNECTED TO HISTORY OF RACIAL INJUSTICE ####  
PCs$PC_2[PCs$PC=="COMMENTING ON PREVENTING HISTORICAL PATTERNS OF INJUSTICE"|
         PCs$PC=="EDUCATING ON HISTORY OF POLITICAL ORGANIZATIONS"|
         PCs$PC=="IDENTIFYING HISTORICAL PATTERN"|
         PCs$PC=="LINKING THE KILLING OF GEORGE FLOYD TO ERIC GARNER"|
         PCs$PC=="LINKING THE MURDER OF GARNER AND FLOYD"|
         PCs$PC=="MAKING HISTORICAL COMPARISON"|
         PCs$PC=="RECKONING WITH HISTORY"|
         PCs$PC=="SHARING STRUCTURAL BARRIERS TO VOTING"|
         PCs$PC=="IDENTIFYING A SYSTEMIC PATTERN"|
         PCs$PC=="IDENTIFYING ABUSIVE PATTERNS"|
         PCs$PC=="IDENTIFYING SYSTEMIC PROBLEMS"|
         PCs$PC=="POINTING OUT INJUSTICE IN HOW SOME ARE TREATED BY POLICE VS OTHERS"|
         PCs$PC=="COMMENTING ON HISTORY OF POLICE VIOLENCE IN THE US"|
         PCs$PC=="ADVOCATING FOR TRUTHFUL HISTORY"| 
         PCs$PC=="SHARING MINI DOCUMENTARY"|
         PCs$PC=="PROMOTING CIVIL RIGHTS DOCUMENTARY ON HISTORY CHANNEL"|
         PCs$PC=="PROMOTING DOCUMENTARY"|
         PCs$PC=="COMMENTING ON HISTORY OF STATE SANCTIONED VIOLENCE"|
         PCs$PC=="IDENTIFYING A SOCIETAL PROBLEM"|
         PCs$PC=="IDENTIFYING PREDATORY LANDLORD PRACTICES"|
         PCs$PC=="IDENTIFYING THE ROOTS OF PROBLEMS"|
         PCs$PC=="INFORMING ABOUT COVID AND IMMIGRATION"|
         PCs$PC=="MAKING HISTORICAL COMPARISON"|
         PCs$PC=="RECOGNIZING HISTORICAL PATTERNS"|
         PCs$PC=="REMEMBERING INCIDENTS OF RACIST TERRORISM"|
         PCs$PC=="REVEALING ABUSE OF POWER IMBEDDED IN POLICING SYSTEM"|
         PCs$PC=="SAYING THAT THE SYSTEM HAS ALWAYS BEEN FLAWED"|
         PCs$PC=="SHARING LONG TIMELINE FROM EARLY SLAVE TRADE TO TO NOW"|
         PCs$PC=="SHARING THE ECONOMIC IMPACTS OF OVERPOLICING THE UNHOUSED"|
         PCs$PC=="SHARING TULSA MASSACARE"]<- "CONNECTED TO HISTORY OF RACIAL INJUSTICE" 

####  62 DEMANDING INVESTMENT ####  
PCs$PC_2[PCs$PC=="DEMANDING FUNDING BLACK COMMUNITIES"|
         PCs$PC=="DEMANDING FUNDING FOR SOCIAL PROGRAMS"|
         PCs$PC=="DEMANDING FUNDING OTHER PROGRAMS"|
         PCs$PC=="DEMANDING FUNDING OTHER SOCIAL PROGRAMS"|
         PCs$PC=="DEMANDING INVESTMENT IN OTHER COMMUNITY RESOURCES"|
         PCs$PC=="DEMANDING INVESTMENT IN OTHER SERVICES"|
         PCs$PC=="DEMANDING INVESTMENTS IN OTHER WAYS"|
         PCs$PC=="DEMANDING INVESTMENTS IN SCHOOLS"|
         PCs$PC=="INVEST DIVEST"|
         PCs$PC=="DEMANDING SUPPORT FOR RACIAL JUSTICE EFFORTS"]<- "DEMANDING INVESTMENT" 

####  63 ENCOURAGING ELECTORAL PARTICIPATION ####  
PCs$PC_2[PCs$PC=="CALLING ON PEOPLE TO RESITER TO VOTE"|
         PCs$PC=="ENCOURAGING ELECTORAL PARTICIPATION"|
         PCs$PC=="PROMOTING PHONE BANKING"]<- "ENCOURAGING ELECTORAL PARTICIPATION" 

####  64 GRIFT ####  
PCs$PC_2[PCs$PC=="WARNING OTHERS TO NOTSUPPORT SHAUN KING"|
         PCs$PC=="CRITICIZING OPPORTUNISM?"|
         PCs$PC=="CRITIQUING PERFORMATIVE JUSTICE"|
         PCs$PC=="REPORTING FAKE EVENT SHARED ON TWITTER"|
         PCs$PC=="WARNING AGAINST FOLLOWING FALSE LEADERS"|
         PCs$PC=="WARNING AGAINST OPPORTUNISM"]<- "WARNING OF GRIFT" 

####  65 IMAGINING FUTURES ####  
PCs$PC_2[PCs$PC=="ENVISIONING A NEW FUTURE"|
         PCs$PC=="CONCEPTUALIZING REFORMED LAW ENFORCEMENT"|
         PCs$PC=="DEMANDING LONG-TERM SOLUTIONS"|
         PCs$PC=="DEMANDING SYSTEMIC CHANGE"|
         PCs$PC=="DENYING THE EFFICACY OF POLICING"|
         PCs$PC=="DISCOURAGING PEOPLE FROM FINDING SOLUTION IN THE CURRENT INCARCERAL SYSTEM"|
         PCs$PC=="DISCUSSING STAKES OF THE MOVEMENT"|
         PCs$PC=="EDUCATING ON DISBANDING THE POLICE"|
         PCs$PC=="ENCOURAGING PUBLIC AWARENESS OF ALTERNATIVES TO POLICING"|
         PCs$PC=="POINTING OUT INADEQUACY IN POLICING"|
         PCs$PC=="PROPOSING LAW ENFORCEMENT ALTERNATIVES"|
         PCs$PC=="REIMAGINING POLICING ROLES"|
         PCs$PC=="SHARING A VISION OF THE FUTURE"|
         PCs$PC=="SHARING PLANS FOR THE FUTURE"]<- "IMAGINING FUTURES" 

####  66 INVEST DIVEST ####  
PCs$PC_2[PCs$PC=="DEMANDING INVEST DIVEST"]<- "INVEST DIVEST" 

####  67 MEETING OFFICIAL ####  
PCs$PC_2[PCs$PC=="REPORTING ON DISCUSSION WITH LOCAL POLITICIAN"|
         PCs$PC=="MEETING OFFICIAL"|
         PCs$PC=="SHARING THAT A COUNCIL MEMBER REACHED OUT TO TALK TO PROTESTER"]<- "MEETING OFFICIAL" 

####  67 SHARING NO PROTEST ####  
PCs$PC_2[PCs$PC=="JUSTIFYING DECISION NOT TO PROTEST"|
         PCs$PC=="EXPRESSING INTENT NOT TO PROTEST"|
         PCs$PC=="INSTRUCTING NOT TO PROTEST"]<- "SHARING NO PROTEST" 

####  68 SHARING NO PROTEST ####  
PCs$PC_2[PCs$PC=="IDENTIFYING A DOUBLE STANDARD"|
         PCs$PC=="CALLING ON PROTESTERS TO FIGHT AGAINST WHITE SUPREMACY AND POVERTY"|
         PCs$PC=="ADDRESSING SYSTEMIC RACISM"|
         PCs$PC=="ASSOCIATING THE AMPLIFICATION OF FORCES WITH SYSTEMATIC RACISM"|
         PCs$PC=="CALLING FOR FREEDOM"|
         PCs$PC=="DISCUSSING THE USE OF BLACK PAIN"|
         PCs$PC=="COMPARING DOUBLE STANDARDS"|
         PCs$PC=="DEBUNKING AMERICAN HEROICS"|
         PCs$PC=="DEFENDING BEHAVIOR OF THOSE ADVERSELY IMPACTED BY SYSTEMETIC OPPRESSION"|
         PCs$PC=="EXPOSING HYPOCRICY"|
         PCs$PC=="IDENTIFYING DOUBLE STANDARD"|
         PCs$PC=="LINKING POLICING AND RACIAL CAPITALISM"|
         PCs$PC=="PROMOTING ANTI-RACISM"|
         PCs$PC=="IDENTIFYING NEGLECTED STORIES"|
         PCs$PC=="POINTING OUT HYPOCRISY"|
         PCs$PC=="PROVIDING EXAMPLE OF STRUCTURAL RACISM"|
         PCs$PC=="PROVIDING RESOURCES ON STRUCTURAL ANALYSIS OF RACE, GENDER, AND POLICING"|
         PCs$PC=="RECOGNIZING EVOLVING OPPRESSION"|
         PCs$PC=="REFRAMING POLICE MISCONDUCT AS POLICING ITSELF"|
         PCs$PC=="RIDICULING MISCONCEPTIONS ABOUT ANTI-RACIST MOVEMENTS"|
         PCs$PC=="SHARING MEME LINKING RACISM TO AMERICAN CULTURE"|
         PCs$PC=="SHARING QUOTE ON SYSTEMIC RACISM"|
         PCs$PC=="TERRORIZING BLACK PEOPLE"]<- "REDEFINING RACISM" 

####  69 SURVEYING FOLLOWERS ####  
PCs$PC_2[PCs$PC=="ASKING FOLLOWERS FOR THEIR FAVORITE LIVE STREAMING AP"|
         PCs$PC=="ASKING FOR INFORMATION"|
         PCs$PC=="ASKING HOW TO HELP"|
         PCs$PC=="SHARING COVID SURVEY"|
         PCs$PC=="SHARING SURVEY"|
         PCs$PC=="CROWDSOURCING IDEAS/STRATEGIES FEEDBACK"|
         PCs$PC=="SHARING COVID SURVEY"|
         PCs$PC=="REQUESTING VERBAL OR VIDEO EVIDENCE OF POLICE BRUTALITY"|
         PCs$PC=="CROWDSOURCING IDEAS/STRATEGIES FEEDBACK"|
         PCs$PC=="SOLICITING COMMENTS"]<- "SURVEYING FOLLOWERS " 


####  70 SHARING NO PROTEST ####  
PCs$PC_2[PCs$PC=="JUSTIFYING DECISION NOT TO PROTEST"|
         PCs$PC=="INSTRUCTING NOT TO PROTEST"]<- "SHARING NO PROTEST" 

####  71 BLAMING PROTESTERS FOR VIOLENCE ####  
PCs$PC_2[PCs$PC=="REPORTING ON VIOLENCE DIRECTED AT POLICE"|
         PCs$PC=="REPORTING ON AGGRESSION TOWARDS POLICE"]<- "BLAMING PROTESTERS FOR VIOLENCE" 

####  72 INTRODUCING ACCOUNT ####  
PCs$PC_2[PCs$PC=="SHARING BIOS OF LINCOLN ORGANIZERS"|
         PCs$PC=="SHARING DEMOGRAPHICS OF ORGANIZATION"|
         PCs$PC=="SHARING DETAILS FOR NATIONAL M4BL EVENT WITH FOCUS ON MINNEAPOLIS LEADERS"|
         PCs$PC=="SHARING INFORMATION ABOUT DREAM DEFENDERS"]<- "INTRODUCING ACCOUNT" 


####  73 CREATING IDENTITY ####  
PCs$PC_2[PCs$PC=="SHARING A PLAYLIST"| # SHARING ART OR MUSIC
         PCs$PC=="SHARING A SONG"|
         PCs$PC=="USING A SLOGAN ABOUT PEACEFUL PROTEST"|
         PCs$PC=="USING HUMOR"|
         PCs$PC=="SHARING POLITICAL CARTOON"|
         PCs$PC=="RESPONDING THAT THEY ARE UNAFFILATED WITH ANY ORGANIZATION"|
         PCs$PC=="SHARING ART OF AFRICAN VISUAL ARTIST"|
         PCs$PC=="SHARING PHOTO OF MURALS"|
         PCs$PC=="SHARING CARTOONS AND MEMES"|
         PCs$PC=="SHARING SUPPORT FOR PROTESTERS AND ARTISTS"]<- "CREATING IDENTITY" 

####  74 CONNECTING DISPERATE ISSUES ####  
PCs$PC_2[PCs$PC=="LINKING CLIMATE STRUGGLE WITH RACIAL INEQUALITY"| # 
         PCs$PC=="LINKING COVID AND POLICE FATALITITIES AS STRUCTURAL RACISM"|
         PCs$PC=="LINKING COVID RISKS TO ABORTION RISKS"|
         PCs$PC=="LINKING DISABILITY AND REPRODUCTIVE RIGHTS"|
         PCs$PC=="LINKING ALL ISSUES THAT AFFECT BLACK COMMUNITY TO THESE PROTESTS"|
         PCs$PC=="LINKING CLIMATE STRUGGLE WITH RACIAL INEQUALITY"|
         PCs$PC=="LINKING COVID AND POLICE FATALITITIES AS STRUCTURAL RACISM"|
         PCs$PC=="LINKING COVID RISKS TO ABORTION RIGHTS"|
         PCs$PC=="LINKING DISABILITY TO REPRODUCTIVE RIGHTS"|
         PCs$PC=="LINKING DISABILITY AND REPRODUCTIVE RIGHTS"|
         PCs$PC=="LINKING HOUSING TO CLIMATE"|
         PCs$PC=="LINKING IMMIGRATION AND BORDER ENFORCEMENT TO CALLS AGAINST LOCAL POLICE"|
         PCs$PC=="LINKING MINNEAPOLIS TO OTHER CITIES"|
         PCs$PC=="LINKING MULTIPLE FORMS OF OPPRESSION"|
         PCs$PC=="LINKING MUSIC VIDEO ABOUT BEING IMEPRFECT"|
         PCs$PC=="LINKING POLICE TACTS IN FLORIDA TO TACTICS USED ELSEWHERE"|
         PCs$PC=="LINKING POLICE, COVID, AND CAPITALISM AS DANGERS TO BLACK PEOPLE"|
         PCs$PC=="LINKING POLICING AND RACIAL CAPITALSIM"|
         PCs$PC=="LINKING POLICING TO CAPITALISM"|
         PCs$PC=="LINKING POLICING TO COLONIALISM"|
         PCs$PC=="LINKING REPRODUCTIVE RIGHTS TO MORE GENERAL FREEDOMS"|
         PCs$PC=="LINKING SAFETY TO COMMUNITY, NOT POLICE"|
         PCs$PC=="LINKING HOUSING TO CLIMATE"]<- "CONNECTING DISPERATE ISSUES " 


####  75 CONNECTING DISPERATE ISSUES ####  
PCs$PC_2[PCs$PC=="LINKING CLIMATE STRUGGLE WITH RACIAL INEQUALITY"| # 
         PCs$PC=="LINKING COVID AND POLICE FATALITITIES AS STRUCTURAL RACISM"|
         PCs$PC=="LINKING COVID RISKS TO ABORTION RISKS"|
         PCs$PC=="LINKING DISABILITY AND REPRODUCTIVE RIGHTS"|
         PCs$PC=="LINKING HOUSING TO CLIMATE"]<- "CONNECTING DISPERATE ISSUES " 

####  76 SHARING OFFICIAL NEWS OR MEDIA ####  
PCs$PC_2[PCs$PC=="SHARING A PODCAST"|  
         PCs$PC=="SHARING AN ARTICLE"|         
         PCs$PC=="SHARING ARTICLE ABOUT CHAUVIN ARREST"|  
         PCs$PC=="SHARING INFORMATIONAL ARTICLE"|  
         PCs$PC=="SHARING INFORMATIONAL CONTEN"|
         PCs$PC=="SHARING INFORMATIONAL CONTENT"|  
         PCs$PC=="SHARING INFORMATIONAL NEWS COVERAGE"|
         PCs$PC=="SHARING LOCAL NEWS COVERAGE"|  
         PCs$PC=="SHARING MAYOR ON PODCAST TO DISCUSS POLICE VIOLENCE"|
         PCs$PC=="SHARING NEWS OF GF OFFICER RELEASE"|
         PCs$PC=="SHARING NEWS OF MISSING CHILD"|
         PCs$PC=="SHARING NEWS SEGMENT"|
         PCs$PC=="OFFERING A NEWS SOURCE"|
         PCs$PC=="SHARING ARTICLE"|
         PCs$PC=="SHARING NEWS THAT MISSING CHILD WAS FOUND DEAD"|
         PCs$PC=="SHARING NEWS THAT PORTLAND POLICE CHEIF RESIGNED"]<- "SHARING OFFICIAL NEWS OR MEDIA" 


####  77 OFFERING RESOURCES #####
PCs$PC_2[ PCs$PC=="OFFERING A HUB FOR ORGANIZING PROTESTS"|
          PCs$PC=="OFFERING ANSWERS TO QUESTIONS"|
          PCs$PC=="OFFERIG ASSISTANCE WITH MEDICAL EXPENSES"|
          PCs$PC=="OFFERING FINANCIAL SUPPORT"|
          PCs$PC=="OFFERING INFORMATION ON ABLITION TEACHING"|
          PCs$PC=="OFFERING INFORMATION ON HYPER-SURVEILLANCE"|
          PCs$PC=="OFFERING INFORMATION ON POLICE BRUTALITY"|
          PCs$PC=="OFFERING INFORMATION ON STATE BUDGET"|
          PCs$PC=="OFFERING MEDICAL ASSISTANCE TO PROTESTERS"|
          PCs$PC=="OFFERING MEDICAL HELP"|
          PCs$PC=="SHARING PAGE OF RESOURCES"|
          PCs$PC=="OFFERING PROTEST INFORMATION"|
          PCs$PC=="OFFERING SUPPORT TO PROTESTERS"|
          PCs$PC=="OFFERING TO POST BAIL" |
          PCs$PC=="OFFERING ASSISTANCE WITH MEDICAL EXPENSES"|
          PCs$PC=="OFFERING MEDICAL ASSISTANCE TO PROTESTERS"|
          PCs$PC=="OFFERING SUPPORT AND ADVICE"|
          PCs$PC=="PROVIDING LEGAL SUPPORT"]<-"OFFERING RESOURCES" 

####  78 DEMANDING INVESTMENT #####
PCs$PC_2[ PCs$PC=="DEMANDING COMMUNITY LED EFFORTS"|
          PCs$PC=="DEMANDING FUDNING BLACK COMMUNITIES"|
          PCs$PC=="DEMANDING FUNDING FOR SOCIAL PROGRAMS"|
          PCs$PC=="DEMANDING FUNDING OTHER PROGRAMS"|
          PCs$PC=="DEMANDING FUNDING OTHER SOCIAL PROGRAMS"|
          PCs$PC=="DEMANDING INVESTMENT IN OTHER COMMUNITY RESOURCES"|
          PCs$PC=="DEMANDING INVESTMENT IN OTHER SERVICES"|
          PCs$PC=="DEMANDING INVESTMENTS IN OTHER WAYS"|
          PCs$PC=="DEMANDING INVESTMENTS IN SCHOOLS"|
          PCs$PC=="DEMANDING LONG-TERM SOLUTIONS"|
          PCs$PC=="DEMANDING PROTESTION OF HUMAN RIGHTS"|
          PCs$PC=="DEMANDING RELEASE OF OFFICER"|
          PCs$PC=="DEMANDING FUND ALLOCATION"|
          PCs$PC=="DEMANDING INVEST DIVEST"|
          PCs$PC=="DEMANDING INVESTMENT "|
          PCs$PC=="DEMANDING FUND REALLOCATION"|
          PCs$PC=="DEMANDING INVEST DIVEST"|
          PCs$PC=="DEMANDING INVESTMENT IN COMMUNITY"|
          PCs$PC=="DEMANDING SUPPORT FOR RACIAL JUSTICE EFFORTS"]<-"DEMANDING INVESTMENT" 

####  77 PROTEST LOGISTICS #####
PCs$PC_2[ PCs$PC=="UPDATING PROTEST ACTIVITIES"|
          PCs$PC=="SHARING PROTEST EVENT"|
          PCs$PC=="SHARING PROTEST EVENTS"|
          PCs$PC=="ENCOURAGING PROTEST SAFETY"|
          PCs$PC=="ADVISING PROTESTERS"| 
          PCs$PC=="APPOLOGIZING FOR TECHNICAL ISSUE"]<-"PROTEST LOGISTICS" 

####  78 ARGUING AGAINST MASS INCARCERATION #####
PCs$PC_2[ PCs$PC=="EDUCATING ON MASS INCARCERATION"]<-"ARGUING AGAINST MASS INCARCERATION" 

####  79 EMOTING NEGATIVE #####
PCs$PC_2[ PCs$PC=="EXPRESSING ANGER"|
          PCs$PC=="EXPRESSING DISSAPOINTMENT"|
          PCs$PC=="RECOUNTING TRAUMA"|
          PCs$PC=="DISCUSSING THE PAIN OF UNCHANGED POLICE BRUTALITY"|
          PCs$PC=="EXPRESSING CONCERN"|
          PCs$PC=="EXPRESSING DISGUST"|
          PCs$PC=="EXPRESSING DISSAPOINTMENT AT ELECTORAL RESULTS"|
          PCs$PC=="EXPRESSING DISSATISFACTION WITH WORDS"|
          PCs$PC=="EXPRESSING DOUBT IN BALTIMORE'S DEMOCRACY"|
          PCs$PC=="GRIEVING"]<-"EMOTING NEGATIVE" 

####  80 ADVOCATING FOR HOMELESS #####
PCs$PC_2[ PCs$PC=="EXPRESSING ANGER"|
          PCs$PC=="EXPLAINING THE CRIMINALIZATION OF HOMELESSNESS"|
          PCs$PC=="DISCOURAGING POLICING HOMELESSNESS"]<-"ADVOCATING FOR HOMELESS" 

####  81 ADVOCATING FOR PRISONERS #####
PCs$PC_2[PCs$PC=="SUPPORTING PEOPLE IN CRIMINAL JUSTICE SYSTEM"|
         PCs$PC=="RECRUITING VOLUNTEERS FOR LETTERS TO PRISONERS"|
         PCs$PC=="SUPPORTING YOUTH IN CRIMINAL JUSTICE SYSTEM"|
         PCs$PC=="SHARING A TRAGIC PRISON STORY"]<-"ADVOCATING FOR PRISONERS" 

####  82 CRITICIZING PROTESTERS #####
PCs$PC_2[PCs$PC=="CONDEMNING RIOT"]<-"CRITICIZING PROTESTERS"

####  83 ADVOCATING FOR DRUG USERS #####
PCs$PC_2[PCs$PC=="COUNTERING STIGMAS OF DRUG"]<-"ADVOCATING FOR DRUG USERS"


####  84 DEMANDING HUMAN RIGHT #####
PCs$PC_2[PCs$PC=="DEMANDING A RIGHT"]<-"DEMANDING HUMAN RIGHT"


####  85 ARGUING FOR POLICIES #####
PCs$PC_2[PCs$PC=="DESCRIBING IMPORTANCE OF POLICY CHANGE"|
         PCs$PC=="EDUCATING ON POLICE REFORM"|
         PCs$PC=="EDUCATING ON POLICE CAUSED DEATHS"|
         PCs$PC=="QUESTIONING THE VALUE OF POLICE"|
         PCs$PC=="PROVIDING EVIDENCE FOR ABOLISHING POLICE"]<-"ARGUING FOR POLICIES"

####  86 ARGUING FOR ABOLITION #####
PCs$PC_2[PCs$PC=="DISCUSSING PRISION INDUSTRIAL COMPLEX ABOLITION"|
         PCs$PC=="EDUCATING ON ABOLITION"|
         PCs$PC=="EDUCATING ON MASS INCARCERATION"|
         PCs$PC=="SHARING ABOLITIONIST EDUCATIONAL EVENT"|
         PCs$PC=="SHARING ABOLITIONIST RESOURCES"|
         PCs$PC=="SHARING ABOLITIONIST STEPS"|
         PCs$PC=="JUSTIFYING ABOLISHING POLICE"]<-"ARGUING FOR ABOLITION"

####  87 SHARING A JOB #####
PCs$PC_2[PCs$PC=="POSTING A JOB OFFER"|
         PCs$PC=="POSTING A JOB OPENING FOR WRITER"|
         PCs$PC=="SEEKING AN EMPLOYEE"|
         PCs$PC=="SEEKING AN EMPLOYEE"|
         PCs$PC=="SEEKING ARTIST"|
         PCs$PC=="SEEKING ARTISTS"]<-"SHARING A JOB"

####  88 PROTECTING PRIVACY #####
PCs$PC_2[PCs$PC=="REFUSING TO RELEASE ORGANIZIERS FOR FEAR OF DOXING"]<-"PROTECTING PRIVACY"


####  89 INCITING ANGER #####
PCs$PC_2[PCs$PC=="RETWEETING RIGHTWING JOURNALIST"|
         PCs$PC=="SHARING RACIST COMMENTS ONLINE"|
         PCs$PC=="SHARING VIDEO OF NYPD PRESS CONFERENCE WHERE POLICE DEMAND MORE RESPECT"|
         PCs$PC=="SHARING REPORT OF THREATS TO KILL BLACK PROTESTERS"|
         PCs$PC=="SHARING STORY OF MISGENDERING OF FEMALE PRISONERS"|
         PCs$PC=="RETWEETING THAT FLOYD PUT HIMSELF IN THE SITUATION" ]<-"INCITING ANGER"


##### 90 CONDEMNING LOCAL OFFICIALS ####
PCs$PC_2[ PCs$PC=="COMDEMNING GOVERNMENT OFFICIALS" |
            PCs$PC=="CONDEMNING LOCAL REPRESENTATIVE" |
            PCs$PC=="CONDEMNING POLITICIAN" |
            PCs$PC=="ADDRESSING REPRESENTATIVE DIRECTLY" |
            PCs$PC=="SHAMING A POLITICIAN" |
            PCs$PC=="CALLING TRUMP A RACIST" |
            PCs$PC=="EXPOSING AN OFFICIAL" |
            PCs$PC=="CALLING OUT GOVERNMENT FAILURES" |
            PCs$PC=="CALLING OUT VOTER SUPPRESSION" |
            PCs$PC=="HOLDING MAYOR TO THEIR PROMISE" |
            PCs$PC=="IDENTIFYING FAILURES OF GOVERNMENT" |
            PCs$PC=="CRITICIZING LOCAL GOVERNMENT" |
            PCs$PC=="SHARING THAT TRUMP CALLED BLACK PEOPLE THUGS"|
            PCs$PC=="SHOWING WEAKNESS OF ELECTED OFFICIAL"|
            PCs$PC=="STATING THAT CLEVELAND PREVENTS PUBLIC COMMENT IN CITY COUNCIL MEETINGS"|
            PCs$PC=="TWEETING AT STATE ATTORNEY THAT THEY ARE SLOWWALKING TRANSPARENCY REPORTS"|
            PCs$PC=="CRITICIZING NATIONAL GUARD DEPLOYMENT"|
            PCs$PC=="CRITIQUING LOCAL CURFEW"|
            PCs$PC=="DENOUNCING FAILURES OF LOCAL GOVERNMENT"|
            PCs$PC=="CRITIQUING LOCAL GOVERNMENT POLICY"|
            PCs$PC=="DEMANDING THE CALLING OFF OF ARMED FORCES"|
            PCs$PC=="DEMANDING THE MAYOR MEET THE DEMANDS"|
            PCs$PC=="CRITICIZING THE JUSTICE SYSTEM"|
            PCs$PC=="SHAMING POLITICIANS" ]<-"CONDEMNING LOCAL OFFICIALS" 

#SHAREING PETITITON FOR COUNCIL ACTION
##### 91 CONDEMNING LOCAL POLICE #####
PCs$PC_2[ PCs$PC=="CONDEMNING POLICE CHIEF" |
            PCs$PC=="CONDEMNING POLICE VIOLENCE" |
            PCs$PC=="ADDRESSING POLICE BRUTALITY" |
            PCs$PC=="ADDRESSING POLICE CHIEF" |
            PCs$PC=="CRITICIZING THE LAPD FOR THEIR CONDUCT DURING A PUBLIC TOWNHALL TYPE EVENT" |
            PCs$PC=="ADDRESSING POLICE CHIEF DIRECTLY" |
            PCs$PC=="CONJECTURING THAT IT IS EASIER TO FIRE OFFICERS THAN IT MAY APPEAR"|
            PCs$PC=="IDENTIFYING SYSTEMIC PROBLEMS IN LAW ENFORCEMENT"|
            PCs$PC=="SHARING A VIDEO OF RIOT POLICE ASKING TODDLER IF SHE IS GOING TO SHOOT THEM"|
            PCs$PC=="SHARE VIDEO OF BLACK MAN BEING ARRESTED AND POLICE FINDING OUT THAT THIS IS NOT HIS IDENTITY"|
            PCs$PC=="SHARING THAT THE TAYLOR WARRENT WAS ILLEGAL"|
            PCs$PC=="ADDRESSING POLICE VIOLENCE"]<-"CONDEMNING LOCAL POLICE"  #

PCs$PC_2[ PCs$PC=="CONDEMNING USE OF FORCE ON CROWD"]<-"CONDEMNING STATE OFFICIALS" 


##### 92 COLLABORATING WITH LOCAL ORGANIZATION ####
PCs$PC_2[ PCs$PC=="ANNOUNCING A COLLABORATION"|
            PCs$PC=="CROSS POSTING OTHER ORGANIZATIONS"|
            PCs$PC=="ANNOUNCING A SPONSORSHIP"|
            PCs$PC=="COLLABORATING WITH LOCAL ORG"|
            PCs$PC=="COLLABORATING WITH LOCAL ORGANIZATIONS"|
            PCs$PC=="COLLABORATING WITH LOCAL ORGANIZERS"|
            PCs$PC=="COLLABORATING WITH M4BL"|
            PCs$PC=="COLLABORATING WITH NATIONAL ORGANIZATIONS"|
            PCs$PC=="EXPRESSING INTEREST IN WORKING WITH OTHER PROTESTERS"|
            ### POSSIBLY NOT LOCAL
            PCs$PC=="PROMOTING OTHER BLM CHAPTERS"|
            PCs$PC=="PROMOTING WEBSITE OF ORGANIZATION"|
            PCs$PC=="PROVIDING INFORMATION ON AN ORGANIZATION"|
            PCs$PC=="REACHING OUT TO MIAMI"|
            PCs$PC=="SHARED STATEMENT BY OTHER ORGANIZATION"|
            PCs$PC=="SHARING BRITISH ORGANIZATIONS SUPPORT FOR PROTESTERS"|
            PCs$PC=="SHARING LINK TO POC LED ORGANIZATIONS"|
            PCs$PC=="SHARING LINKS MULTIPLE ACTIVIST ORGANIZATIONS"|
            PCs$PC=="SHARING LINKS TO OTHER LOCAL ORGANIZATIONS"|
            PCs$PC=="SHARING NORTORIOUS HACKER GROUPS SUPPORT FOR BLM"|
            PCs$PC=="SHARING OTHER LOCAL ORGANIZATION"|
            PCs$PC=="SHARING OTHER SOCIAL MEDIA ACCOUNTS"|
            PCs$PC=="SHARING RESOURCES"|
            PCs$PC=="SHARING RESOURCES FROM NATIONAL ORGANIZATION"|
            PCs$PC=="SHARING RESOURCES MANUAL"|
            PCs$PC=="SHARING THAT K-POP FANS ARE AMBUSHING #BLUELIVESMATTER AND OTHER PROPOLICE CAMPAIGNS"|
            PCs$PC=="CLARIFYING ORGANIZATION AFFILIATION"|
            PCs$PC=="COLLABORATING WITH OTHER ORGANIZATIONS"|
            PCs$PC=="CLARIFYING ORGANIZATION AFFILIATIONS"|
            PCs$PC=="CELEBRATING OTHER REGIONAL PROTEST CHAPTER"|
            PCs$PC=="CLARIFYING ORGANIZATION AFFLICATION"|
            PCs$PC=="CLARIFYING ORGANIZATION N AFFILIATIONS"|
            PCs$PC=="ANNOUNCING A PARTNERSHIP"]<-"COLLABORATING WITH LOCAL ORGANIZATION" 
##### 93 ANTAGONIZING POLICE #####
PCs$PC_2[ PCs$PC=="CALLING COPS PIGS"|
            PCs$PC=="ANTAGONIZING POLICE"|
            PCs$PC=="CALLING POLICE PIGS"|
            PCs$PC=="ANTAGONIZING NORTORIOUSLY AGGRESSIVE POLICE DEPARTMENTS" |
            PCs$PC=="ANTAGONIZING THE POLICE"]<-"ANTAGONIZING POLICE" 

##### 94 SPOTLIGHTING HYPOCRICY #####
PCs$PC_2[ PCs$PC=="POINTING OUT HYPOCRICY"|
            PCs$PC=="CALLING OUT HYPOCRICY IN FAMOUS RAPPERS" |
            PCs$PC=="CALLING OUT HYPOCRICY"]<-"SPOTLIGHTING HYPOCRICY" 

##### 95 ENCOURAGING PROTESTERS ######
PCs$PC_2[ PCs$PC=="SUPPORTING PROTESTORS"|
            PCs$PC == "COMMENDING PROTESTERS FOR SHOWING UP"]<-"ENCOURAGING PROTESTERS"


## REMOVE ACCOUNTS THAT ARENT MEANGINGFUL
length(unique(PCs$PC))  # There are 2106 unique process codes
length(unique(PCs$PC_2)) # There are 85 2nd dimensional process codes

# Remove Accounts that don't apply to theory
PCs <- filter(PCs, Username != "ConMijente"  & 
                        Username != "sparrowmedia" &
                        Username != "ukblm" & 
                        Username != "rooseveltinst" & 
                        Username != "BLM_TO" )

length(unique(PCs$PC))  # There are 1920 unique process codes
length(unique(PCs$PC_2)) # There are 85 2nd dimensional process codes
dim(PCs)


length(unique(PCs$Tweet.Id))
length(unique(PCs$City))
length(unique(PCs$Username))

## PC Table #####
PC_Table <- PCs %>%
  group_by(PC_2) %>%
  summarize(n = n())


# Write PC Table 
write.csv(PC_Table, file = "/Users/aricaschuett/Documents/Process Codes/PCs_Table.csv")

# CREATING VERB SUBJECT 
PCs[c('PC_Verbs', "PC_Subjects")] <- str_split_fixed(PCs$PC_2, ' ', 2)

length(unique(PCs$PC_Verbs))  # There are 217 unique verbs for PCs/ 46 for PC_2
length(unique(PCs$PC_Subjects))  # There are 1918 unique subjects/ 80 for PC_2

# What are the verbs?
verbs <- unique(PCs$PC_Verbs)

verbstable <- PCs %>%
  group_by(PC_Verbs) %>%
  summarize(n = n())

rm(verbs)

write.csv(verbstable, file = "/Users/aricaschuett/Documents/Process Codes/Verbs_Reduced4-14.csv")


length(unique(PCs$PC_Verbs))  # There are 210 unique verbs
length(unique(PCs$PC_Subjects))  # There are 1727 unique subjects
length(unique(PCs$PC))  # There are 1832 unique PCs

write.csv(PCs, file = "/Users/aricaschuett/Documents/Process Codes/PCs4-14.csv")

PC_Index <- read.csv("/Users/aricaschuett/Documents/Process Codes/PCs4-14.csv")

# total post word cloud
PC_Index$PC_Subjects<-gsub(" ", "_", PC_Index$PC_Subjects)

VerbCorpus <-
  Corpus(VectorSource(PC_Index$PC_Verbs)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

VerbCorpus <- sort(rowSums(VerbCorpus), decreasing=TRUE)
VerbCorpus <- data.frame(word = names(VerbCorpus), freq=VerbCorpus, row.names = NULL)

# build wordcloud
wordcloud <- wordcloud2(data = VerbCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloud


# create subject wordcloud
# Convert the data into a summary table
PC_Index$PC_Subjects<-gsub(" ", "_", PC_Index$PC_Subjects)

textCorpusSub <-
  Corpus(VectorSource(PC_Index$PC_Subjects)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpusSub <- sort(rowSums(textCorpusSub), decreasing=TRUE)
textCorpusSub <- data.frame(word = names(textCorpusSub), freq=textCorpusSub, row.names = NULL)

# build wordcloud
wordcloudSub <- wordcloud2(data = textCorpusSub, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloudSub

# Fully PC cloud
PCsWC <- PCs
PCsWC$PC_2<-gsub(" ", "_", PCsWC$PC_2)


PCCorpus <-
  Corpus(VectorSource(PCsWC$PC_2)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

PCCorpus <- sort(rowSums(PCCorpus), decreasing=TRUE)
#VerbCorpus <- filter(freq >= 3)
PCCorpus <- data.frame(word = names(PCCorpus), freq=PCCorpus, row.names = NULL)

# build wordcloud
PCwordcloud <- wordcloud2(data = PCCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
PCwordcloud

############ Demands
demands <- grep(PCs, "DEMAND") 

# give obs where ABRAMS is in "CANDIDATE" variable 
demands <- PCs %>%
  filter(grepl("DEMAND", PC_2))

demandsTable <- demands %>%
  group_by(PC_2) %>%
  summarise((n=n()))

demandsDateCount <- demands %>%
  group_by(Date) %>%
  summarize(n=n())

# Libraries
library(ggplot2)

# Plot
ggplot(demandsDateCount, aes(Date, n, group = 1)) +
  labs(title ="Demands over Time", xlab = "Date", ylab= "Count") +
  geom_line()
  
PostsDateCount <- CompletePCs %>%
  group_by(Date) %>%
  summarize(n=n())
  
ggplot(PostsDateCount, aes(Date, n, group = 1)) +
  labs(title ="Posts over Time", xlab = "Date", ylab= "Count") +
  geom_line()


### Demands as percent of posts by group

PostsByGroupCount <- PCs %>%  #USING PCs here. Make sure I'm using the appropriate dataframe
  group_by(Username) %>%
  summarize(n=n())

DemandsByGroupCount <- demands %>%
  group_by(Username) %>%
  summarize(n=n())

DemandsPostGroup<- merge(PostsByGroupCount,  DemandsByGroupCount, by = "Username")

colnames(DemandsPostGroup) <- c("Username", "Posts", "Demands")

# Ratio of Posts which are demands
DemandsPostGroup$DemandsRatio <- DemandsPostGroup$Demands/DemandsPostGroup$Posts


# 









