#########################################################################################################
# BC2407 Semester Analytics Project: Initial Data Cleaning
# Team Number 8
# Members: Christopher Gerard Toh, Teo Tian Shun Kenneth, Lim De Quan, Jonathan Kevin Chandra
# Datasets: dice_com-job_us_sample.csv, skills_one_hot.csv, skills_td_cut_syn.csv
# Library: dplyr, plyr, tidytext, tidyr, stringr
########################################################################################################


library(dplyr)
library(plyr)
library(tidytext)
library(tidyr)
library(stringr)


setwd("C:/Users/jkchandra/Desktop/BC2407/Semester Project")

dfraw <- read.csv("dice_com-job_us_sample.csv")

#Useful columns only
df1 <- subset(dfraw, select = c(company, employmenttype_jobstatus, jobdescription, joblocation_address, jobtitle, shift, skills))
#Sets rowid as unique identifier
df1$rowid <- as.integer(rownames(df1))
df1$skills <- as.character(df1$skills)

#JOB TITLE DATA CLEANING =============================================================

#Cutting down on the number of job titles
jobtitle_df <- data_frame(line = 1:length(df1$jobtitle), titles = df1$jobtitle)
jobtitle_count <- count(jobtitle_df, "titles")

#Cut those with frequency below 10 and put in decreasing order
jobtitle_count_cut <- subset(jobtitle_count, freq >= 10)
jobtitle_count_cut <- jobtitle_count_cut[order(-jobtitle_count_cut$freq),]

frequent_jobtitle <- jobtitle_count_cut$titles

#SKILLS DATA CLEANING ==========================================================================

skills_df <- data_frame(line = 1:length(df1$skills),skills = df1$skills)

#Converts to tidy text
skills_td <- skills_df %>%
               unnest_tokens(word, skills, token = stringr::str_split, pattern=",|\n|/|;|\\Band\\B")

#Removing leading and trailing whitespace
skills_td$word <- trimws(skills_td$word, which = "both")

#Counts the number of frequency of skills
skills_count <- skills_td %>%
                  dplyr::count(word, sort = TRUE)

#Cut skills with freqencies lower than 51
skills_count_cut <- subset(skills_count, n>50)

frequent_skills <- skills_count_cut$word

skills_td_cut <- skills_td %>%
                    filter(word %in% frequent_skills)

write.csv(skills_td_cut, "skills_td_cut.csv", row.names = FALSE)


#Go to lookUpReplace.py to eliminate similar words


#Come back after DataCleaning.OneHot.R

#MERGING ===============================================================================

#import skills one hot
skills_one_hot <- read.csv("skills_one_hot.csv")

df2 <- subset(df1, df1$jobtitle %in% frequent_jobtitle)
df3 <- merge(x = df2, y = skills_one_hot, by.x = "rowid", by.y = "line", sort = TRUE)

#Convert all NA to 0
df3[is.na(df3)] <- 0

df4 <- subset(df3, select = -c(rowid, company, skills, employmenttype_jobstatus, 
                               jobdescription, joblocation_address, shift))

#Export for Predictive Model
write.csv(df4, "final_title_skills.csv", row.names = FALSE)

#For Aggregated Skills
dfaggregate <- read.csv("skills_td_cut_syn.csv")
dfaggregate <- stats::aggregate(word ~ line, data = dfaggregate,  paste, collapse = ",")
dfaggregate2 <- merge(x = df2, y = dfaggregate, by.x = "rowid", by.y = "line", sort = TRUE)
dfaggregate2 <- subset(dfaggregate2 , select = c(jobtitle, word))
dfaggregate3 <- str_split_fixed(dfaggregate2$word, ",", n = Inf)
write.csv(dfaggregate3, "skills_title_aggregate.csv", row.names = FALSE)

#FOR POWER BI =====================================================================================

library(splitstackshape)
dfraw$joblocation_address <- as.character(dfraw$joblocation_address)
dfraw$jobdescription <- as.character(dfraw$jobdescription)

data1[data1$company == "Robert Half"]$company <- "Robert Half Technology"

data1 <- cSplit(as.data.table(dfraw), "joblocation_address", ",")
data1 <- subset(data1, select = -c(joblocation_address_3))
names(data1)[names(data1) == 'joblocation_address_2'] <- 'state'
names(data1)[names(data1) == 'joblocation_address_1'] <- 'city'
data1[is.na(data1$state) | data1$state == " " | data1$state == ""]$state <- "missing"
data1[data1$state == "Washington"]$state <- "WA"
data1$state <- droplevels(data1$state)

data1 <- cSplit(as.data.table(data1), "employmenttype_jobstatus", ",")
data1 <- subset(data1, select = -c(employmenttype_jobstatus_2,employmenttype_jobstatus_3,employmenttype_jobstatus_4,employmenttype_jobstatus_5,employmenttype_jobstatus_6,employmenttype_jobstatus_7,employmenttype_jobstatus_8,employmenttype_jobstatus_9))
names(data1)[names(data1) == 'employmenttype_jobstatus_1'] <- 'employmenttype'
data1[is.na(data1$employmenttype) | data1$employmenttype == "-" | data1$employmenttype == "BASED ON EXPERIENCE"]$employmenttype <- "missing"
data1$employmenttype <- droplevels(data1$employmenttype)

data1 <- cSplit(as.data.table(data1), "shift", "|")
names(data1)[names(data1) == 'shift_1'] <- 'telecommuting'
names(data1)[names(data1) == 'shift_2'] <- 'travel'
data1[is.na(data1$telecommuting)]$telecommuting <- "missing"
data1[is.na(data1$travel)]$travel <- "missing"

library(tm)
library(stringr)

data1$jobdescription <- gsub("[^[:alnum:][:blank:]+?&/\\-]", " ", data1$jobdescription)
stopwords_regex = paste(c(stopwords('en'),stopwords('SMART'),"â","Â","years","experience","development","software","develop"), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
data1$jobdescription = stringr::str_replace_all(data1$jobdescription, stopwords_regex, ' ')

summary(data1$company)
summary(data1$travel)

#Export For Insights
write.csv(data1, "final_everything.csv")

summary(data1$state)


