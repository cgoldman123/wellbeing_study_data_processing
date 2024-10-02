## Purpose: Combine LEAS responses for scoring

## Import libraries ------------------------------------------------------------
library(tidyverse)  # Keep script tidy
library(glue)       # String formatting throughout
library(pbapply)    # Progress bars for *apply functions
library(readr)
library(magrittr)
library(ggplot2)
library(qwraps2)
library(reshape2)
library(data.table)
library(car)
library(psych)
library(lme4)
library(dplyr)


## Clear workspace -------------------------------------------------------------
rm(list = ls())

ignore.ids <- c('CLTEST', 'CGTEST','CMGTEST1', 'test', 
                'AR_TEST', 'AR111','temp', 'm4bGK7uedSWACG')

l.files <- c(list.files(path='L:/NPC/DataSink/StimTool_Online/WB_Emotional_Faces', pattern='leas_',full.names = T),
             list.files(path='L:/NPC/DataSink/StimTool_Online/WB_Emotional_Faces_test', pattern='leas_',full.names = T),
  list.files(path='L:/NPC/DataSink/StimTool_Online/WB_Emotional_Faces_CB', pattern='leas_',full.names = T),
  list.files(path='L:/NPC/DataSink/StimTool_Online/WB_Cooperation_Task', pattern='leas_',full.names = T))

l.files = l.files[!grepl(paste(ignore.ids, collapse='|'), l.files, ignore.case = T)]


extract_datetime <- function(file) {
  # Use regular expression to extract the date and time part
  datetime_string <- gsub(".*_T[1-2]_(\\d{4}_\\d{2}_\\d{2}_(\\d{1,2})_(\\d{1,2}))\\.csv", "\\1", file)
  # Pad the hours and minutes with leading zeros if necessary
  datetime_string <- gsub("_(\\d{1})_", "_0\\1_", datetime_string)
  datetime_string <- gsub("_(\\d{1})$", "_0\\1", datetime_string)
  # Convert the extracted string to a datetime object
  as.POSIXct(datetime_string, format="%Y_%m_%d_%H_%M")
}

# Apply the function to extract datetime and reorder the files
ordered_files <- l.files[order(sapply(l.files, extract_datetime))]

leas.q.dict <- list('question1'='question1_self', 'question2'='question1_other',
                    'question3'='question2_self', 'question4'='question2_other',
                    'question5'='question3_self', 'question6'='question3_other',
                    'question7'='question4_self', 'question8'='question4_other',
                    'question9'='question5_self', 'question10'='question5_other',
                    'question11'='question6_self', 'question12'='question6_other',
                    'question13'='question7_self', 'question14'='question7_other',
                    'question15'='question8_self', 'question16'='question8_other',
                    'question17'='question9_self', 'question18'='question9_other',
                    'question19'='question10_self', 'question20'='question10_other')

final = data.frame()
for(sub in ordered_files){
  pid <- gsub(glue('.*leas_(.+)_T[1-2].*'), '\\1',sub)
  time = gsub(glue('.*leas_{pid}_T[1-2]_(.+).*.csv'), '\\1',sub)
  timename = glue('LEAS_time')
  
  

  
  
  
  
  data <- read.csv(sub, header=F) %>%
    filter(!grepl('TRQ', V1))
  pivoted_df <- pivot_wider(data, names_from = V1, values_from = V2)
  names(pivoted_df) <- unlist(leas.q.dict[names(pivoted_df)])
  pivoted_df$id = pid
  pivoted_df$time = time
  pivoted_df = pivoted_df %>% select(c(id, time, everything()))
  


  final <- rbind(final,pivoted_df)
    
}

write.csv(final,'L:/rsmith/wellbeing/data/prolific/leas/LEAS_data_prolific_raw.csv')











# 
# 
# 
# 
# for(sub in ordered_files){
#   pid <- gsub(glue('.*leas_(.+)_T[1-2].*'), '\\1',sub)
#   time = gsub(glue('.*leas_{pid}_T[1-2]_(.+).*.csv'), '\\1',sub)
#   timename = glue('LEAS_time')
# 
#   
#   leas.q.dict <- list('question1'='self_hammer', 'question2'='other_hammer',
#                       'question3'='self_backrub', 'question4'='other_backrub',
#                       'question5'='self_bridge', 'question6'='other_bridge',
#                       'question7'='self_boss', 'question8'='other_boss',
#                       'question9'='self_bank', 'question10'='other_bank',
#                       'question11'='self_excellentwork', 'question12'='other_excellentwork',
#                       'question13'='self_dentist', 'question14'='other_dentist',
#                       'question15'='self_pizza', 'question16'='other_pizza',
#                       'question17'='self_business', 'question18'='other_business',
#                       'question19'='self_money', 'question20'='other_money')
# 
#   
#   data <- read.csv(sub, header=F) %>%
#     filter(!grepl('TRQ', V1)) %>% 
#     rowwise %>% 
#     mutate(V1 = leas.q.dict[V1][[1]])
#   self.qs <- data %>% filter(grepl('self',V1))
#   other.qs <- data %>% filter(grepl('other',V1))
#   
#     for(ques in c('self.qs','other.qs')){
#       type=gsub('(.+).qs', replacement = '\\1', ques)
#       filetoadd <- glue('L:/rsmith/wellbeing/data/prolific/leas/LEAS_{type}_responses.csv')
#       ques=get(ques) %>%
#         as.data.frame %>%
#         rename(V2='V1', V3='V2') %>%
#         mutate(V1 = pid) %>%
#         relocate(V1, .before=everything())
#       
#       scores <- read.csv(filetoadd, header=F)
#       if(pid %in% scores$V1){
#         next
#       }else{
#         final <- rbind(scores,ques)
#         final[1,1] <- final$V1 %>% unique %>% length-1
#         write.table(final, filetoadd, sep=",", row.names = F, col.names=F)
#       }
#     }
# }

## Once you put the two responses .csvs into eleastest.net scoring!!!===========
# save them as Other_scores.csv and Self_scores.csv
# o.scores <- read.csv('L:/rsmith/wellbeing/data/prolific/leas/Other_scores.csv')
# 
# df <- data.frame()
# for(id in unique(o.scores$Person)){
#   Other <- o.scores %>% 
#     filter(Person==id) %>%
#     pull(UniqueMaximums) %>% 
#     sum
#   df <- rbind(df, data.frame(id, Other))
# }



