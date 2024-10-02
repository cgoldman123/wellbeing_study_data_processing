## Purpose: Prolific survey data scoring

## Import libraries ------------------------------------------------------------
library(tidyverse)  # Keep script tidy
library(glue)       # String formatting throughout
library(pbapply)    # Progress bars for *apply functions
library(readr)
library(magrittr)
library(qwraps2)
library(reshape2)
library(data.table)
library(car)
library(psych)
library(dplyr)

## Utilities -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
rm(list = ls())
select=dplyr::select
rename=dplyr::rename
mutate=dplyr::mutate
`%!in%` = Negate(`%in%`)

## Lists -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
ignore.ids <- c('CLTEST', 'CGTEST','CMGTEST1', 'test', 
                'AR_TEST', 'AR111','temp', 'm4bGK7uedSWACG')
survey.list = c('crt_7')

## Functions -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
extract.survey <- function(survey_str){
  scores <- data.frame()
  folder = glue('L:/NPC/DataSink/StimTool_Online/WB_Blind_Dating')
  s.files = list.files(path = folder, pattern = glue('{survey_str}_[0-9]'), full.names = T)
  s.files = s.files[!grepl(paste(ignore.ids, collapse='|'), s.files, ignore.case = T)]
  for(ff in s.files){
    sub = gsub(glue('.*{survey_str}_(.+)_T[1-2].*'), '\\1',ff)
    time = gsub(glue('.*{survey_str}_{sub}_T[1-2]_(.+).*.csv'), '\\1',ff)
    timename = glue('{toupper(survey_str)}_time')
    if(sub %in% scores$id){
      ogtime <- scores %>% filter(id==sub) %>% pull(timename)
      if(difftime(as.POSIXct(time, format='%Y_%m_%d_%H_%M', tz='UTC'), as.POSIXct(ogtime, format='%Y_%m_%d_%H_%M',tz='UTC'))>0){
        next
      }else{
        scores <- scores %>% filter(id!=sub)
      }
    }
    
    if(survey_str=='ctq'&difftime(as.POSIXct(time, format='%Y_%m_%d_%H_%M', tz='UTC'), as.POSIXct('2024_06_06_2_24', format='%Y_%m_%d_%H_%M', tz='UTC'))<0){
      str_ex='_old'
    }else{str_ex=''}
    
    df = read.csv(ff, header=F)
    surv_func <- glue('{survey_str}_score{str_ex}')
    
    scores <- do.call(surv_func, args = list(data=df)) %>%
      mutate(id=sub) %>%
      mutate('{timename}':=time) %>%
      rbind(scores,.)
    rm(sub,time,timename,df)
  }
  
  return(scores)
}
merge.scores <- function(full.df, new.df){
  commoncols = intersect(names(full.df),names(new.df))
  final <- merge(full.df, new.df, by=commoncols, all=T)
  return(final)
}  


# Function to generate the all_numbers_pattern
generate_all_numbers_pattern <- function(correct_answers) {
  decimals <- sprintf("\\.%02d", 0:99)  # Generates strings like ".00", ".01", ..., ".99"
  numbers <- as.character(1:99)
  basic_numbers <- c(
    "zero","one","two","three","four","five","six","seven","eight","nine",
    "ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen",
    "seventeen","eighteen","nineteen"
  )
  tens <- c("twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety")
  tens_units <- unlist(lapply(tens, function(t) {
    units <- c("", " one", " two", " three", " four", " five", " six", " seven", " eight", " nine")
    comb <- paste0(t, units)
    c(comb, gsub(" ", "-", comb))
  }))
  number_words <- c(basic_numbers, tens_units)
  number_words <- trimws(number_words)
  all_numbers <- c(decimals, numbers, number_words)
  # Escape special regex characters in correct answers
  all_numbers_excl_correct <- setdiff(all_numbers, correct_answers)
  # Create the pattern
  all_numbers_pattern <- paste0("\\b(", paste(all_numbers_excl_correct, collapse = "|"), ")\\b")
  
  return(all_numbers_pattern)
}

crt_score <- function(data, correct_answers) {
  # Generate the pattern excluding correct answers
  incorrect_answers <- generate_all_numbers_pattern(correct_answers)
  # Escape correct answers for the acceptance pattern
  correct_pattern <- paste0("\\b(", paste(correct_answers, collapse = "|"), ")\\b")
  # Filter and score
  q_cor <- ifelse(
        grepl(correct_pattern, data, ignore.case = TRUE) & 
          !grepl(incorrect_answers, data, ignore.case = TRUE), 
        1, 0)
  
  return(q_cor)
}
data = read.csv("L:/NPC/DataSink/StimTool_Online/WB_Blind_Dating/crt_7_669d7a56135e5944f27496f1_T1_2024_08_03_23_43.csv",header=F)

crt_7_score <- function(data){
  q1_value <- data %>% 
    filter(V1 %in% c('question1')) %>% 
    pull(V2)
  q2_value <- data %>% 
    filter(V1 %in% c('question2')) %>% 
    pull(V2)
  q3_value <- data %>% 
    filter(V1 %in% c('question3')) %>% 
    pull(V2)
  q4_value <- data %>% 
    filter(V1 %in% c('question4')) %>% 
    pull(V2)
  q5_value <- data %>% 
    filter(V1 %in% c('question5')) %>% 
    pull(V2)
  q6_value <- data %>% 
    filter(V1 %in% c('question6')) %>% 
    pull(V2)
  q7_value <- data %>% 
    filter(V1 %in% c('question7')) %>% 
    pull(V2)  
  
  q1_cor = crt_score(q1_value, c("5", "five", "\\.05", "\\0.05"))
  q2_cor = crt_score(q2_value, c("5", "five"))  
  q3_cor = crt_score(q3_value, c("47", "forty-seven","forty seven"))  
  q4_cor = crt_score(q4_value, c("4", "four"))  
  q5_cor = crt_score(q5_value, c("29", "twenty nine", "twenty-nine"))
  q6_cor = crt_score(q6_value, c("twenty", "20"))  
  q7_cor = as.numeric(q7_value == "Item 3")

  q1_intuit = crt_score(q1_value, c("10", "ten"))
  q2_intuit = crt_score(q2_value, c("100", "one hundred", "hundred", "hundered"))  
  q3_intuit = crt_score(q3_value, c("24", "twenty-four","twenty four"))  
  q4_intuit = crt_score(q4_value, c("9", "nine"))  
  q5_intuit = crt_score(q5_value, c("30", "thirty"))
  q6_intuit = crt_score(q6_value, c("10", "ten"))  
  q7_intuit = as.numeric(q7_value == "Item 2")

  final <- data.frame(q1_value, q2_value, q3_value, q4_value, q5_value, q6_value, q7_value,
                      q1_cor, q2_cor, q3_cor, q4_cor, q5_cor, q6_cor, q7_cor,
                      q1_intuit, q2_intuit, q3_intuit, q4_intuit, q5_intuit, q6_intuit, q7_intuit)
  return(final)
  
}
crt_7_score(data)

## Main -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
full.data <- data.frame(id=NA)
i=1
for(s in survey.list){
  tryCatch(
    {
      full.data <- extract.survey(s) %>%
        merge.scores(full.data, .) %>%
        filter(!is.na(id))
      print(glue('{100*round(i/length(survey.list),2)}% done'))
      i=i+1
    },
    error=function(e){
      print(glue('The following problem occurred within the {s} function:\n {e}'))
    }
  )
}

# make sure someone is never given correct and intuitive answer
full.data <- full.data %>%
  mutate(
    q1_same = q1_cor & q1_intuit,
    q2_same = q2_cor & q2_intuit,
    q3_same = q3_cor & q3_intuit,
    q4_same = q4_cor & q4_intuit,
    q5_same = q5_cor & q5_intuit,
    q6_same = q6_cor & q6_intuit,
    q7_same = q7_cor & q7_intuit
  )

# write.csv(full.data,glue('L:/rsmith/wellbeing/data/prolific/crt_data.csv'), row.names=F)

