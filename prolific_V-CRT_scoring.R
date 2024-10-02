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
ignore.ids <- c('CLTEST', 'CGTEST','CMGTEST1', 'test','FENTTEST', 
                'AR_TEST', 'AR111','temp', 'm4bGK7uedSWACG')
survey.list = c('v_crt')

## Functions -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
extract.survey <- function(survey_str){
  scores <- data.frame()
  folder = glue('L:/NPC/DataSink/StimTool_Online/WB_Advice')
  advice.files = list.files(path = folder, pattern = glue('v_crt'), full.names = T)
  advice.files = advice.files[!grepl(paste(ignore.ids, collapse='|'), advice.files, ignore.case = T)]
  
  folder = glue('L:/NPC/DataSink/StimTool_Online/WB_Social_Media')
  social.files = list.files(path = folder, pattern = glue('v_crt'), full.names = T) 
  social.files = social.files[!grepl(paste(ignore.ids, collapse='|'), social.files, ignore.case = T)]
  
  folder = glue('L:/NPC/DataSink/StimTool_Online/WB_Social_Media_CB')
  social_cb.files = list.files(path = folder, pattern = glue('v_crt'), full.names = T) 
  social_cb.files = social_cb.files[!grepl(paste(ignore.ids, collapse='|'), social_cb.files, ignore.case = T)]
  
  s.files = c(advice.files,social.files, social_cb.files)
  for(ff in s.files){
    sub = gsub(glue('.*v_crt_(.+)_T[1-2].*'), '\\1',ff)
    time = gsub(glue('.*v_crt_{sub}_T[1-2]_(.+).*.csv'), '\\1',ff)
    timename = glue('VCRT_time')
    if(sub %in% scores$id){
      ogtime <- scores %>% filter(id==sub) %>% pull(timename) %>% .[1]
      if(difftime(as.POSIXct(time, format='%Y_%m_%d_%H_%M', tz='UTC'), as.POSIXct(ogtime, format='%Y_%m_%d_%H_%M',tz='UTC'))>0){
        next
      }else{
        scores <- scores %>% filter(id!=sub)
      }
    }
    

    
    df = read.csv(ff, header=F)
    surv_func <- glue('{survey_str}_score')
    
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





v_crt_score <- function(data){
  
  data_filtered = data %>% filter(V1 != "TRQ0")
  return(data_filtered)
  
}

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

full_data = full.data %>% select(c(id,VCRT_time,V1,V2))


# write.csv(full_data,glue('L:/rsmith/wellbeing/data/prolific/v_crt_raw_data.csv'), row.names=F)

