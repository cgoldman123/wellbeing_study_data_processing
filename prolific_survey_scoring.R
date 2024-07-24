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
survey.list = c('ctq','qes','panasx','ryff_wb','sticsa_state',
                'sticsa_trait','well_being','dpes','hils','his','upps_p',
                'indcol', 'mhs', 'pdsq','stait_hope', 'trait_hope','swls',
                'asi', 'cit', 'dast_10', 'dts', 'lot_r', 'maia', 'mlq', 'crt_7',
                'ncs_short', 'oasis', 'pcl_5', 'phq_8', 'shs', 'tas', 
                'vps', 'wolf', 'gfi', 'bfi', 'fss', 'sbi', 'stai_state', 'stai_trait',
                'svs','teps', 'vhs', 'whodas','dfas', 'bis_bas', 'promis_emotion',
                'promis_meaning', 'promis_sleep', 'promis_social_iso',
                'promis_social_sat','promis_self_efficacy','promis_self_efficacy_manage',
                'csass_oecd', 'health_questions')
## Functions -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
extract.survey <- function(survey_str){
  scores <- data.frame()
  for(study in c('Emotional_Faces', 'Advice', 'Blind_Dating', 'Social_Media', 'Cooperation_Task',
                 'Emotional_Faces_CB', 'Social_Media_CB')){
    folder = glue('L:/NPC/DataSink/StimTool_Online/WB_{study}')
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
  }
  return(scores)
}
merge.scores <- function(full.df, new.df){
  commoncols = intersect(names(full.df),names(new.df))
  final <- merge(full.df, new.df, by=commoncols, all=T)
  return(final)
}  

ctq_score <- function(data){
  CTQ_emoabuse = data %>% 
    filter(V1 %in% c('question3', 'question8', 'question14', 'question17', 'question24')) %>% 
    pull(V2) %>% sum
  CTQ_emoneglect = data %>% 
    filter(V1 %in% c('question5', 'question7', 'question13', 'question18', 'question28')) %>%
    mutate(V2 = 6-V2) %>% 
    pull(V2) %>% sum
  CTQ_physabuse = data %>% 
    filter(V1 %in% c('question9', 'question10', 'question11', 'question15', 'question16')) %>% 
    pull(V2) %>% sum
  CTQ_physneglect = data %>% 
    filter(V1 %in% c('question1', 'question2', 'question4', 'question6', 'question25')) %>% 
    mutate(V2 = ifelse(V1 %in% c('question2', 'question25'),6-V2,V2)) %>%
    pull(V2) %>% sum
  CTQ_sexabuse = data %>% 
      filter(V1 %in% c('question19', 'question20', 'question22', 'question23', 'question27')) %>% 
      pull(V2) %>% sum
  CTQ_denial = data %>% 
    filter(V1 %in% c('question12', 'question21', 'question26')) %>% 
    pull(V2) %>% sum
  CTQ_total <- CTQ_emoneglect+CTQ_emoabuse+CTQ_physabuse+CTQ_physneglect+CTQ_sexabuse
  
  final <- data.frame(CTQ_emoabuse, CTQ_emoneglect, CTQ_physabuse, 
                      CTQ_physneglect, CTQ_sexabuse, CTQ_denial,CTQ_total)
  return(final)
    
}
ctq_score_old <- function(data){
  data = data %>% filter(V1!='question14')
  CTQ_emoabuse = data %>% 
    filter(V1 %in% c('question3', 'question8', 'question17', 'question24')) %>% 
    pull(V2) %>% sum
  CTQ_emoneglect = data %>% 
    filter(V1 %in% c('question5', 'question7', 'question13', 'question18', 'question28')) %>%
    mutate(V2 = 6-V2) %>% 
    pull(V2) %>% sum
  CTQ_physabuse = data %>% 
    filter(V1 %in% c('question9', 'question10', 'question11', 'question15', 'question16')) %>% 
    pull(V2) %>% sum
  CTQ_physneglect = data %>% 
    filter(V1 %in% c('question1', 'question2', 'question4', 'question6', 'question25')) %>% 
    mutate(V2 = ifelse(V1 %in% c('question2', 'question25'),6-V2,V2)) %>%
    pull(V2) %>% sum
  CTQ_sexabuse = data %>% 
    filter(V1 %in% c('question19', 'question20', 'question22', 'question23', 'question27')) %>% 
    pull(V2) %>% sum
  CTQ_denial = data %>% 
    filter(V1 %in% c('question12', 'question21', 'question26')) %>% 
    pull(V2) %>% sum
  CTQ_total <- CTQ_emoneglect+CTQ_emoabuse+CTQ_physabuse+CTQ_physneglect+CTQ_sexabuse
  
  final <- data.frame(CTQ_emoabuse, CTQ_emoneglect, CTQ_physabuse, 
                      CTQ_physneglect, CTQ_sexabuse, CTQ_denial,CTQ_total)
  return(final)
  
}
qes_score <- function(data){
  QES_attncheck1 <- data %>% filter(V1=='question_check') %>% pull(V2)=='Item 2'
  QES_total <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    mutate(V2 = ifelse(V1 %in% c('question2','question6','question10','question11','question14'), 6-V2,V2)) %>%
    pull(V2) %>% sum
  final <- data.frame(QES_attncheck1, QES_total)
  return(final)
}
panasx_score <- function(data){
  PANAS_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)=='Item 4'
  PANAS_attncheck2 <- data %>% filter(V1=='question_check2') %>% pull(V2)=='Item 4'
  
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))-1)
  
  PANAS_negaffect <- data %>%
    filter(grepl(paste(paste0('question', c(11,18,26,32,34,
                                            38,40,42,44,50), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  PANAS_posaffect <- data %>%
    filter(grepl(paste(paste0('question', c(3,8,13,25,31,37,
                                            39,47,52,55), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  PANAS_fear <- data %>%
    filter(grepl(paste(paste0('question', c(18,21,34,40,44,53), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  PANAS_hostility <- data %>%
    filter(grepl(paste(paste0('question', c(2,9,11,27,38,56), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  PANAS_guilt <- data %>%
    filter(grepl(paste(paste0('question', c(15,32,42,46,51,60), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  PANAS_sadness <- data %>%
    filter(grepl(paste(paste0('question', c(16,24,29,35,48), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  PANAS_joviality <- data %>%
    filter(grepl(paste(paste0('question', c(1,12,22,33,37,41,47,58), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  PANAS_selfassure <- data %>%
    filter(grepl(paste(paste0('question', c(6,8,14,28,39,57), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  PANAS_attentive <- data %>%
    filter(grepl(paste(paste0('question', c(3,25,52,59), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  PANAS_shyness <- data %>%
    filter(grepl(paste(paste0('question', c(4,23,30,49), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  PANAS_fatigue <- data %>%
    filter(grepl(paste(paste0('question', c(5,19,36,45), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  PANAS_serenity <- data %>%
    filter(grepl(paste(paste0('question', c(10,17,43), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  PANAS_surprise <- data %>%
    filter(grepl(paste(paste0('question', c(7,20,54), '_'), collapse='|'),
                 V1)) %>%
    pull(V2) %>% sum
  
  final <- data.frame(PANAS_attncheck1, PANAS_attncheck2,PANAS_negaffect,
                      PANAS_posaffect,PANAS_fear,PANAS_hostility,PANAS_guilt,
                      PANAS_sadness,PANAS_joviality,PANAS_selfassure,PANAS_attentive,
                      PANAS_shyness,PANAS_fatigue,PANAS_serenity,PANAS_surprise)
  return(final)
  
}
ryff_wb_score <- function(data){
  ryff_attncheck1 <- data %>% filter(V1=='question16') %>% pull(V2)=='Item 6'
  ryff_attncheck2 <- data %>% filter(V1=='question30') %>% pull(V2)=='Item 6'
  
  # 1=disagree and 7=agree online therefore I reverse-scored the regular items 
  # and left the reverse-coded items alone
  data <- data %>% filter(!grepl('check', V1)) %>%
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    mutate(V2 = ifelse(V1 %in% paste0('question', c(1,2,3,4,6,7,11,13,
                                                    18,21,22,23,24,28,
                                                    30,33,37,38,39,40,42)), 
                       V2,8-V2))
  RYFF_autonomy <- data %>%
    filter(V1 %in% c('question1','question13','question25','question37',
                     'question43','question10','question22')) %>%
    pull(V2) %>% sum
  RYFF_envmastery <- data %>%
    filter(V1 %in% c('question3','question15','question27','question38',
                     'question44','question12','question24')) %>%
    pull(V2) %>% sum
  RYFF_persgrowth <- data %>%
    filter(V1 %in% c('question5','question18','question29','question39',
                     'question2','question14','question26')) %>%
    pull(V2) %>% sum
  RYFF_posrelations <- data %>%
    filter(V1 %in% c('question7','question19','question32','question40',
                     'question4','question17','question28')) %>%
    pull(V2) %>% sum
  RYFF_purposelife <- data %>%
    filter(V1 %in% c('question9','question21','question34','question41',
                     'question6','question30','question35')) %>%
    pull(V2) %>% sum
  RYFF_selfaccept <- data %>%
    filter(V1 %in% c('question11','question23','question36','question42',
                     'question8','question20','question33')) %>%
    pull(V2) %>% sum
  final <- data.frame(ryff_attncheck1, ryff_attncheck2,RYFF_autonomy, RYFF_envmastery,RYFF_persgrowth,
                      RYFF_posrelations,RYFF_purposelife,RYFF_selfaccept)
  return(final)
}
sticsa_state_score <- function(data){
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  STICSA_s_somatic = data %>% 
    filter(V1 %in% paste0('question', c(1,2,6,7,8,12,14,15,18,20,21))) %>%
    pull(V2) %>% sum/11
  STICSA_s_cognitive = data %>% 
    filter(V1 %in% paste0('question', c(3,4,5,9,10,11,13,16,17,19))) %>%
    pull(V2) %>% sum/10
  final <- data.frame(STICSA_s_somatic, STICSA_s_cognitive)
  return(final)
}
sticsa_trait_score <- function(data){
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  STICSA_t_somatic = data %>% 
    filter(V1 %in% paste0('question', c(1,2,6,7,8,12,14,15,18,20,21))) %>%
    pull(V2) %>% sum/11
  STICSA_t_cognitive = data %>% 
    filter(V1 %in% paste0('question', c(3,4,5,9,10,11,13,16,17,19))) %>%
    pull(V2) %>% sum/10
  
  final <- data.frame(STICSA_t_somatic, STICSA_t_cognitive)
  return(final)
}
well_being_score <- function(data){
  WB_attncheck1 <- data %>% filter(V1=='question41_attention3') %>% pull(V2)=='Item 4'
  WB_attncheck2 <- data %>% filter(V1=='question42_attention9') %>% pull(V2)=='Item 10'
  
  data <- data %>% filter(!grepl('attention', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))-1) %>%
    mutate(V2 = ifelse(grepl(paste(paste0('question', c(5,6,12,30,36,40)), collapse='|'),V1), 10-V2,V2))
  
  WB_emohealth <- data %>%
    filter(grepl(paste(paste0('question', 1:7, '_'), collapse='|'),V1)) %>%
    pull(V2) %>% sum/7
  WB_physhealth <- data %>%
    filter(grepl(paste(paste0('question', 8:14, '_'), collapse='|'),V1)) %>%
    pull(V2) %>% sum/7
  WB_meanpurpose <- data %>%
    filter(grepl(paste(paste0('question', 15:20), collapse='|'),V1)) %>%
    pull(V2) %>% sum/6
  WB_charstrengths <- data %>%
    filter(grepl(paste(paste0('question', 21:27), collapse='|'),V1)) %>%
    pull(V2) %>% sum/7
  WB_socialconnect <- data %>%
    filter(grepl(paste(paste0('question', 28:34), collapse='|'),V1)) %>%
    pull(V2) %>% sum/7
  WB_financial <- data %>%
    filter(grepl(paste(paste0('question', 35:40), collapse='|'),V1)) %>%
    pull(V2) %>% sum/6
  WB_overall <- (WB_emohealth+WB_physhealth+WB_meanpurpose+WB_charstrengths+WB_socialconnect+WB_financial)/6
  
  final <- data.frame(WB_attncheck1, WB_attncheck2,WB_emohealth,WB_physhealth,
                      WB_meanpurpose,WB_charstrengths,WB_socialconnect,WB_financial,
                      WB_overall)
  return(final)
}
dpes_score <- function(data){
  #need to check to make sure it works
  DPES_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)=='Item 6'
  
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  
  DPES_joy <- data %>%
    filter(grepl(paste(paste0('question', 1:6), collapse='|'),V1)) %>%
    pull(V2) %>% sum/6
  DPES_content <- data %>%
    filter(grepl(paste(paste0('question', 7:11), collapse='|'),V1)) %>%
    pull(V2) %>% sum/5
  DPES_pride <- data %>%
    filter(grepl(paste(paste0('question', 12:16), collapse='|'),V1)) %>%
    pull(V2) %>% sum/5
  DPES_love <- data %>%
    filter(grepl(paste(paste0('question', 17:22), collapse='|'),V1)) %>%
    pull(V2) %>% sum/6
  DPES_compassion <- data %>%
    filter(grepl(paste(paste0('question', 23:27), collapse='|'),V1)) %>%
    pull(V2) %>% sum/5
  DPES_amusement <- data %>%
    filter(grepl(paste(paste0('question', 28:32), collapse='|'),V1)) %>%
    pull(V2) %>% sum/5
  DPES_awe <- data %>%
    filter(grepl(paste(paste0('question', 33:38), collapse='|'),V1)) %>%
    pull(V2) %>% sum/6
  
  final <- data.frame(DPES_attncheck1,DPES_joy,DPES_content,DPES_pride,DPES_love,
                      DPES_compassion,DPES_amusement,DPES_awe)
  return(final)
  #DPES_total <- data %>% filter(!grepl('check', V1)) %>% 
    #filter(!grepl('TRQ', V1)) %>%
    #mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    #pull(V2) %>% sum/38
  #final <- data.frame(DPES_attncheck1,DPES_total)
}
hils_score <- function(data){
  HILS_total <- data %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    pull(V2) %>% sum
  final <- data.frame(HILS_total)
  return(final)
}
his_score <- function(data){
  # The items for this survey don't match the real numbers! There are only 5 points on this scale
  # "Item 4" = 3, "Item 6" = 4, "Item 7" = 5
  IHS_total <- data %>% filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    mutate(V2 = case_when(V2==1~1, V2==2~2, V2==4~3,
                          V2==6~4, V2==7~5)) %>%
    pull(V2) %>% sum
  final <- data.frame(IHS_total)
  return(final)
  
}
indcol_score <- function(data){
  INDCOL_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)=='Item 8'
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) 
  
  INDCOL_hi <- data %>% filter(V1 %in% paste0('question', c(1,5,6,16,19,22,26,33))) %>%
    pull(V2) %>% sum
  INDCOL_vi <- data %>% filter(V1 %in% paste0('question', c(4,9,11,13,20,24,27,31))) %>%
    mutate(V2 = ifelse(V1=='question31', 10-V2, V2)) %>%
    pull(V2) %>% sum
  INDCOL_hc <- data %>% filter(V1 %in% paste0('question', c(2,10,12,15,17,21,23,29))) %>%
    pull(V2) %>% sum
  INDCOL_vc <- data %>% filter(V1 %in% paste0('question', c(3,7,14,18,25,28,30,32))) %>%
    pull(V2) %>% sum
  
  final <- data.frame(INDCOL_attncheck1,INDCOL_hi,INDCOL_vi,INDCOL_hc,INDCOL_vc)
  return(final)
}
mhs_score <- function(data){
  MHS_total <- data %>% filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    mutate(V2 = ifelse(V1 %in% c('question4','question7','question11'),6-V2,V2)) %>%
    pull(V2) %>% sum
  final <- data.frame(MHS_total)
  return(final)
}
pdsq_score <- function(data){
  PDSQ_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)==1
  PDSQ_attncheck2 <- data %>% filter(V1=='question_check2') %>% pull(V2)==1
  
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(V2))
  PDSQ_mdd <- data %>% filter(V1 %in% paste0('question', 1:15)) %>%
    pull(V2) %>% sum
  PDSQ_gad <- data %>% filter(V1 %in% paste0('question', 92:101)) %>%
    pull(V2) %>% sum
  PDSQ_ptsd <- data %>% filter(V1 %in% paste0('question', 22:36)) %>%
    mutate(criterionA = ifelse(V1=='question22'&V2==1|V1=='question23'&V2==1,T,F)) %>%
    mutate(V3 = ifelse(criterionA==T,V2,0)) %>%
    pull(V3) %>% sum
  PDSQ_bulimia <- data %>% filter(V1 %in% paste0('question', 37:46)) %>%
    pull(V2) %>% sum
  PDSQ_ocd <- data %>% filter(V1 %in% paste0('question', 47:53)) %>%
    pull(V2) %>% sum
  PDSQ_panic <- data %>% filter(V1 %in% paste0('question', 54:61)) %>%
    pull(V2) %>% sum
  PDSQ_psychosis <- data %>% filter(V1 %in% paste0('question', 62:67)) %>%
    pull(V2) %>% sum
  PDSQ_agoraphobia <- data %>% filter(V1 %in% c('question68','question69a','question69b',
                                                'question69c','question69d','question69e',
                                                'question69f','question69g','question69h',
                                                'question70', 'question71')) %>%
    pull(V2) %>% sum
  PDSQ_social <- data %>% filter(V1 %in% c('question72','question73','question74',
                                                'question75','question76','question77a',
                                                'question77b','question77c','question77d',
                                                'question77e', 'question77f','question77g',
                                           'question77h','question78','question79')) %>%
    pull(V2) %>% sum
  PDSQ_alcohol <- data %>% filter(V1 %in% paste0('question', 80:85)) %>%
    pull(V2) %>% sum
  PDSQ_drugs <- data %>% filter(V1 %in% paste0('question', 86:91)) %>%
    pull(V2) %>% sum
  PDSQ_somatization <- data %>% filter(V1 %in% paste0('question', 102:106)) %>%
    pull(V2) %>% sum
  PDSQ_hypochondria <- data %>% filter(V1 %in% paste0('question', 107:111)) %>%
    pull(V2) %>% sum
  
  final <- data.frame(PDSQ_attncheck1,PDSQ_attncheck2,PDSQ_mdd,PDSQ_ptsd,
                      PDSQ_bulimia,PDSQ_ocd,PDSQ_panic,
                      PDSQ_psychosis,PDSQ_agoraphobia,PDSQ_social,PDSQ_alcohol,
                      PDSQ_drugs,PDSQ_somatization,PDSQ_hypochondria)
  return(final)
}
stait_hope_score <- function(data){
  HOPE_s_pathways <- data %>% filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    filter(V1 %in% paste0('question', c(1,3,5))) %>%
    pull(V2) %>% sum
  HOPE_s_agency <- data %>% filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    filter(V1 %in% paste0('question', c(2,4,6))) %>%
    pull(V2) %>% sum
  HOPE_s_total <- HOPE_s_pathways + HOPE_s_agency
  final <- data.frame(HOPE_s_pathways,HOPE_s_agency,HOPE_s_total)
  return(final)
}
trait_hope_score <- function(data){
  HOPE_t_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)=='Item 4'

  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1))
  
  HOPE_t_pathways <- data %>% filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    filter(V1 %in% paste0('question', c(1,4,6,8))) %>%
    pull(V2) %>% sum
  HOPE_t_agency <- data %>% filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    filter(V1 %in% paste0('question', c(2,9,10,12))) %>%
    pull(V2) %>% sum
  HOPE_t_total <- HOPE_t_pathways + HOPE_t_agency
  
  final <- data.frame(HOPE_t_attncheck1,HOPE_t_pathways,HOPE_t_agency,HOPE_t_total)
  return(final)
}
swls_score <- function(data){
  # Note that this survey on prolific is coded backwards! 7=strongly agree but it appears in the 
  # first position so it gets coded as "Item 1". That's why I reverse-coded all items
  SWLS_total <- data %>% filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = 8-as.numeric(sub('.*Item ','', V2))) %>%
    pull(V2) %>% sum
  final <- data.frame(SWLS_total)
  return(final)
}
asi_score <- function(data){
  data <- data %>% filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  
  ASI_physical <- data %>% filter(V1 %in% paste0('question', c(3,4,7,8,12,15))) %>%
    pull(V2) %>% sum
  ASI_cognitive <- data %>% filter(V1 %in% paste0('question', c(2,5,10,14,16,18))) %>%
    pull(V2) %>% sum
  ASI_social <- data %>% filter(V1 %in% paste0('question', c(1,6,9,11,13,17))) %>%
    pull(V2) %>% sum
  ASI_total <- ASI_physical+ASI_cognitive+ASI_social
  
  final <- data.frame(ASI_physical,ASI_cognitive,ASI_social,ASI_total)
  return(final)
}
cit_score <- function(data){
  CIT_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)=='Item 1'
  CIT_attncheck2 <- data %>% filter(V1=='question_check2') %>% pull(V2)=='Item 5'
  
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  

  
  CIT_relationship_support = data %>%
    filter(V1 %in% paste0('question', 1:3)) %>%
    pull(V2) %>% sum
  CIT_relationship_community = data %>%
    filter(V1 %in% paste0('question', 4:6)) %>%
    pull(V2) %>% sum 
  CIT_relationship_trust = data %>%
    filter(V1 %in% paste0('question', 7:9)) %>%
    pull(V2) %>% sum
  CIT_relationship_respect = data %>%
    filter(V1 %in% paste0('question', 10:12)) %>%
    pull(V2) %>% sum
  CIT_lack_of_loneliness = data %>%
    filter(V1 %in% paste0('question', 13:15)) %>%
    mutate(V2=6-V2) %>%
    pull(V2) %>% sum
  CIT_relationship_belonging = data %>%
    filter(V1 %in% paste0('question', 16:18)) %>%
    pull(V2) %>% sum
  CIT_relationship <- data %>%
    filter(V1 %in% paste0('question', 1:18)) %>%
    pull(V2) %>% sum
  
  CIT_engagement <- data %>%
    filter(V1 %in% paste0('question', 19:21)) %>%
    pull(V2) %>% sum
  
  
  CIT_mastery_skills <- data %>%
    filter(V1 %in% paste0('question', 22:24)) %>%
    pull(V2) %>% sum
  CIT_mastery_learning <- data %>%
    filter(V1 %in% paste0('question', 25:27)) %>%
    pull(V2) %>% sum
  CIT_mastery_accomplishment <- data %>%
    filter(V1 %in% paste0('question', 28:30)) %>%
    pull(V2) %>% sum
  CIT_mastery_self_efficacy <- data %>%
    filter(V1 %in% paste0('question', 31:33)) %>%
    pull(V2) %>% sum
  CIT_mastery_self_worth <- data %>%
    filter(V1 %in% paste0('question', 34:36)) %>%
    pull(V2) %>% sum
  CIT_mastery <- data %>%
    filter(V1 %in% paste0('question', 22:36)) %>%
    pull(V2) %>% sum
  
  CIT_autonomy <- data %>%
    filter(V1 %in% paste0('question', 37:39)) %>%
    mutate(V2=6-V2) %>%
    pull(V2) %>% sum
  
  CIT_meaning <- data %>%
    filter(V1 %in% paste0('question', 40:42)) %>%
    pull(V2) %>% sum
  
  CIT_optimism <- data %>%
    filter(V1 %in% paste0('question', 43:45)) %>%
    pull(V2) %>% sum
  
  CIT_swb_life_satisfaction <- data %>%
    filter(V1 %in% paste0('question', 46:48)) %>%
    pull(V2) %>% sum
  CIT_swb_pos_feelings <- data %>%
    filter(V1 %in% paste0('question', 49:51)) %>%
    pull(V2) %>% sum
  CIT_swb_non_neg_feelings <- data %>%
    filter(V1 %in% paste0('question', 52:54)) %>%
    mutate(V2 = ifelse(V1 %in% paste0('question', 52:54),6-V2,V2)) %>%
    pull(V2) %>% sum
  CIT_swb <- data %>%
    filter(V1 %in% paste0('question', 46:54)) %>%
    mutate(V2 = ifelse(V1 %in% paste0('question', 52:54),6-V2,V2)) %>%
    pull(V2) %>% sum
  
  
  CIT_total <- data %>% 
    mutate(V2 = ifelse(V1 %in% paste0('question', c(13,14,15, 37,38,39,52,53,54)),6-V2,V2)) %>%
    pull(V2) %>% sum
  
  
  
  final <- data.frame(CIT_attncheck1, CIT_attncheck2, CIT_relationship_support,
                      CIT_relationship_community, CIT_relationship_trust, CIT_relationship_respect,
                      CIT_lack_of_loneliness, CIT_relationship_belonging, CIT_relationship,
                      CIT_engagement, CIT_mastery_skills, CIT_mastery_learning, CIT_mastery_accomplishment,
                      CIT_mastery_self_efficacy, CIT_mastery_self_worth, CIT_mastery, CIT_autonomy, CIT_meaning,
                      CIT_optimism, CIT_swb_life_satisfaction, CIT_swb_pos_feelings,
                      CIT_swb_non_neg_feelings, CIT_swb, CIT_total)
  return(final)
  
}
crt_7_score <- function(data){
  data <- data %>% filter(!grepl('TRQ', V1)) %>%
    mutate(response = case_when(V1=='question1'&V2==5~2,V1=='question1'&V2==10~1,
                                V1=='question2'&V2==5~2,V1=='question2'&V2==100~1,
                                V1=='question3'&V2==47~2,V1=='question3'&V2==24~1,
                                V1=='question4'&V2==4~2, V1=='question4'&V2==9~1,
                                V1=='question5'&V2==29~2, V1=='question5'&V2==30~1,
                                V1=='question6'&V2==20~2,V1=='question6'&V2==10~1,
                                V1=='question7'&V2=='Item 3'~2, V1=='question7'&V2=='Item 2'~1)) %>%
    mutate(response=ifelse(is.na(response),0,response))
  CRT_correct_answers <- sum(data$response==2)
  CRT_intuitive_answers <- sum(data$response==1)
  CRT_incorrect_answers <- sum(data$response==0)
  
  return(data.frame(CRT_correct_answers,CRT_intuitive_answers,CRT_incorrect_answers))
}
v_crt_score <- function(data){
  data <- data %>% filter(!grepl('TRQ', V1)) %>%
    mutate(response = case_when(V1=='question1'&grepl('Mary',V2,ignore.case=T)~2,V1=='question1'&grepl('Nunu',V2,ignore.case=T)~1,
                                V1=='question2'&grepl('second|2nd|2',V2,ignore.case=T)~2,V1=='question2'&grepl('first|1st|1',V2,ignore.case=T)~1,
                                V1=='question3'&grepl('do not bury|nowhere|alive|not buried',V2,ignore.case=T)~2,V1=='question3'&grepl('USA|united states|us',V2,ignore.case=T)~1,
                                V1=='question4'&grepl("none|no banana|coconut tree",V2,ignore.case=T)~2, V1=='question4'&grepl('bird',V2,ignore.case=T)~1,
                                V1=='question5'&grepl('one floor|one-story|no stairs',V2,ignore.case=T)~2, V1=='question5'&grepl('pink',V2,ignore.case=T)~1,
                                V1=='question6'&grepl("none|noah|moses didn't|0",V2,ignore.case=T)~2,V1=='question6'&grepl('2|two',V2,ignore.case=T)~1,
                                V1=='question7'&grepl("no smoke|train does not|train doesn't|trains don't",V2,ignore.case=T)~2, V1=='question7'&grepl('west',V2,ignore.case=T)~1,
                                V1=='question8'&grepl("match",V2,ignore.case=T)~2, V1=='question8'&grepl('lamp',V2,ignore.case=T)~1,
                                V1=='question9'&grepl("not possible|dead",V2,ignore.case=T)~2, V1=='question9'&V2=='no'~1,
                                V1=='question10'&grepl("neither|yellow|not white",V2,ignore.case=T)~2, V1=='question10'&grepl("b|yolk is white",V2,ignore.case=T)~1)) %>%
    mutate(response=ifelse(is.na(response),0,response))
  VCRT_correct_answers <- sum(data$response==2)
  VCRT_intuitive_answers <- sum(data$response==1)
  VCRT_incorrect_answers <- sum(data$response==0)
  
  return(data.frame(VCRT_correct_answers,VCRT_intuitive_answers,VCRT_incorrect_answers))
} # not included because tricky to score
dast_10_score <- function(data){
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = 2 - as.numeric(sub('.*Item ','', V2)))
  
  DAST_total <- data %>% 
    mutate(V2 = ifelse(V1 == 'question3',1-V2,V2)) %>%
    pull(V2) %>% sum
  
  final <- data.frame(DAST_total)
  return(final)
  
} 
dts_score <- function(data){
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  
  DTS_total <- data %>% pull(V2) %>% sum
  final <- data.frame(DTS_total)
  return(final)
}
lot_r_score <- function(data){
  # supposed to be 0-4 not 1-5
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))-1)
  
  LOT_total <- data %>% 
    mutate(V2 = ifelse(V1 %in% paste0('question', c(3,7,9)),4-V2,V2)) %>%
    pull(V2) %>% sum
  final <- data.frame(LOT_total)
  return(final)
}
maia_score <- function(data){
  MAIA_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)==0
  
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(V2))
  
  MAIA_noticing <- data %>% 
    filter(V1 %in% paste0('question', 1:4)) %>%
    pull(V2) %>% sum/4
  MAIA_distracting <- data %>% 
    filter(V1 %in% paste0('question', 5:7)) %>%
    mutate(V2 = 6-V2) %>%
    pull(V2) %>% sum/3
  MAIA_notworrying <- data %>% 
    filter(V1 %in% paste0('question', 8:10)) %>%
    mutate(V2 = ifelse(V1 %in% c('question8', 'question9'), 6-V2,V2)) %>%
    pull(V2) %>% sum/3
  MAIA_attreg <- data %>% 
    filter(V1 %in% paste0('question', 11:17)) %>%
    pull(V2) %>% sum/7
  MAIA_emoaware <- data %>% 
    filter(V1 %in% paste0('question', 18:22)) %>%
    pull(V2) %>% sum/5
  MAIA_selfreg <- data %>% 
    filter(V1 %in% paste0('question', 23:26)) %>%
    pull(V2) %>% sum/4
  MAIA_bodylisten <- data %>% 
    filter(V1 %in% paste0('question', 27:29)) %>%
    pull(V2) %>% sum/3
  MAIA_trust <- data %>% 
    filter(V1 %in% paste0('question', 30:32)) %>%
    pull(V2) %>% sum/3

  final <- data.frame(MAIA_attncheck1,MAIA_noticing,MAIA_distracting,MAIA_notworrying,
                      MAIA_attreg,MAIA_emoaware,MAIA_selfreg,MAIA_bodylisten,MAIA_trust)
  return(final)
  
}
mlq_score <- function(data){
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  
  MLQ_presence <- data %>% filter(V1 %in% paste0('question',c(1,4,5,6,9))) %>%
    mutate(V2 = ifelse(V1=='question9',8-V2,V2)) %>%
    pull(V2) %>% sum
  MLQ_search <- data %>% filter(V1 %in% paste0('question',c(2,3,7,8,10))) %>%
    pull(V2) %>% sum
  
  final <- data.frame(MLQ_presence, MLQ_search)
  return(final)
}
ncs_short_score <- function(data){
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  
  NCS_total <- data %>% mutate(V2 = ifelse(V1 %in% c('question3', 'question4'), 6-V2,V2)) %>%
    pull(V2) %>% sum
  
  final <- data.frame(NCS_total)
  return(final)
}
oasis_score <- function(data){
  # supposed to be 0-4 not 1-5
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))-1)
  
  OASIS_total <- data %>% pull(V2) %>% sum
  final <- data.frame(OASIS_total)
  return(final)
}
pcl_5_score <- function(data){
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  
  PCL_total <- data %>% pull(V2) %>% sum
  final <- data.frame(PCL_total)
  return(final)
}
phq_8_score <- function(data){
  # supposed to be 0-4 not 1-5
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))-1)
  
  PHQ_total <- data %>% pull(V2) %>% sum
  final <- data.frame(PHQ_total)
  return(final)
}
shs_score <- function(data){
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  
  SHS_total <- data %>% pull(V2) %>% sum
  final <- data.frame(SHS_total)
  return(final)
}
tas_score <- function(data){
  TAS_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)=='Item 4'
  
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    mutate(V2 =ifelse(V1 %in% paste0('question', c(4,5,10,18,19)), 6-V2, V2))
  
  TAS_dif <- data %>% filter(V1 %in% paste0('question',c(1,3,6,7,9,13,14))) %>%
    pull(V2) %>% sum
  TAS_ddf <- data %>% filter(V1 %in% paste0('question',c(2,4,11,12,17))) %>%
    pull(V2) %>% sum
  TAS_eot <- data %>% filter(V1 %in% paste0('question',c(5,8,10,15,16,18,19,20))) %>%
    pull(V2) %>% sum
  TAS_total <- data %>% pull(V2) %>% sum
  
  final <- data.frame(TAS_attncheck1,TAS_dif,TAS_ddf,TAS_eot,TAS_total)
  return(final)
}
vps_score <- function(data){
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  
  VPS_total <- data %>% pull(V2) %>% sum
  final <- data.frame(VPS_total)
  return(final)
}
wolf_score <- function(data){
  WOLF_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)=='Item 3'
  
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  
  WOLF_absorption <- data %>% filter(V1 %in% paste0('question',1:4)) %>%
    pull(V2) %>% sum
  WOLF_workenjoy <- data %>% filter(V1 %in% paste0('question',5:8)) %>%
    pull(V2) %>% sum
  WOLF_intrinmotiv <- data %>% filter(V1 %in% paste0('question',9:13)) %>%
    pull(V2) %>% sum
  
  final <- data.frame(WOLF_attncheck1,WOLF_absorption,WOLF_workenjoy,WOLF_intrinmotiv)
  return(final)
}
gfi_score <- function(data){
  #need to check that this works
  data <- data %>% filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  
  #minus one because second question is zero indexed
  gfi_total <- data %>% pull(V2) %>% sum - 1 
  final <- data.frame(gfi_total)
  return(final)
}
bfi_score <- function(data){
  BFI_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)==2
  
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(V2))

  BFI_extraversion <- data %>% filter(V1 %in% paste0('question', c(1,6,11,16,21,26,31,36))) %>%
    mutate(V2 = ifelse(V1 %in% paste0('question', c(6,21,31)),6-V2,V2)) %>%
    pull(V2) %>% sum
  BFI_agreeable <- data %>% filter(V1 %in% paste0('question', c(2,7,12,17,22,27,32,37,42))) %>%
    mutate(V2 = ifelse(V1 %in% paste0('question', c(2,12,27,37)),6-V2,V2)) %>%
    pull(V2) %>% sum
  BFI_conscientious <- data %>% filter(V1 %in% paste0('question', c(3,8,13,18,23,28,33,38,43))) %>%
    mutate(V2 = ifelse(V1 %in% paste0('question', c(8,18,23,43)),6-V2,V2)) %>%
    pull(V2) %>% sum
  BFI_neurotic <- data %>% filter(V1 %in% paste0('question', c(4,9,14,19,24,29,34,39))) %>%
    mutate(V2 = ifelse(V1 %in% paste0('question', c(9,24,34)),6-V2,V2)) %>%
    pull(V2) %>% sum
  BFI_openness <- data %>% filter(V1 %in% paste0('question', c(5,10,15,20,25,30,35,40,41,44))) %>%
    mutate(V2 = ifelse(V1 %in% paste0('question', c(35,41)),6-V2,V2)) %>%
    pull(V2) %>% sum
    
  
  final <- data.frame(BFI_attncheck1,BFI_extraversion,BFI_agreeable,
                      BFI_conscientious,BFI_neurotic,BFI_openness)
  return(final)
}
fss_score <- function(data){
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  FSS_absorption <- data %>% filter(V1 %in% paste0('question',c(1,3,6,10))) %>%
    pull(V2) %>% sum
  FSS_fluency <- data %>% filter(V1 %in% paste0('question',c(2,4,5,7,8,9))) %>%
    pull(V2) %>% sum
  final <- data.frame(FSS_absorption,FSS_fluency)
  return(final)
}
sbi_score <- function(data){
  SBI_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)=='Item 6'
  
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    mutate(V2 =ifelse(V1 %in% paste0('question', c(2,4,6,8,10,12,14,16,18,20,22,24)),8-V2,V2)) 
  
  SBI_anticipating <- data %>% filter(V1 %in% paste0('question', c(4,10,16,22,1,7,13,19))) %>%
    pull(V2) %>% sum/8
  SBI_savoring <- data %>% filter(V1 %in% paste0('question', c(2,8,14,20,5,11,17,23))) %>%
    pull(V2) %>% sum/8
  SBI_reminiscing <- data %>% filter(V1 %in% paste0('question', c(6,12,18,24,3,9,15,21))) %>%
    pull(V2) %>% sum/8
  SBI_total <- data %>% pull(V2) %>% sum/24
  
  final <- data.frame(SBI_attncheck1,SBI_anticipating,SBI_savoring,SBI_reminiscing,SBI_total)
  return(final)
}
stai_state_score <- function(data){
  STAI_s_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)=='Item 4'
  
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    mutate(V2 = ifelse(V1 %in% paste0('question', c(1,2,5,8,10,11,15,16,19,20)),5-V2,V2))
  STAI_S_total <- data %>% pull(V2) %>% sum
  final <- data.frame(STAI_s_attncheck1,STAI_S_total)
  return(final)
}
stai_trait_score <- function(data){
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    mutate(V2 = ifelse(V1 %in% paste0('question', c(1,3,6,7,10,13,14,16,19)),5-V2,V2))
  STAI_T_total <- data %>% pull(V2) %>% sum
  final <- data.frame(STAI_T_total)
  return(final)
}
svs_score <- function(data){
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  
  SVS_inddiff <- data %>% filter(V1 %in% paste0('question', 1:7)) %>%
    mutate(V2 = ifelse(V1 == 'question2', 8-V2,V2)) %>%
    pull(V2) %>% sum/7
  SVS_state <- data %>% filter(V1 %in% paste0('question', 8:14)) %>%
    mutate(V2 = ifelse(V1 == 'question9', 8-V2,V2)) %>%
    pull(V2) %>% sum/7
  final <- data.frame(SVS_inddiff,SVS_state)
  return(final) 
}
teps_score <- function(data){
  # note that the questions are labelled wrong in StimTool and in the output files so I write over them here
  # ALSO THE REDCAP SCORING IS WRONG
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    mutate(V1 = paste0('question', c(18,6,5,16,9,15,8,
                                     14,13,12,11,10,7,4,
                                     3,17,2,1)))
  
  TEPS_anticipatory <- data %>% filter(V1 %in% paste0('question', c(1,3,7,11,12,14,15,16,17,18))) %>%
    mutate(V2 = ifelse(V1 == 'question7',7-V2,V2)) %>% pull(V2) %>% sum
  TEPS_consummatory <- data %>% filter(V1 %in% paste0('question', c(2,4,5,6,8,9,10,13))) %>%
    pull(V2) %>% sum
  
  final <- data.frame(TEPS_anticipatory,TEPS_consummatory)
  return(final)  
}
upps_p_score <- function(data){
  # same problem as teps, questions are labelled wrong
  # REDCAP SCORING IS WRONG
  UPPSP_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)=='Item 1'
  UPPSP_attncheck2 <- data %>% filter(V1=='question_check2') %>% pull(V2)=='Item 1'
  
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    mutate(V1 = paste0('question', 1:59))
  
  UPPSP_negurg <- data %>% filter(V1 %in% paste0('question', c(2,7,12,17,22,29,
                                                               34,39,44,50,53,58))) %>% 
    mutate(V2 = ifelse(V1=='question53',V2,5-V2)) %>% 
    pull(V2) %>% sum
  UPPSP_posurg <- data %>% filter(V1 %in% paste0('question', c(5,10,15,20,25,30,35,
                                                               40,45,49,52,54,57,59))) %>% 
    mutate(V2 = 5-V2) %>% pull(V2) %>% sum
  UPPSP_lackpremed <- data %>% filter(V1 %in% paste0('question', c(1,6,11,16,21,28,
                                                                   33,38,43,48,55))) %>%
    pull(V2) %>% sum
  UPPSP_lackpersev <- data %>% filter(V1 %in% paste0('question', c(4,9,14,19,24,
                                                                   27,32,37,42,47))) %>%
    mutate(V2 = ifelse(V1 %in% c('question9', 'question47'),5-V2,V2)) %>% 
    pull(V2) %>% sum
  UPPSP_senseeking <- data %>% filter(V1 %in% paste0('question', c(3,8,13,18,23,26,31,
                                                                   36,41,46,51,56))) %>%
    mutate(V2 = 5-V2) %>% pull(V2) %>% sum
  return(data.frame(UPPSP_attncheck1,UPPSP_attncheck2,UPPSP_negurg,UPPSP_posurg,
                    UPPSP_lackpremed,UPPSP_lackpersev,UPPSP_senseeking))
}
vhs_score <- function(data){
  # REDCAP scoring is wrong
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2)))
  
  VHS_total <- data %>% pull(V2) %>% sum
  VHS_F1 <- data %>% filter(V1 %in% paste0('question', c(1,3,5,7))) %>%
    pull(V2) %>% sum
  VHS_F2 <- data %>% filter(V1 %in% paste0('question', c(2,4,6,8))) %>%
    pull(V2) %>% sum
  
  final <- data.frame(VHS_total,VHS_F1,VHS_F2)
  return(final) 
}
zan_srv_score <- function(data){
  # should be scored 0-4 not 1-5
  data <- data %>% filter(!grepl('check', V1)) %>% 
    filter(!grepl('TRQ', V1)) %>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))-1)
  
  ZAN_total <- data %>% pull(V2) %>% sum
  final <- data.frame(ZAN_total)
  return(final) 
}
whodas_score <- function(data){
  WHODAS_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)=='Item 5'
  WHODAS_total <- data %>% filter(V1 %in% paste0("question", 1:12)) %>% 
    mutate(V2 = as.numeric(sub('.*Item ','', V2))) %>%
    pull(V2) %>% sum
  WHODAS_days_with_difficulties = data %>% filter(V1 =='question13') %>% 
    mutate(V2 = as.numeric(V2)) %>% pull(V2)
  WHODAS_days_totally_unable = data %>% filter(V1 =='question14') %>% 
    mutate(V2 = as.numeric(V2)) %>% pull(V2)
  WHODAS_days_reduce_activities = data %>% filter(V1 =='question15') %>% 
    mutate(V2 = as.numeric(V2)) %>% pull(V2)
  
  final = data.frame(WHODAS_attncheck1,WHODAS_total,WHODAS_days_with_difficulties,
                     WHODAS_days_totally_unable,WHODAS_days_reduce_activities)
  return(final)
  
}
dfas_score <- function(data){
  # REDCAP scoring is wrong, Dr. Lundy email says official reverse-coded items
  # also Strongly Agree should be 6 and Strongly Disagree should be 0 so I flipped our scoring
  DFAS_attncheck1 <- data %>% filter(V1=='question_check1') %>% pull(V2)=='Item 6'
  df_filtered = data %>% filter(!grepl('TRQ', V1))  %>% 
    filter(!(V1=='question_check1'))%>%
    mutate(V2 = abs(7-as.numeric(sub('.*Item ','', V2))))
  DFAS_total = df_filtered %>%  
    mutate(V2 = ifelse(V1 %in% c('question2','question6','question8','question9',
                                 'question14','question17','question19','question20',
                                 'question24','question26','question29','question30'), 7-V2,V2)) %>%
    pull(V2) %>% sum
  return(data.frame(DFAS_total,DFAS_attncheck1))
}
bis_bas_score <- function(data){
  BIS_BAS_attncheck <- data %>% filter(V1=='question_check') %>% pull(V2)=='Item 1'
  df_filtered = data %>% filter(!grepl('TRQ', V1))  %>% 
    filter(!(V1=='question_check'))%>%
    mutate(V2 = 5-as.numeric(sub('.*Item ','', V2))) %>%   
    mutate(V1 = paste0("question", 1:24))
  
  BAS_drive = df_filtered %>%  filter(V1 %in% c('question3','question9','question12','question21'))%>%
    pull(V2) %>% sum
  BAS_fun_seeking = df_filtered %>%  filter(V1 %in% c('question5','question10','question15','question20'))%>%
    pull(V2) %>% sum
  BAS_reward = df_filtered %>%  filter(V1 %in% c('question4','question7','question14','question18','question23'))%>%
    pull(V2) %>% sum
  BAS_total = BAS_drive + BAS_fun_seeking + BAS_reward
  
  BIS_total = df_filtered %>%  filter(V1 %in% c('question2','question8','question13',
                                                'question16','question19','question22','question24'))%>%
    mutate(V2 = ifelse(V1 %in% c('question2','question22'), 5-V2,V2)) %>%
    pull(V2) %>% sum
  return(data.frame(BIS_BAS_attncheck,BAS_drive,BAS_fun_seeking,BAS_reward,BAS_total,BIS_total))
}
promis_emotion_score <- function(data){
  promis_emotion_tscore = data %>% filter(V1=="tScore") %>% pull(V2)
  df_filtered = data %>% filter(!grepl('TRQ', V1))  %>% 
    filter(!(V1 %in% c('question_check', 'tScore')))%>% 
    mutate(V2=as.numeric(sub('.*Item ','', V2)))
  promis_emotion_raw = df_filtered %>% pull(V2) %>% sum
  return(data.frame(PROMIS_emotional_support_raw, PROMIS_emotional_support_tscore))
}
promis_meaning_score <- function(data){
  promis_meaning_tscore = data %>% filter(V1=="tScore") %>% pull(V2)
  if (any(grepl('question_check1', data$V1))) {
    promis_meaning_attncheck = data %>% filter(V1=="question_check1") %>% pull(V2) == '4.0'
  } else {
    promis_meaning_attncheck = NA
  }
  df_filtered = data %>% filter(!grepl('TRQ', V1))  %>% filter(!(V1 %in% c('question_check1', 'tScore')))%>% mutate(V2=as.numeric(sub('.*Item ','', V2)))
  promis_meaning_attncheck = TRUE
  promis_meaning_raw = df_filtered %>% pull(V2) %>% sum
  #print(promis_emotion_score)
  return(data.frame(PROMIS_meaning_tscore, PROMIS_meaning_attncheck,PROMIS_meaning_raw))
}
promis_self_efficacy_score <- function(data){
  PROMIS_self_efficacy_tscore = data %>% filter(V1=="tScore") %>% pull(V2)
  PROMIS_self_efficacy_raw = data %>% filter(!grepl('TRQ', V1))%>% 
    filter(!(V1 %in% c('question_check', 'tScore'))) %>% 
    mutate(V2=as.numeric(sub('.*Item ','', V2))) %>% 
    pull(V2) %>% sum
  return(data.frame(PROMIS_self_efficacy_tscore, PROMIS_self_efficacy_raw))
}
promis_self_efficacy_manage_score <- function(data){
  PROMIS_self_efficacy_manage_tscore = data %>% filter(V1=="tScore") %>% pull(V2)
  PROMIS_self_efficacy_manage_raw = data %>% filter(!grepl('TRQ', V1)) %>% 
    filter(!(V1 %in% c('question_check', 'tScore')))%>% 
    mutate(V2=as.numeric(sub('.*Item ','', V2)))%>% 
    pull(V2) %>% sum
  return(data.frame(PROMIS_self_efficacy_manage_raw, PROMIS_self_efficacy_manage_tscore))
}
promis_sleep_score <- function(data){
  promis_sleep_tscore = data %>% filter(V1=="tScore") %>% pull(V2)
  df_filtered = data %>% filter(!grepl('TRQ', V1))  %>% filter(!(V1 %in% c('question_check', 'tScore')))%>% mutate(V2=as.numeric(sub('.*Item ','', V2)))
  promis_sleep_raw = df_filtered %>% pull(V2) %>% sum
  #print(promis_emotion_score)
  return(data.frame(PROMIS_sleep_raw, PROMIS_sleep_tscore))
}
promis_social_iso_score <- function(data){
  PROMIS_social_iso_tscore = data %>% filter(V1=="tScore") %>% pull(V2)
  PROMIS_social_iso_raw = data %>% filter(!grepl('TRQ', V1))  %>% 
    filter(!(V1 %in% c('question_check', 'tScore')))%>%
    mutate(V2=as.numeric(sub('.*Item ','', V2))) %>% pull(V2) %>% sum
  return(data.frame(PROMIS_social_iso_raw, PROMIS_social_iso_tscore))
}
promis_social_sat_score <- function(data){
  #5 should be never and 1 should be always so this is fixed by reverse scoring here
  # also don't think of this as "social satisfaction" bc that's not right
  PROMIS_social_sat_tscore = data %>% filter(V1=="tScore") %>% pull(V2)
  PROMIS_social_sat_raw = data %>% filter(!grepl('TRQ', V1))  %>% 
    filter(!(V1 %in% c('question_check', 'tScore')))%>% 
    mutate(V2=6-as.numeric(sub('.*Item ','', V2))) %>% 
    pull(V2) %>% sum
  return(data.frame(PROMIS_social_ability_raw, PROMIS_social_ability_tscore))
}
csass_oecd_score <- function(data){
  # Item 1=0, item 2=1, etc.
  df_filtered = data %>% filter(!grepl('TRQ', V1))  %>% 
    filter(!(V1=='question_check1'))%>%
    mutate(V2 = as.numeric(sub('.*Item ','', V2))-1) 
  Ladder_personal_step = df_filtered$V2[df_filtered$V1=='question1']
  Ladder_5yrs_standout = df_filtered$V2[df_filtered$V1=='question2']
  Ladder_happy = df_filtered$V2[df_filtered$V1=='question3']
  Ladder_5yrsago_sat = df_filtered$V2[df_filtered$V1=='question4']
  Ladder_5yrs_sat = df_filtered$V2[df_filtered$V1=='question5']
  
  return(data.frame(Ladder_personal_step,Ladder_5yrs_standout,Ladder_happy,
                    Ladder_5yrsago_sat,Ladder_5yrs_sat))
  
}
health_questions_score <- function(data){
  if(length(data$V1)==1){
    return(data.frame(Health_diagnoses=NA,
                      Health_medications=NA,
                      Health_conditions=NA))
    }else{
  diagnoses <- c(); diagnosis_dict <- list('Item 1'='MDD', 'Item 2' = 'GAD',
                                           'Item 3'='Bipolar', 'Item 4' = 'PTSD',
                                           'Item 5' = 'Schiz', 'Item 6' = 'ED',
                                           'Item 7' = 'SUD', 'Item 8' = 'OCD',
                                           'Item 9' = 'Panic','Item 10' = 'Agoraphobia',
                                           'Item 11' = 'Social', 'Item 12' = 'ADHD',
                                           'Item 13' = 'PNTA', 'Item Other'='Other')
  ed_dict <- list('Item 1' ='Anorexia', 'Item 2' = 'Bulimia', 'Item 3'='PNTA_ED',
                  'Item Other' = 'Other_ED')
  sud_dict <- list('Item 1' = 'Alcohol', 'Item 2'='Opioid', 'Item 3'='Meth',
                   'Item 4'='Cannabis', 'Item 5'='PNTA_SUD', 'Item Other'='Other_SUD')
  diags <- strsplit(data$V2[data$V1=='question_diagnose'],split = ';')
  if(!is_empty(diags)){
    for(i in diags[[1]]){
      diagnoses <- append(diagnoses,(diagnosis_dict[[i]]))
    }
  }
  if('question_diagnose_other' %in% data$V1){
    diagnoses <- append(diagnoses,data$V2[data$V1=='question_diagnose_other'])
  }
  if('question_ed' %in% data$V1){
    eats <- strsplit(data$V2[data$V1=='question_ed'],split = ';')
    for(j in eats[[1]]){
      diagnoses <- append(diagnoses,(ed_dict[[j]]))
    }
    if('question_ed_other' %in% data$V1){
      diagnoses <- append(diagnoses,data$V2[data$V1=='question_ed_other'])
    }
  }
  if('question_sud' %in% data$V1){
    suds <- strsplit(data$V2[data$V1=='question_sud'],split = ';')
    for(j in suds[[1]]){
      diagnoses <- append(diagnoses,(sud_dict[[j]]))
    }
    if('question_sud_other' %in% data$V1){
      diagnoses <- append(diagnoses,data$V2[data$V1=='question_sud_other'])
    }
  }
  medications<-c(); meds <- strsplit(data$V2[data$V1=='question_med'],split = ';')
  med_dict <- list('Item 1'='Antidepress', 'Item 2'='Antianx','Item 3'='MoodStabil',
                   'Item 4'='Neuroleptics', 'Item 5'='ADHDMeds', 'Item 6'='SleepDis',
                   'Item 7'='Seizure','Item 8'='Withdrawal','Item 9'='SUD',
                   'Item 10'='PNTA_med', 'Item Other'='Other_med')
  if(!is_empty(meds)){
    for(i in meds[[1]]){
      medications <- append(medications,(med_dict[[i]]))
      if('question_med_other' %in% data$V1){
        medications <- append(medications,data$V2[data$V1=='question_med_other'])
      }
    }
  }
  antidep_dict <- list('Item 1'='Zoloft/Sertraline', 'Item 2'='Lexapro/Escitalopram',
                       'Item 3'='Prozac/Fluoxetine', 'Item 4'='Wellbutrin/Buproprion',
                       'Item 5'='Celexa/Citalopram', 'Item 6'='Desvenlafaxine/Pristiq',
                       'Item 7'='Duloxetine/Cymbalta','Item 8'='Venlafaxine/Effexor',
                       'Item 9'='Amitripytline', 'Item 10'='Amoxapine/Imipramine',
                       'Item 11'='Tofranil', 'Item 12'='Nortriptyline/Pamelor',
                       'Item 13'='PNTA_antidep', 'Item Other'='Other_antidep')
  if('question_antidep' %in% data$V1){
    meds <- strsplit(data$V2[data$V1=='question_antidep'],split = ';')
    for(j in meds[[1]]){
      medications <- append(medications,(antidep_dict[[j]]))
    }
    if('question_antidep_other' %in% data$V1){
      medications <- append(medications,data$V2[data$V1=='question_antidep_other'])
    }
  }
  antianx_dict <- list('Item 1'='Valium/Diazepam','Item 2'='Xanax/Alprazolam',
                       'Item 3'='Ativan/Lorazepam','Item 4'='Klonopin/Clonazepam',
                       'Item 5'='PNTA_antianx','Item Other'='Other_antianx')
  if('question_antianx' %in% data$V1){
    meds <- strsplit(data$V2[data$V1=='question_antianx'],split = ';')
    for(j in meds[[1]]){
      medications <- append(medications,(antianx_dict[[j]]))
    }
    if('question_antianx_other' %in% data$V1){
      medications <- append(medications,data$V2[data$V1=='question_antidep_other'])
    }
  }
  mood_dict <- list('Item 1'='Lithium','Item 2'='Valproate/Depakote',
                    'Item 3'='Lamotrigine/Lamictal','Item 4'='PNTA_antidep', 'Item Other'='Other_mood')
  if('question_moodstab' %in% data$V1){
    meds <- strsplit(data$V2[data$V1=='question_moodstab'],split = ';')
    for(j in meds[[1]]){
      medications <- append(medications,(mood_dict[[j]]))
    }
    if('question_moodstab_other' %in% data$V1){
      medications <- append(medications,data$V2[data$V1=='question_moodstab_other'])
    }
  }
  neuro_dict <- list('Item 1'='Risperidone/Risperdal', 'Item 2'='Quetiapine/Seroquel',
                     'Item 3'='Olanzapine/Zyprexa','Item 4'='Ability/Aripiprazole',
                     'Item 5'='Clozapine', 'Item 6'='PNTA_neuroleptic')
  if('question_neur' %in% data$V1){
    meds <- strsplit(data$V2[data$V1=='question_neur'],split = ';')
    for(j in meds[[1]]){
      medications <- append(medications,(neuro_dict[[j]]))
    }
    if('question_neur_other' %in% data$V1){
      medications <- append(medications,data$V2[data$V1=='question_neur_other'])
    }
  }
  adhd_dict <- list('Item 1'='Ritalin/Methylphenidate','Item 2'='Adderall/Amphetamine',
                    'Item 3'='Vyvanse/Lisdexamfetamine','Item 4'='Atomoxetine/Strattera',
                    'Item 5'='Guanfacine/Intuniv','Item 6'='PNTA_adhd','Item Other'='Other_adhd')
  if('question_adhd' %in% data$V1){
    meds <- strsplit(data$V2[data$V1=='question_adhd'],split = ';')
    for(j in meds[[1]]){
      medications <- append(medications,(adhd_dict[[j]]))
    }
    if('question_adhd_other' %in% data$V1){
      medications <- append(medications,data$V2[data$V1=='question_adhd_other'])
    }
  }
  sleep_dict <- list('Item 1'='Modafinil/Provigil', 'Item 2'='Eszopiclone/Lunesta',
                     'Item 3'='Zolpidem/Ambien','Item 4'='PNTA_sleep', 'Item Other'='Other')
  if('question_sleep' %in% data$V1){
    meds <- strsplit(data$V2[data$V1=='question_sleep'],split = ';')
    for(j in meds[[1]]){
      medications <- append(medications,(sleep_dict[[j]]))
    }
    if('question_sleep_other' %in% data$V1){
      medications <- append(medications,data$V2[data$V1=='question_sleep_other'])
    }
  }
  seizure_dict <- list('Item 1'='Gabapentin/Neurontin', 'Item 2'='Pregabalin/Lyrica',
                       'Item 3'='Topiramate/Topamax','Item 4'='Lamotrigine/Lamictal',
                       'Item 5'='PNTA_seizure', 'Item Other'='Other_seizure')
  if('question_seiz' %in% data$V1){
    meds <- strsplit(data$V2[data$V1=='question_seiz'],split = ';')
    for(j in meds[[1]]){
      medications <- append(medications,(seizure_dict[[j]]))
    }
    if('question_seiz_other' %in% data$V1){
      medications <- append(medications,data$V2[data$V1=='question_seiz_other'])
    }
  }
  withdraw_dict <- list('Item 1'='Buprenorphine/Suboxone', 'Item 2'='Methadone',
                        'Item 3'='PNTA_withdraw', 'Item Other'='Other_withdraw')
  if('question_withd' %in% data$V1){
    meds <- strsplit(data$V2[data$V1=='question_withd'],split = ';')
    for(j in meds[[1]]){
      medications <- append(medications,(withdraw_dict[[j]]))
    }
    if('question_withd_other' %in% data$V1){
      medications <- append(medications,data$V2[data$V1=='question_withd_other'])
    }
  }
  sudmeds_dict <- list('Item 1'='Naltrexone/Vivitrol','Item 2'='Acamprosate/Campral',
                       'Item 3'='PNTA_sudmeds','Item Other'='Other_sudmeds')
  if('question_sudmed' %in% data$V1){
    meds <- strsplit(data$V2[data$V1=='question_sudmed'],split = ';')
    for(j in meds[[1]]){
      medications <- append(medications,(sudmeds_dict[[j]]))
    }
    if('question_sudmed_other' %in% data$V1){
      medications <- append(medications,data$V2[data$V1=='question_sudmed_other'])
    }
  }
  conditions <- c(); cond_dict <- list('Item 1'='Seizures','Item 2'='Epilepsy',
                                       'Item 3'='Stroke','Item 4'='Parkinsons',
                                       'Item 5' = 'Alzheimers', 'Item 6'='TBI',
                                       'Item 7'='MemoryAttention', 'Item 8'='LossConciousnessHeadInjury',
                                       'Item 9'='PNTA', 'Item Other'='Other')
  conds <- strsplit(data$V2[data$V1=='question_medical'],split = ';')
  if(!is_empty(conds)){
    for(i in conds[[1]]){
      conditions <- append(conditions,(cond_dict[[i]]))
    }
  }
  Health_diagnoses <- paste(diagnoses, collapse = ';')
  Health_medications <- paste(medications, collapse = ';')
  Health_conditions <- paste(conditions, collapse = ';')
  return(data.frame(Health_diagnoses,Health_medications,Health_conditions))
    }
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

write.csv(full.data,glue('L:/rsmith/wellbeing/data/prolific/survey_data.csv'), row.names=F)

