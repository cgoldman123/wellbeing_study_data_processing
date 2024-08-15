## Purpose: Prolific analyses


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
library(dplyr)
library(reticulate)

## Clear workspace -------------------------------------------------------------
rm(list = ls())
setwd('L:/rsmith/wellbeing/tasks')
## Run necessary scripts before combining --------------------------------------
# note that python here runs using a reticulate virtual environment
source('../util/prolific_survey_scoring.R')
source_python('./QC/pull_demographics.py')
## Load in data -----------------------------------------------------------
setwd('L:/rsmith/wellbeing/tasks')
surveys <- read.csv('../data/prolific/survey_data.csv')



## ========= Social Media ========
directory <- file.info(list.files('./SocialMedia/output/prolific/kf', 
                                  pattern='fits', full.names = T)) %>% 
  as.data.frame %>% 
  rownames_to_column(.) %>% rename(filname='rowname') %>%
  mutate(d=ifelse(grepl(pattern = 'Dislike', filname), T, F)) %>%
  mutate(cb=ifelse(grepl(pattern = '_CB', filname), T, F))


sm.data <- data.frame()
for(rd in c(F,T)){
  #for(counter in c(F,T)){  # CMG commented out because no longer fit separately by CB
    #sm.data <- directory %>% filter(d==rd) %>% filter(cb==counter) %>%
      sm.data <- directory %>% filter(d==rd) %>%
      arrange(mtime) %>% tail(n=1) %>%
      pull(filname) %>% print(.) %>%
      read.csv(.) %>% 
      mutate(room_type = ifelse(rd,'Dislike','Like')) %>%
     # mutate(CB = counter) %>%
      mutate(DE = info_bonus_h5 - info_bonus_h1) %>%
      mutate(RE = dec_noise_h5_13 - dec_noise_h1_13) %>%
      rbind(sm.data,.) 

  #}
}
names(sm.data) <- ifelse((names(sm.data) != "id" & names(sm.data) != "room_type" & names(sm.data) != "has_practice_effects"), paste("KF", names(sm.data), sep = "_"), names(sm.data))


# uncomment to get logistic choice model results
# combine_social_fits <- function(root, result_dir, write.table=F){
#   rooms = c('Like', 'Dislike')  
#   for(room in rooms){
#     all.files <- list.files(glue("{root}"), pattern=glue(".*{room}.*.csv"), full.names = T)
#     name <- paste("df", sep='.', room)
#     assign(name, all.files %>% 
#              pblapply(., FUN = read.csv) %>% 
#              do.call(rbind, .) %>%
#              mutate(room_type = room) %>%
#              relocate(room_type, .after = session))
#   } 
#   df <- rbind(df.Like, df.Dislike)
#   return(df)
#   if(write.table){write.csv(df, glue("{result_dir}/fits_{Sys.Date()}.csv"), row.names=F)}
# }

# sm.data <- combine_social_fits(root='./SocialMedia/output/prolific/logistic', 
#                     result_dir = './SocialMedia/output') %>% 
#   rename(id='subject') %>%
#   merge(sm.data,.,by=c('id', 'room_type'), all=T)%>% 
#   rename_with(~paste0('SM_',.), -id)
# if(sm.data$SM_has_practice_effects.x %>% is.na() %>% sum >0){
#   print(glue('{sm.data$SM_has_practice_effects.x %>% is.na() %>% sum/2} subjects need to be added to the KF!'))
# }
# sm.data <- sm.data %>% filter(!is.na(SM_has_practice_effects.x))
# if(sum(sm.data$SM_has_practice_effects.x != sm.data$SM_has_practice_effects.y)){
#   print('Warning! Practice effects don"t match up in Social Media')
# }else{
#   sm.data <- sm.data %>% 
#     mutate(SM_has_practice_effects = SM_has_practice_effects.x) %>%
#     select(-contains('.x'),-contains('.y'))
# }
# sm.data <- sm.data %>%
#   select(-SM_num_games) %>%
#   select(id, SM_has_practice_effects, SM_room_type, everything())%>%
#   pivot_wider(id_cols=c(id, SM_has_practice_effects),
#               names_from = 'SM_room_type', values_from = c(4:86)) %>%
#   as.data.frame() %>%
#   mutate(across(everything(), ~sapply(., function(x) if (length(x) > 0) x[1] else NA_real_))) %>%
#   mutate(
#     SM_DE_Dislike = SM_KF_info_bonus_h5_Dislike - SM_KF_info_bonus_h1_Dislike,
#     SM_DE_Like = SM_KF_info_bonus_h5_Like - SM_KF_info_bonus_h1_Like,
#     SM_RE_Dislike = SM_KF_dec_noise_h5_13_Dislike - SM_KF_dec_noise_h1_13_Dislike,
#     SM_RE_Like = SM_KF_dec_noise_h5_13_Like - SM_KF_dec_noise_h1_13_Like
#   )

duplicated_sm_fits = sm.data %>% count(id) %>% filter(n > 2)
cat("Number of SM subjects removed for duplicate data", nrow(duplicated_sm_fits ),"\n")
sm.data = sm.data %>% filter(!id %in% duplicated_sm_fits$id) 


sm.data <- sm.data %>%
    select(id, has_practice_effects, room_type, everything())%>%
    pivot_wider(id_cols=c("id","has_practice_effects"),
                names_from = c("room_type"),
                values_from = setdiff(names(sm.data), c("id", "room_type","has_practice_effects"))) %>%
    as.data.frame() %>%
    rename_with(~paste0('SM_',.), -id)

    

  


  
## =========== Advice Task ===========
ad.data <- file.info(list.files('./AdviceTask/output',
                                    pattern='fits', full.names = T)) %>% 
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() %>%
  rename_with(~paste0('AD_',.), -id)





## =========== Theory of Mind ===========
tom.1_1_1_0 = file.info(list.files('./TheoryofMind/fits/prolific',
                                   pattern='_1_1_1_0', full.names = T)) %>% 
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() %>%
  rename(id = subject) %>% as.data.frame(.) %>%   # Rename 'subject' to 'id'
  rename_with(~paste0('model_1_1_1_0_',.), -id)

tom.1_1_1_1 = file.info(list.files('./TheoryofMind/fits/prolific',
                                   pattern='_1_1_1_1', full.names = T)) %>% 
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() %>%
  rename(id = subject) %>% as.data.frame(.) %>%   # Rename 'subject' to 'id'
  rename_with(~paste0('model_1_1_1_1_',.), -id)

tom.1_1_1_2 = file.info(list.files('./TheoryofMind/fits/prolific',
                                   pattern='_1_1_1_2', full.names = T)) %>% 
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() %>%
  rename(id = subject) %>% as.data.frame(.) %>%   # Rename 'subject' to 'id'
  rename_with(~paste0('model_1_1_1_2_',.), -id)

tom.data <- inner_join(tom.1_1_1_0, inner_join(tom.1_1_1_1, tom.1_1_1_2, by = "id"), by = "id") %>%
  rename_with(~paste0('TOM_',.), -id)




## ========= Emotional Faces =========
model_based_patterns <- c("rho", "sigma", "omega", "kappa", "AIC", "LME", "avg_act", "model", "p_or_r", 
                          "variance", "mu", "ze", "nu\\b", "be", "h_intensity_sal", "l_intensity_conf")
base_dir <- './EmotionalFaces/output/prolific'
# List and filter files containing both 'hgf' and 'predictions'
file_paths_filtered <- list.files(base_dir, pattern='hgf', full.names = TRUE)
file_paths_filtered <- file_paths_filtered[grep('predictions', file_paths_filtered)]
ef.data.pred.model <- file.info(file_paths_filtered) %>%
  as.data.frame() %>% rownames_to_column() %>% rename(filname='rowname') %>% arrange((mtime)) %>% tail(n=1) %>%
  pull(filname) %>% read.csv() %>% rename(id='ID') %>%
  rename_with(~ sapply(.x, function(x) {
    if (any(sapply(model_based_patterns, function(p) grepl(p, x)))) {
      paste0('EF_pred_model_', x)
    } else {
      paste0('EF_', x)
    }
  }), .cols = -id)

# List and filter files containing both 'hgf' and 'responses'
file_paths_filtered <- list.files(base_dir, pattern='hgf', full.names = TRUE)
file_paths_filtered <- file_paths_filtered[grep('responses', file_paths_filtered)]
ef.data.resp.model <- file.info(file_paths_filtered) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(filname='rowname') %>%
  arrange((mtime)) %>%
  tail(n=1) %>%
  pull(filname) %>%
  read.csv() %>%
  rename(id='ID') %>%
  rename_with(~ sapply(.x, function(x) {
    if (any(sapply(model_based_patterns, function(p) grepl(p, x)))) {
      paste0('EF_resp_model_', x)
    } else {
      paste0('EF_', x)
    }
  }), .cols = -id)

ef.data <- inner_join(ef.data.pred.model, ef.data.resp.model, by = "id")
# drop duplicated cols
ef.data <- ef.data %>%
  select(-matches("\\.y$")) %>%  # Remove columns ending with .y
  rename_with(~ sub("\\.x$", "", .x), matches("\\.x$"))  # Rename columns ending with .x



## ========= Cooperation Task =========

# coop.data <- file.info(list.files('./Cooperation/output/prolific_fits_and_mf',
#                                 pattern='coop', full.names = T)) %>% 
#   as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
#   arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() %>%
#   rename_with(~paste0('COP_',.), -id)

model_based_patterns <- c("posterior","prior","avg_action_prob","model_acc")
base_dir <- './Cooperation/output/prolific_fits_and_mf'
# List and filter files containing both 'hgf' and 'predictions'
file_paths_filtered <- list.files(base_dir, pattern='one_eta', full.names = TRUE)
coop.data.one.eta <- file.info(file_paths_filtered) %>%
  as.data.frame() %>% rownames_to_column() %>% rename(filname='rowname') %>% arrange((mtime)) %>% tail(n=1) %>%
  pull(filname) %>% read.csv() %>%
  rename_with(~ sapply(.x, function(x) {
    if (any(sapply(model_based_patterns, function(p) grepl(p, x)))) {
      paste0('COP_one_eta_', x)
    } else {
      paste0('COP_', x)
    }
  }), .cols = -id)

file_paths_filtered <- list.files(base_dir, pattern='three_etas', full.names = TRUE)
coop.data.three.etas <- file.info(file_paths_filtered) %>% as.data.frame() %>% rownames_to_column() %>%
  rename(filname='rowname') %>%arrange((mtime)) %>%tail(n=1) %>%pull(filname) %>% read.csv()  %>%
  rename_with(~ sapply(.x, function(x) {
    if (any(sapply(model_based_patterns, function(p) grepl(p, x)))) {
      paste0('COP_three_etas_', x)
    } else {
      paste0('COP_', x)
    }
  }), .cols = -id)

coop.data <- inner_join(coop.data.one.eta, coop.data.three.etas, by = "id")
# drop duplicated cols
coop.data <- coop.data %>%
  select(-matches("\\.y$")) %>%  # Remove columns ending with .y
  rename_with(~ sub("\\.x$", "", .x), matches("\\.x$"))  # Rename columns ending with .x









## ========= Blind Dating ==========
bd.data <- file.info(list.files('./BlindDating/output/fits_and_model_free/prolific',
                                  pattern='BD', full.names = T)) %>% 
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() %>%
  rename(id = subject) %>%  # Rename 'subject' to 'id'
  rename_with(~paste0('BD_',.), -id)



## ========= Factor Analysis ==========
factor.data <- file.info(list.files('../data/prolific/factor_scores/',
                                pattern='factor_scores', full.names = T)) %>% 
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() 


## =========== Combine ============
df <- merge(surveys,sm.data, by='id',all=T) %>%
  merge(.,ad.data,by='id',all=T)%>%
  merge(.,ef.data,by='id',all=T)%>%
  merge(.,coop.data,by='id',all=T) %>%
  merge(.,bd.data,by='id',all=T) %>%
  merge(.,tom.data,by='id',all=T) %>%
  merge(.,factor.data,by='id',all=T) %>%
  filter(!duplicated(.))

## ============= Add Demographics ==========
collectivistic_countries <- c("Cambodia", "China", "Hong Kong", "India", "Indonesia", 
                              "Japan", "Korea", "Lao People's Democratic Republic", 
                              "Malaysia", "Mongolia", "Myanmar", "Nepal", "Philippines", 
                              "Taiwan", "Thailand", "Vietnam")

final <- read.csv('L:/rsmith/wellbeing/data/prolific/all_demographic_data.csv') %>% 
  rename(id='Participant.id') %>% 
  group_by(id) %>% mutate(Age_at_start = min(Age)) %>% 
  ungroup %>% as.data.frame %>% select(-Age) %>% # take care of participants that might have done different order/CB 
  filter(!duplicated(.)) %>%
  filter(!(id=='65f0677c96375d7ff6595772'&(R=='R2'|CB=='CB2'))) %>%
  filter(!(id=='65f37b4d647d49eb9fee0090'&(R=='R2'))) %>%
  filter(!(id=='65fe5444cc257c70f668d2a4'&(R=='R2'| CB=='CB1'))) %>%
  filter(!(id=='65ea6d657bbd3689a87a1de6'&(R=='R2'| CB=='CB2'))) %>%
  filter(!(id=='663b8df565dd08db10006c4e'&(R=='R2'& CB=='CB1'))) %>%
  filter(!(id=='65f333c755d191fcd18fa279'&(R=='R2'& CB=='CB2'))) %>%
  # take care of participants that have duplicated values except for Culture column (thinking that they started a study that was discontinued due to some error)
  filter(!(id=='65ea6d657bbd3689a87a1de6'&(Culture=='AsiaOld'))) %>%
  filter(!(id=='65f03cac071873738667ccbb'&(Culture=='AsiaOld'))) %>%
  filter(!(id=='65f0405b36b8d4bfa1eb9637'&(Culture=='AsiaOld'))) %>%
  filter(!(id=='65f0677c96375d7ff6595772'&(Culture=='AsiaOld'))) %>%
  filter(!(id=='65f11794f02cbf5b0acb34b0'&(Culture=='AsiaOld'))) %>%
  filter(!(id=='65f85564dbfd935f4f68d062'&(Culture=='AsiaOld'))) %>%
  filter(!(id=='65fb8ba703fee27775bdc9bf'&(Culture=='AsiaOld'))) %>%
  filter(!(id=='65fe5444cc257c70f668d2a4'&(Culture=='AsiaOld'))) %>%
  filter(!(id=='65ff24005c028050281d3f17'&(Culture=='USATest'))) %>%
  # take care of participant row with expired country of birth data
  filter(!(id=='66989959a9a6e990d72c7a91'&(Country.of.birth=='DATA_EXPIRED'))) %>%
  
  mutate(collectivistic = if_else(Nationality %in% collectivistic_countries &
                                    Country.of.birth %in% collectivistic_countries, 
                                  TRUE, FALSE)) %>%
  pivot_wider(id_cols=c('id','collectivistic','Culture','Age_at_start','Sex','Ethnicity.simplified',
                        'Country.of.birth','Country.of.residence',
                        'Nationality','Language','R','CB'), 
              names_from = 'Session', 
              names_prefix = 'Status_ses_',
              values_from = 'Status') %>%
  arrange(id) %>% group_by(id) %>% 
  fill(starts_with('Status'),.direction='downup') %>%
  filter(!(Sex=='CONSENT_REVOKED'&Status_ses_1=='APPROVED')) %>%
  filter(Status_ses_1 %in% c('APPROVED', 'AWAITING REVIEW')) %>%
  merge(df,.,by='id',all=T) %>%
  filter(!is.na(Status_ses_1)) %>%
  select(id, collectivistic, CB, R, Culture, everything())

# Make sure no participants have duplicated rows
duplicated_ids <- final$id[duplicated(final$id) | duplicated(final$id, fromLast = TRUE)]
duplicates <- final %>% filter(id %in% duplicated_ids)
cat("Number of duplicated subjects in final DF: ", nrow(duplicates ),"\n")
#View(duplicates)

write.csv(final, 'L:/rsmith/wellbeing/data/prolific/SWB_prolific_data.csv', row.names = F)
