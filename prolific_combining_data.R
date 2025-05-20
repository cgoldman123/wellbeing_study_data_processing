## Purpose: Create wide dataframe for Prolific analyses


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
library(readxl)
library(lubridate)


## Clear workspace -------------------------------------------------------------
rm(list = ls())
setwd('L:/rsmith/wellbeing/tasks')
## Run necessary scripts before combining --------------------------------------
# note that python here runs using a reticulate virtual environment
# source('../util/prolific_survey_scoring.R') # no need to run this unless changes to scoring are made
# source_python('./QC/pull_demographics.py') # no need to run this unless changes to demographics are made
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
      sm.data <- directory %>% filter(d==rd) %>%
      arrange(mtime) %>% tail(n=1) %>%
      pull(filname) %>% print(.) %>%
      read.csv(.) %>% 
      mutate(room_type = ifelse(rd,'Dislike','Like')) %>%
      mutate(DE = info_bonus_h5 - info_bonus_h1) %>%
      mutate(RE = dec_noise_h5_13 - dec_noise_h1_13) %>%
      rbind(sm.data,.) 
}
names(sm.data) <- ifelse((names(sm.data) != "id" & names(sm.data) != "room_type" & names(sm.data) != "has_practice_effects"), paste("KF", names(sm.data), sep = "_"), names(sm.data))


duplicated_sm_fits = sm.data %>% count(id) %>% filter(n > 2)
cat("Number of SM subjects removed for duplicate data", nrow(duplicated_sm_fits ),"\n")
sm.data = sm.data %>% filter(!id %in% duplicated_sm_fits$id) 


sm.data <- sm.data %>%
    dplyr::select(id, has_practice_effects, room_type, everything())%>%
    pivot_wider(id_cols=c("id","has_practice_effects"),
                names_from = c("room_type"),
                values_from = setdiff(names(sm.data), c("id", "room_type","has_practice_effects"))) %>%
    as.data.frame() %>%
    rename_with(~paste0('SM_',.), -id)

    

  


  
## =========== Advice Task ===========
ad.data <- file.info(list.files('./AdviceTask/output',
                                    pattern='fits', full.names = T)) %>% 
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% print(.) %>% read.csv() %>%
  rename_with(~paste0('AD_',.), -id)





## =========== Theory of Mind ===========
#tom.1_1_1_0 = file.info(list.files('./TheoryofMind/fits/prolific',
#                                   pattern='_1_1_1_0', full.names = T)) %>% 
#  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
#  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% print(.) %>% read.csv() %>%
#  rename(id = subject) %>% as.data.frame(.) %>%   # Rename 'subject' to 'id'
#  rename_with(~paste0('model_1_1_1_0_',.), -id)

#tom.1_1_1_1 = file.info(list.files('./TheoryofMind/fits/prolific',
#                                   pattern='_1_1_1_1', full.names = T)) %>% 
#  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
#  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% print(.) %>% read.csv() %>%
#  rename(id = subject) %>% as.data.frame(.) %>%   # Rename 'subject' to 'id'
#  rename_with(~paste0('model_1_1_1_1_',.), -id)

#tom.1_1_1_2 = file.info(list.files('./TheoryofMind/fits/prolific',
#                                   pattern='_1_1_1_2', full.names = T)) %>% 
#  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
#  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% print(.) %>% read.csv() %>%
# rename(id = subject) %>% as.data.frame(.) %>%   # Rename 'subject' to 'id'
# rename_with(~paste0('model_1_1_1_2_',.), -id)

#tom.data <- inner_join(tom.1_1_1_0, inner_join(tom.1_1_1_1, tom.1_1_1_2, by = "id"), by = "id") %>%
#  rename_with(~paste0('TOM_',.), -id)
tom.data <- file.info(list.files('./TheoryofMind/Data/',
                                 pattern='ToM', full.names = T)) %>%
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>%
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% print(.) %>% read.csv() %>%
  rename_with(~paste0('TOM_',.), -ID)
tom.data <- tom.data %>%
  select(-TOM_model_n) %>%
  rename(id = ID)



## ========= Emotional Faces =========
model_based_patterns <- c("rho", "sigma", "omega", "kappa", "AIC", "LME", "avg_act", "model", "p_or_r", 
                          "variance", "mu", "ze", "nu\\b", "be", "h_intensity_sal", "l_intensity_conf")
base_dir <- './EmotionalFaces/output/prolific'
# List and filter files containing both 'hgf' and 'predictions'
file_paths_filtered <- list.files(base_dir, pattern='hgf', full.names = TRUE)
file_paths_filtered <- file_paths_filtered[grep('predictions', file_paths_filtered)]
ef.data.pred.model <- file.info(file_paths_filtered) %>%
  as.data.frame() %>% rownames_to_column() %>% rename(filname='rowname') %>% arrange((mtime)) %>% tail(n=1) %>%
  pull(filname) %>% print(.) %>% read.csv() %>% rename(id='ID') %>%
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
  as.data.frame() %>% rownames_to_column() %>% rename(filname='rowname') %>% arrange((mtime)) %>%
  tail(n=1) %>% pull(filname) %>% print(.) %>% read.csv() %>% rename(id='ID') %>%
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

coop.data <- file.info(list.files('./Cooperation/output/prolific_fits_and_mf',
                                pattern='coop', full.names = T)) %>%
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>%
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% print(.) %>% read.csv() %>%
  rename_with(~paste0('COP_',.), -id)




## ========= Blind Dating ==========
# bd.data <- file.info(list.files('./BlindDating/output/fits_and_model_free/prolific',
#                                   pattern='BD', full.names = T)) %>% 
#   as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
#   arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% print(.) %>% read.csv() %>%
#   rename(id = subject) %>%  # Rename 'subject' to 'id'
#   rename_with(~paste0('BD_',.), -id)


model_based_patterns <- c("simfit", "posterior", "prior", "fixed", "F\\b", "model_acc", "action_prob")
bd.winning.model <- file.info(list.files('./BlindDating/output/fits_and_model_free/prolific',
                                pattern='BD_winning_model', full.names = T)) %>% 
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% print(.) %>% read.csv() %>%
  rename(id = subject) %>%  # Rename 'subject' to 'id'
  rename_with(~ sapply(.x, function(x) {
    if (any(sapply(model_based_patterns, function(p) grepl(p, x)))) {
      paste0('BD_winning_model_', x)
    } else {
      paste0('BD_', x)
    }
  }), .cols = -id)

bd.second.best.model <- file.info(list.files('./BlindDating/output/fits_and_model_free/prolific',
                                              pattern='BD_second_best', full.names = T)) %>% 
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% print(.) %>% read.csv() %>%
  rename(id = subject) %>%  # Rename 'subject' to 'id'
  rename_with(~ sapply(.x, function(x) {
    if (any(sapply(model_based_patterns, function(p) grepl(p, x)))) {
      paste0('BD_second_best_model_', x)
    } else {
      paste0('BD_', x)
    }
  }), .cols = -id)

bd.data <- inner_join(bd.winning.model, bd.second.best.model, by = "id")
# drop duplicated cols
bd.data <- bd.data %>%
  dplyr::select(-matches("\\.y$")) %>%  # Remove columns ending with .y
  dplyr::rename_with(~ sub("\\.x$", "", .x), matches("\\.x$"))  # Rename columns ending with .x






## ========= Factor Analysis ==========
factor.data <- file.info(list.files('../data/prolific/factor_scores/',
                                pattern='factor_scores', full.names = T)) %>% 
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% print(.) %>% read.csv() 



## ========= LEAS ==========
leas_data = read.csv('L:/rsmith/wellbeing/data/prolific/leas/LEAS_data_scored_fixed.csv')
result_list <- list()
empty_indices <- which(leas_data$id == "")
result_counter <- 1
for (i in empty_indices) {
  id_window <- leas_data$id[(i-10):(i-1)]
  # proceed if getting total scores from only one id 
  if (length(unique(id_window)) == 1) {
    row_11 <- leas_data[i, c("X334.other", "X334.self", "X3345plus", "length", "time", "vocab")]
    result_list[[result_counter]] <- cbind(leas_data$id[i-1], row_11)
    result_counter <- result_counter + 1
  } else {
    print(i)
    print(leas_data$id[i-1])
    stop("Inconsistent id values detected or id present in row 11")
  }
}
leas.data <- do.call(rbind, result_list)
leas.data = leas.data %>% rename(id = `leas_data$id[i - 1]`,
                                 LEAS_X334_other = `X334.other`,
                                 LEAS_X334_self = `X334.self`,
                                 LEAS_X3345_combined = `X3345plus`,
                                 LEAS_total_num_words = length,
                                 LEAS_total_unique_words = vocab,
                                 LEAS_time = time)
# take the most recent LEAS score
leas.data <- leas.data %>%
  dplyr::group_by(id) %>%
  dplyr::slice_min(LEAS_time, with_ties = FALSE) %>%  # Keep the row with the lowest 'time'
  ungroup()  # Ungroup after operation


## ========= V-CRT ==========
vcrt.data = read_excel('L:/rsmith/wellbeing/data/prolific/v-crt/v_crt_hand_scored_prolific_grading_combined.xlsx')
cat("Any missing V-CRT scores in final decision column? ", any(is.na(vcrt.data$`FINAL DECISION`)))
vcrt.data.select = vcrt.data %>% select(c(id, VCRT_time, Question, `FINAL DECISION`))
id_counts <- vcrt.data.select %>%
  group_by(id) %>%
  summarise(row_count = n()) %>%
  filter(row_count != 10)
cat("Do any V-CRT IDs have more than 10 rows? ", nrow(id_counts))
# get the number of correct and intuitive responses for each participant, exlcuding the religion question
vcrt.scores <- vcrt.data.select %>%
  filter(Question %in% paste0("question", c(1:5, 7:10))) %>%  # Exclude question6
  group_by(id) %>%
  summarise(
    vcrt_time = first(VCRT_time),
    vcrt_num_corr_exclude_6 = sum(`FINAL DECISION` == 2),   # Count FINAL DECISION == 2
    vcrt_num_intuit_exclude_6 = sum(`FINAL DECISION` == 1)     # Count FINAL DECISION == 1
  ) %>%
  ungroup()


## ========= CRT ==========
crt.data = read.csv("L:/rsmith/wellbeing/data/prolific/crt/crt_data.csv")
# Remove @email.prolific.com from ids
crt.data$id <- sub("@email\\.prolific\\.com$", "", crt.data$id)
cat("Do any CRT IDs have duplicated scores? ", any(duplicated(crt.data$id)))
crt.scores <- crt.data %>%
  mutate(
    crt_time = CRT_7_time,  # Rename CRT_7_time to crt_time
    crt_num_corr = rowSums(select(., q1_cor:q7_cor)),  # Sum of q1_cor to q7_cor
    crt_num_intuit = rowSums(select(., q1_intuit:q7_intuit))  # Sum of q1_intuit to q7_intuit
  ) %>%
  select(id, crt_time, crt_num_corr, crt_num_intuit)  # Select required columns



## =========== Combine ============
df <- merge(surveys,sm.data, by='id',all=T) %>%
  merge(.,ad.data,by='id',all=T)%>%
  merge(.,ef.data,by='id',all=T)%>%
  merge(.,coop.data,by='id',all=T) %>%
  merge(.,bd.data,by='id',all=T) %>%
  merge(.,tom.data,by='id',all=T) %>%
  merge(.,factor.data,by='id',all=T) %>%
  merge(.,leas.data, by='id', all=T) %>%
  merge(.,vcrt.scores, by='id',all=T) %>%
  merge(.,crt.scores, by='id', all=T) %>%
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
  filter(!(id=='650a0d3e5d77e2efe1142faa'&(Country.of.birth=='DATA_EXPIRED')))%>%
  
  mutate(collectivistic = if_else(Nationality %in% collectivistic_countries &
                                    Country.of.birth %in% collectivistic_countries, 
                                  TRUE, FALSE)) %>%
  mutate(collectivistic = if_else(id == "6679689a61bf2c46ebba0863", TRUE, collectivistic)) %>% # This is a collectivistic person since they were in R1CB1 Asian Nationals, though their data was expired
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

