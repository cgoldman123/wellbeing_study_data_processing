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
# read in survey data
surveys <- read.csv('../data/local/SWB_local_redcap.csv')



## ========= Social Media ========
directory <- file.info(list.files('./SocialMedia/output/local/kf',
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
     # mutate(CB = counter) %>%
      mutate(DE = info_bonus_h5 - info_bonus_h1) %>%
      mutate(RE = dec_noise_h5_13 - dec_noise_h1_13) %>%
      rbind(sm.data,.)

  #}
}
names(sm.data) <- ifelse((names(sm.data) != "id" & names(sm.data) != "room_type" & names(sm.data) != "has_practice_effects"), paste("KF", names(sm.data), sep = "_"), names(sm.data))

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
# ad.data <- file.info(list.files('./AdviceTask/output',
#                                     pattern='fits', full.names = T)) %>% 
#   as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
#   arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() %>%
#   rename_with(~paste0('AD_',.), -id)





## =========== Theory of Mind ===========
# tom.1_1_1_0 = file.info(list.files('./TheoryofMind/fits/prolific',
#                                    pattern='_1_1_1_0', full.names = T)) %>% 
#   as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
#   arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() %>%
#   rename(id = subject) %>% as.data.frame(.) %>%   # Rename 'subject' to 'id'
#   rename_with(~paste0('model_1_1_1_0_',.), -id)
# 
# tom.1_1_1_1 = file.info(list.files('./TheoryofMind/fits/prolific',
#                                    pattern='_1_1_1_1', full.names = T)) %>% 
#   as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
#   arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() %>%
#   rename(id = subject) %>% as.data.frame(.) %>%   # Rename 'subject' to 'id'
#   rename_with(~paste0('model_1_1_1_1_',.), -id)
# 
# tom.1_1_1_2 = file.info(list.files('./TheoryofMind/fits/prolific',
#                                    pattern='_1_1_1_2', full.names = T)) %>% 
#   as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
#   arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() %>%
#   rename(id = subject) %>% as.data.frame(.) %>%   # Rename 'subject' to 'id'
#   rename_with(~paste0('model_1_1_1_2_',.), -id)
# 
# tom.data <- inner_join(tom.1_1_1_0, inner_join(tom.1_1_1_1, tom.1_1_1_2, by = "id"), by = "id") %>%
#   rename_with(~paste0('TOM_',.), -id)




## ========= Emotional Faces =========
# model_based_patterns <- c("rho", "sigma", "omega", "kappa", "AIC", "LME", "avg_act", "model", "p_or_r", 
#                           "variance", "mu", "ze", "nu\\b", "be", "h_intensity_sal", "l_intensity_conf")
# base_dir <- './EmotionalFaces/output/prolific'
# # List and filter files containing both 'hgf' and 'predictions'
# file_paths_filtered <- list.files(base_dir, pattern='hgf', full.names = TRUE)
# file_paths_filtered <- file_paths_filtered[grep('predictions', file_paths_filtered)]
# ef.data.pred.model <- file.info(file_paths_filtered) %>%
#   as.data.frame() %>% rownames_to_column() %>% rename(filname='rowname') %>% arrange((mtime)) %>% tail(n=1) %>%
#   pull(filname) %>% read.csv() %>% rename(id='ID') %>%
#   rename_with(~ sapply(.x, function(x) {
#     if (any(sapply(model_based_patterns, function(p) grepl(p, x)))) {
#       paste0('EF_pred_model_', x)
#     } else {
#       paste0('EF_', x)
#     }
#   }), .cols = -id)
# 
# # List and filter files containing both 'hgf' and 'responses'
# file_paths_filtered <- list.files(base_dir, pattern='hgf', full.names = TRUE)
# file_paths_filtered <- file_paths_filtered[grep('responses', file_paths_filtered)]
# ef.data.resp.model <- file.info(file_paths_filtered) %>%
#   as.data.frame() %>%
#   rownames_to_column() %>%
#   rename(filname='rowname') %>%
#   arrange((mtime)) %>%
#   tail(n=1) %>%
#   pull(filname) %>%
#   read.csv() %>%
#   rename(id='ID') %>%
#   rename_with(~ sapply(.x, function(x) {
#     if (any(sapply(model_based_patterns, function(p) grepl(p, x)))) {
#       paste0('EF_resp_model_', x)
#     } else {
#       paste0('EF_', x)
#     }
#   }), .cols = -id)
# 
# ef.data <- inner_join(ef.data.pred.model, ef.data.resp.model, by = "id")
# # drop duplicated cols
# ef.data <- ef.data %>%
#   select(-matches("\\.y$")) %>%  # Remove columns ending with .y
#   rename_with(~ sub("\\.x$", "", .x), matches("\\.x$"))  # Rename columns ending with .x
# 


## ========= Cooperation Task =========

coop.data <- file.info(list.files('./Cooperation/output/local_fits_and_mf',
                                pattern='coop_fit', full.names = T)) %>%
  as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>%
  arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() %>%
  rename_with(~paste0('COP_',.), -id)



## ========= Blind Dating ==========
# bd.data <- file.info(list.files('./BlindDating/output/fits_and_model_free/prolific',
#                                   pattern='BD', full.names = T)) %>% 
#   as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
#   arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() %>%
#   rename(id = subject) %>%  # Rename 'subject' to 'id'
#   rename_with(~paste0('BD_',.), -id)



## ========= Factor Analysis ==========
# factor.data <- file.info(list.files('../data/prolific/factor_scores/',
#                                 pattern='factor_scores', full.names = T)) %>% 
#   as.data.frame %>% rownames_to_column(.) %>% rename(filname='rowname') %>% 
#   arrange(mtime) %>% tail(n=1) %>% pull(filname) %>% read.csv() 


## =========== Combine ============
df <- merge(surveys,coop.data, by='id',all=T) %>%
  #merge(.,ad.data,by='id',all=T)%>%
  #merge(.,ef.data,by='id',all=T)%>%
  merge(.,sm.data,by='id',all=T) %>%
  #merge(.,bd.data,by='id',all=T) %>%
  #merge(.,tom.data,by='id',all=T) %>%
  #merge(.,factor.data,by='id',all=T) %>%
  filter(!duplicated(.)) %>% select(-c("X"))

final = df

# Make sure no participants have duplicated rows
duplicated_ids <- final$id[duplicated(final$id) | duplicated(final$id, fromLast = TRUE)]
duplicates <- final %>% filter(id %in% duplicated_ids)
cat("Number of duplicated subjects in final DF: ", nrow(duplicates ),"\n")
#View(duplicates)

write.csv(final, 'L:/rsmith/wellbeing/data/local/SWB_local_data.csv', row.names = F)
