## Purpose: Factor Analysis for Prolific SWB Data

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

## Load in functions -----------------------------------------------------------
source('L:/rsmith/lab-members/clavalley/R/functions/fa_prep.R')
source('L:/rsmith/lab-members/clavalley/R/legacy/corrplotplus.R')
## Read in data ----------------------------------------------------------------
df <- read.csv('L:/rsmith/wellbeing/data/prolific/SWB_prolific_data.csv')

neg.affect <- df %>% select(id, starts_with('CTQ_'), PANAS_negaffect, DTS_total,
                            starts_with('STICSA_t'), starts_with('ASI_'), starts_with('DAST'),
                            OASIS_total, PCL_total, PHQ_total, starts_with("TAS_"), MLQ_search,
                            starts_with("STAI_T"), starts_with("VHS"),promis_sleep_raw,
                            PROMIS_social_iso_raw, BFI_neurotic, BIS_total, contains('UPPS'),
                            -CTQ_emoabuse, -contains('_time'), -contains('attncheck'),
                            -ASI_total, -TAS_total, -CTQ_total, -VHS_total)

pos.affect <- df %>% select(id, PANAS_posaffect, NCS_total, starts_with("BAS_"), CRT_correct_answers,
                            starts_with("RYFF"), starts_with("WB_"), starts_with('DPES'), starts_with('HILS'),
                            starts_with('IHS_'), starts_with("MHS_"), starts_with("HOPE_t"),
                            starts_with("SWLS"), starts_with('CIT_'), starts_with("MAIA"),
                            VPS_total, starts_with('WOLF_'), starts_with("SBI"),LOT_total,
                            MLQ_presence, SHS_total, starts_with("SVS"), starts_with("BFI_"), 
                            starts_with("TEPS"), starts_with('DFAS'), promis_emotion_raw,
                            promis_meaning_raw, PROMIS_social_sat_raw, PROMIS_self_efficacy_raw,
                            PROMIS_self_efficacy_manage_raw, starts_with('ladder'), starts_with('FSS'),
                            -BFI_neurotic, -BAS_total,-WB_overall,-HOPE_t_total,-SBI_total,
                            -CIT_total,
                             -contains('_time'), -contains('attncheck'))


neg.fa <- neg.affect %>% select(-id) %>% 
  fa.prep(., plot=F, write=T, output='L:/rsmith/wellbeing/data/prolific/analysis/NegativeAffectFA.csv')
pos.fa <- pos.affect %>% select(-id) %>% 
  fa.prep(., plot=F, write=T, output='L:/rsmith/wellbeing/data/prolific/analysis/PositiveAffectFA.csv')

combined <- merge(neg.affect, pos.affect, by='id', all=T) %>%
  select(-id) %>% fa.prep(., plot=F, write=T, output='L:/rsmith/wellbeing/data/prolific/analysis/CombinedFA.csv')


P.fa <- pos.fa$scores %>% as.data.frame %>% 
  rename_with(~paste0('Pos_', .x)) %>%
  cbind(., select(pos.affect,id))
N.fa <- neg.fa$scores %>% as.data.frame %>% 
  rename_with(~paste0('Neg_', .x)) %>%
  cbind(., select(neg.affect,id))

new.df <- merge(P.fa, N.fa, by='id', all=T) %>% 
  filter(!duplicated(.))%>%
  merge(df,., by='id', all=T)

new.df <- combined$scores %>% as.data.frame %>% 
  cbind(., select(merge(neg.affect, pos.affect, by='id', all=T),id)) %>%
  filter(!duplicated(.))%>%
  merge(df,., by='id', all=T)

new.df %>% select(paste0('PA',1:10),#contains('Pos_PA'), contains('Neg_PA'),
                  starts_with('SM_'), -contains('frac'), -contains('acc'),
                  -contains('_RT_'), -contains('more_info'), -contains('counterbalance'),
                  -contains('spatial'), -SM_has_practice_effects, -SM_CB_Dislike, -SM_CB_Like,
                  -contains('session'), -contains('fit'), -contains('_p_')) %>% 
  corrplotplus(rows=1:ncol(.), cols=1:ncol(.), replace.BF.with.p = T)


new.df %>% select(paste0('PA',1:10),#contains('Pos_PA'), contains('Neg_PA'),
                  starts_with('COP_posterior')) %>% 
  corrplotplus(rows=1:ncol(.), cols=1:ncol(.), replace.BF.with.p = T)
