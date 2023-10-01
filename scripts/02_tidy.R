# ------------------------------------------------------
# Author: Kyle Parrish
# Date 9/17/23
# This script tidies the accuracy and RT data for L2 and monolinguals
# -------------------------------------------------------

# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))

# -----------------------------------------------------------------------------


### L2 learners  

vwm_l2 <- dir_ls(here("data", "L2 learners of Spanish", "Verbal WM task"),
                   regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(correct == "true") %>% 
  group_by(userID) %>% 
  summarize(vmn_score = n()) %>% 
  distinct() %>% 
  rename(participant = userID) # 22 ppl


corsi_l2 <- dir_ls(here("data", "L2 learners of Spanish", "Corsi backwards"),
                     regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(final_corsi_span)) %>% 
  select(`participant*`, final_corsi_span) %>% 
  rename(participant = `participant*`) %>% 
  group_by(participant) %>% 
  summarize(corsi_score = min(final_corsi_span)) %>% 
  rename(final_corsi_span = corsi_score) # 28 ppl 

aud_ldt_l2 <- dir_ls(here("data", "L2 learners of Spanish", "Auditory LDT"),
                      regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  select(Experiment, spatial_verb, key_resp_6.corr, participant, key_resp_6.rt) %>% 
  mutate(log_rt = log(as.numeric(key_resp_6.rt))) %>% 
  filter(Experiment == "visuospatial") %>% 
  filter(!is.na(spatial_verb)) %>%
  mutate(group = "L2 learners")
  
length(unique(aud_ldt_l2$participant)) # 25 ppl 


comb_l2_df = aud_ldt_l2 %>% 
  left_join(corsi_l2, by = "participant") %>% 
  left_join(vwm_l2, by = "participant") %>% 
  filter(!is.na(final_corsi_span)) %>% 
  filter(!is.na(vmn_score)) 
  
length(unique(comb_l2_df$participant)) # 18 for all 3 


comb_l2_df %>% 
  group_by(spatial_verb, participant) %>%
  summarise(n = n())
 
pct_df = comb_l2_df %>% 
  group_by(spatial_verb, participant) %>% 
  summarize(pct_correct = sum(as.numeric(key_resp_6.corr))/12) %>% 
  mutate(group = "L2 learners")

### Monolinguals


vwm_mono <- dir_ls(here("data", "Nativos de español", "Verbal WM task"),
                 regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(correct == "true") %>% 
  group_by(userID) %>% 
  summarize(vmn_score = n()) %>% 
  rename(participant = userID)

corsi_mono <- dir_ls(here("data", "Nativos de español", "Corsi backwards"),
                   regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(final_corsi_span)) %>% 
  select(`participant*`, final_corsi_span) %>% 
  rename(participant = `participant*`) %>% 
  group_by(participant) %>% 
  summarize(corsi_score = min(final_corsi_span)) %>% 
  rename(final_corsi_span = corsi_score) 


aud_ldt_mono <- dir_ls(here("data", "Nativos de español", "Auditory LDT"),
                     regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  select(Experiment, spatial_verb, key_resp_6.corr, participant, key_resp_6.rt) %>% 
  mutate(log_rt = log(as.numeric(key_resp_6.rt))) %>% 
  filter(Experiment == "visuospatial") %>% 
  filter(!is.na(spatial_verb)) %>% 
  mutate(group = "monolinguals")


comb_mono_df = aud_ldt_mono %>% 
  left_join(corsi_mono, by = "participant") %>% 
  left_join(vwm_mono, by = "participant") %>% 
  filter(!is.na(final_corsi_span)) %>% 
  filter(!is.na(vmn_score)) 

length(unique(comb_mono_df$participant))


pct_df_mono = aud_ldt_mono %>% 
  group_by(spatial_verb, participant) %>% 
  summarize(pct_correct = sum(as.numeric(key_resp_6.corr))/12) %>% 
  mutate(group = "monolinguals")


pct_df_comb = rbind(pct_df, pct_df_mono) %>% 
  write.csv(here("data", "tidy", "pct_df_comb.csv"))

long_df_comb = rbind(comb_l2_df, comb_mono_df) %>% 
  mutate(vmn_z = (vmn_score-mean(vmn_score))/sd(vmn_score)) %>% 
  mutate(corsi_z = (as.integer(final_corsi_span)-mean(as.integer(final_corsi_span)))/sd(as.integer(final_corsi_span))) %>% 
  write.csv(here("data", "tidy", "long_df.csv"))

  
