# ------------------------------------------------------
# Author: Kyle Parrish
# Date 9/17/23
# This script loads tidy data for plotting and reporting purposes 
# -------------------------------------------------------

pct_df = read.csv(here("data", "tidy", "pct_df_comb.csv"))

long_df = read.csv(here("data", "tidy", "long_df.csv"))
idf = c("INC_J_BEG_010", "INC_J_BEG_05", "L2SPAN_2023_47", "ADV_EN_25")

### RT ~ WM 

long_df %>% 
  ggplot(aes(x = corsi_z, y = log_rt)) + geom_point() + geom_smooth(method = "lm")

rep = pct_df %>% 
  group_by(group, spatial_verb) %>% 
  summarize(mean_c = round(mean(pct_correct), digits = 2), 
            sd_c = round(sd(pct_correct), digits = 2)) 

#### Plots for checking vis. 

means_y = long_df %>% 
  group_by(group, participant, spatial_verb) %>% 
  summarise(mean_rt = mean(log_rt)) %>% 
  filter(spatial_verb == "yes")

means_n = long_df %>% 
  group_by(group, participant, spatial_verb) %>% 
  summarise(mean_rt = mean(log_rt)) %>% 
  filter(spatial_verb == "no")

means = long_df %>% 
  group_by(group, participant) %>% 
  summarise(mean_rt = mean(log_rt)) 



wm_df = long_df %>% 
  select(participant, vmn_z, corsi_z) %>% 
  distinct()


wm_y_df = left_join(means_y, wm_df, by = "participant")

wm_n_df = left_join(means_n, wm_df, by = "participant")

pooled_df = left_join(means, wm_df, by = "participant")

### LOAD MODELS 

l2_corsi_acc = readRDS(here("data", "models", "l2_corsi_acc.rds"))
l2_vwm_acc = readRDS(here("data", "models", "l2_vwm_acc.rds"))
l2_corsi_rt = readRDS(here("data", "models", "l2_corsi_rt.rds"))
l2_vwm_rt = readRDS(here("data", "models", "l2_vwm_rt.rds"))
  
  
mono_corsi_acc = readRDS(here("data", "models", "mono_corsi_acc.rds"))
mono_vwm_acc = readRDS(here("data", "models", "mono_vwm_acc.rds"))
mono_corsi_rt = readRDS(here("data", "models", "mono_corsi_rt.rds"))
mono_vwm_rt = readRDS(here("data", "models", "mono_vwm_rt.rds"))

mcr = conditional_effects(l2_corsi_acc)[["corsi_z:spatial_verb"]]

mca = conditional_effects(mono_corsi_acc)[["corsi_z:spatial_verb"]]


## Visiospatial verbs and verbal working memory

wm_y_df %>% 
  ggplot(aes(x = mean_rt, y = vmn_z, color = group)) + 
  geom_point() + 
  geom_smooth(method = "lm") + theme_minimal() + xlab("Reaction Time") + 
  ylab("Verbal working memory (Z-score)")

## Non-visiospatial verbs and verbal working memory
wm_n_df %>% 
  ggplot(aes(x = mean_rt, y = vmn_z, color = group)) + 
  geom_point() + 
  geom_smooth(method = "lm")

wm_n_df %>% 
#  filter(mean_rt > -2) %>%
  ggplot(aes(x = mean_rt, y = corsi_z, color = group)) + 
  geom_point() + 
  geom_smooth(method = "lm")

wm_y_df %>% 
  filter(mean_rt > -2) %>%
  ggplot(aes(x = mean_rt, y = corsi_z, color = group)) + 
  geom_point() + 
  geom_smooth(method = "lm")

pooled_df %>% 
  filter(!participant %in% idf) %>% 
  ggplot(aes(x = mean_rt, y = corsi_z, color = group)) + 
  geom_point() + 
  geom_smooth(method = "lm")


nm = lm(mean_rt ~ 1, data = pooled_df %>% 
          filter(!participant %in% idf))
gm = lm(mean_rt ~ group, data = pooled_df %>% 
          filter(!participant %in% idf))
im = lm(mean_rt ~ group*corsi_z, data = pooled_df %>% 
          filter(!participant %in% idf))



vs_df = rbind(wm_y_df, wm_n_df)

library(lme4)
nm = lmer(mean_rt ~ 1 + (1 | participant), data = vs_df %>% 
          filter(!participant %in% idf))
gm = lmer(mean_rt ~ group + (1 | participant), data = vs_df %>% 
          filter(!participant %in% idf))
corsim = lmer(mean_rt ~ group + corsi_z + vmn_z + (1 | participant), data = vs_df %>% 
            filter(!participant %in% idf))
verbm = lmer(mean_rt ~ group + corsi_z + spatial_verb + (1 | participant), data = vs_df %>% 
                filter(!participant %in% idf))
grp_wm = lmer(mean_rt ~ group + corsi_z + spatial_verb + group:corsi_z + (1 | participant), data = vs_df %>% 
                filter(!participant %in% idf))
grp_wm_3 = lmer(mean_rt ~ group + corsi_z + spatial_verb + group:corsi_z + group:corsi_z:spatial_verb + 
                (1 | participant), data = vs_df %>% 
                filter(!participant %in% idf))

anova(nm, gm, corsim, verbm, grp_wm, grp_wm_3)

l2_null = l2_mod = lmer(mean_rt ~ 1 + (1 | participant), data = vs_df %>% 
                          filter(!participant %in% idf) %>% filter(group == "L2 learners"))
l2_mod = lmer(mean_rt ~ corsi_z + (1 | participant), data = vs_df %>% 
                filter(!participant %in% idf) %>% filter(group == "L2 learners"))

anova(l2_null, l2_mod)


l2_null = l2_mod = lmer(mean_rt ~ 1 + (1 | participant), data = vs_df %>% 
                          filter(!participant %in% idf) %>% filter(group == "monolinguals"))
l2_mod = lmer(mean_rt ~ corsi_z + (1 | participant), data = vs_df %>% 
                filter(!participant %in% idf) %>% filter(group == "monolinguals"))

anova(l2_null, l2_mod)


vs_df %>% 
  filter(group == "L2 learners") %>% 
  ggplot(aes(x = mean_rt, y = corsi_z, color = group)) + 
  geom_point() + 
  geom_smooth(method = "lm")



vs_df %>% 
  filter(!participant %in% idf) %>% 
  ggplot(aes(x = mean_rt, y = corsi_z)) + 
  geom_point() + 
  geom_smooth(method = "lm")




nm = lm(mean_rt ~ 1, data = pooled_df)
gm = lm(mean_rt ~ group, data = pooled_df)
im = lm(mean_rt ~ group*vmn_z, data = pooled_df)

anova(nm, gm, im)

pooled_df %>% 
  ggplot(aes(x = mean_rt, y = vmn_z, color = group)) + 
  geom_point() + 
  geom_smooth(method = "lm")




vs_df %>% 
  filter(!participant %in% idf) 

# v working mem + rts for all verbs 

cond_df = vs_df %>% filter(!participant %in% idf)

part_nums = cond_df %>% group_by(group) %>% summarize(n = n())

long_df %>% 
  group_by(participant, spatial_verb) %>% 
  summarize(n = n())


vs_df %>% 
  filter(!participant %in% idf) %>% 
 # filter(spatial_verb == "yes") %>% 
  ggplot(aes(y = vmn_z, x = mean_rt, color = group)) + geom_point() + geom_smooth()

# verbal wm + rts for all verbs combinted 


vs_df %>% 
  filter(!participant %in% idf) %>% 
  # filter(spatial_verb == "yes") %>% 
  ggplot(aes(x = spatial_verb, y = mean_rt)) + geom_boxplot()

# visiospatial + spatial verbs 

# visiospatial + nonspatial verbs 

# verbal spatial verbs 

# verbal non-spatial verbs 



nm = lmer(mean_rt ~ 1 + (1 | participant), data = vs_df %>% 
            filter(!participant %in% idf))
gm = lmer(mean_rt ~ group + (1 | participant), data = vs_df %>% 
            filter(!participant %in% idf))
corsim = lmer(mean_rt ~ group + corsi_z + (1 | participant), data = vs_df %>% 
                filter(!participant %in% idf))
verbwm = lmer(mean_rt ~ group + corsi_z + vmn_z + (1 | participant), data = vs_df %>% 
                filter(!participant %in% idf))
verbm = lmer(mean_rt ~ group + corsi_z + vmn_z + spatial_verb + (1 | participant), data = vs_df %>% 
               filter(!participant %in% idf))
grp_wm = lmer(mean_rt ~ group + corsi_z + vmn_z + spatial_verb + group:corsi_z + (1 | participant), data = vs_df %>% 
                filter(!participant %in% idf))
grp_wm_2 = lmer(mean_rt ~ group + corsi_z + vmn_z + spatial_verb + group:corsi_z + group:vmn_z + 
                  (1 | participant), data = vs_df %>% 
                  filter(!participant %in% idf))
grp_wm_3 = lmer(mean_rt ~ group + corsi_z + vmn_z + spatial_verb + group:corsi_z + group:vmn_z + 
                  group:corsi_z:spatial_verb + 
                  (1 | participant), data = vs_df %>% 
                  filter(!participant %in% idf))
grp_wm_3_v = lmer(mean_rt ~ group + corsi_z + vmn_z + spatial_verb + group:corsi_z + group:vmn_z + 
                    group:corsi_z:spatial_verb + 
                    group:vmn_z:spatial_verb +
                    (1 | participant), data = vs_df %>% 
                    filter(!participant %in% idf))

report_models = anova(nm, gm, corsim, verbwm, verbm, grp_wm, grp_wm_2, grp_wm_3, grp_wm_3_v) %>% 
  rename(p_value = `Pr(>Chisq)`)

rep_mod_ci = confint(grp_wm_3_v)

rep_mod_fe = fixef(grp_wm_3_v)

pct_faster = round(1-exp(round(rep_mod_fe[["groupmonolinguals"]], digits = 2)), digits = 2)

fe_grp = round(rep_mod_fe[["groupmonolinguals"]], digits = 2)
