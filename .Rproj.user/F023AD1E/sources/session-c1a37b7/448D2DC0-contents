# Source libs -----------------------------------------------------------
source(here::here("scripts", "00_libs.R"))

# load data  ------------------------------------------------------------
source(here::here("scripts", "03_load_data.R"))

##### ACC mods 


null_mod = lme4::glmer(formula = as.numeric(key_resp_6.corr) ~ 1 + (1 | participant), 
            family = binomial(), 
            data = long_df)


grp_mod = lme4::glmer(formula = as.numeric(key_resp_6.corr) ~ group + (1 | participant), 
                       family = binomial(), 
                       data = long_df)


corsi_mod = lme4::glmer(formula = as.numeric(key_resp_6.corr) ~ group + corsi_z + (1 | participant), 
                      family = binomial(), 
                      data = long_df)


int_mod = lme4::glmer(formula = as.numeric(key_resp_6.corr) ~ group + corsi_z + group:corsi_z + (1 | participant), 
                        family = binomial(), 
                        data = long_df)

anova(null_mod, grp_mod, corsi_mod, int_mod)

#####

null_mod_rt = lme4::glmer(formula = log_rt ~ 1 + (1 | participant), 
                       data = long_df)


grp_mod_rt = lme4::glmer(formula = log_rt ~ group + (1 | participant), 
                      data = long_df)


corsi_mod_rt = lme4::glmer(formula = log_rt ~ group + corsi_z + (1 | participant), 
                        data = long_df)


int_mod_rt = lme4::glmer(formula = log_rt ~ group + corsi_z + group:corsi_z + (1 | participant), 
                      data = long_df)

anova(null_mod_rt, grp_mod_rt, corsi_mod_rt, int_mod_rt)





null_mod = lme4::glmer(formula = as.numeric(key_resp_6.corr) ~ 1 + (1 | participant), 
                       family = binomial(), 
                       data = long_df)


grp_mod = lme4::glmer(formula = as.numeric(key_resp_6.corr) ~ group + (1 | participant), 
                      family = binomial(), 
                      data = long_df)


corsi_mod = lme4::glmer(formula = as.numeric(key_resp_6.corr) ~ group + vmn_z + (1 | participant), 
                        family = binomial(), 
                        data = long_df)


int_mod = lme4::glmer(formula = as.numeric(key_resp_6.corr) ~ group + vmn_z + group:vmn_z + (1 | participant), 
                      family = binomial(), 
                      data = long_df)

anova(null_mod, grp_mod, corsi_mod, int_mod)

#####

null_mod_rt = lme4::glmer(formula = log_rt ~ 1 + (1 | participant), 
                          data = long_df)


grp_mod_rt = lme4::glmer(formula = log_rt ~ group + (1 | participant), 
                         data = long_df)


corsi_mod_rt = lme4::glmer(formula = log_rt ~ group + vmn_z + (1 | participant), 
                           data = long_df)


int_mod_rt = lme4::glmer(formula = log_rt ~ group + vmn_z + group:vmn_z + (1 | participant), 
                         data = long_df)

anova(null_mod_rt, grp_mod_rt, corsi_mod_rt, int_mod_rt)


library(emmeans)
emmip(int_mod_rt, vmn_z) 


### L2 corsi 
corsi_mod_acc_l2 = lme4::glmer(formula = as.numeric(key_resp_6.corr) ~ spatial_verb:corsi_z + (spatial_verb*corsi_z | participant), 
                              family = binomial(), 
                              data = long_df %>% filter(group == "L2 learners"))


### L2 corsi 
corsi_mod_acc_l2 = lme4::lmer(as.numeric(key_resp_6.corr) ~ spatial_verb:corsi_z + (spatial_verb*corsi_z | participant), 
                       family = bernoulli(link = "logit"), 
                       data = long_df %>% filter(group == "L2 learners"), 
                       file = here("data", "models", "l2_corsi_acc.rds"))


### L2 corsi 
corsi_mod_acc_l2 = brm(as.numeric(key_resp_6.corr) ~ spatial_verb:corsi_z + (spatial_verb*corsi_z | participant), 
                       family = bernoulli(link = "logit"), 
                       data = long_df %>% filter(group == "L2 learners"), 
                       file = here("data", "models", "l2_corsi_acc.rds"))
### mono corsi 
corsi_mod_acc_l2 = brm(as.numeric(key_resp_6.corr) ~ spatial_verb:corsi_z + (spatial_verb*corsi_z | participant), 
                       family = bernoulli(link = "logit"), 
                       data = long_df %>% filter(group == "monolinguals"), 
                       file = here("data", "models", "mono_corsi_acc.rds"))

### L2 vwm 
vwm_mod_acc_l2 = brm(as.numeric(key_resp_6.corr) ~ spatial_verb:vmn_z + (spatial_verb*vmn_z | participant), 
                       family = bernoulli(link = "logit"), 
                       data = long_df %>% filter(group == "L2 learners"), 
                       file = here("data", "models", "l2_vwm_acc.rds"))
### mono vwm 
vwm_mod_acc_l2 = brm(as.numeric(key_resp_6.corr) ~ spatial_verb:vmn_z + (spatial_verb*vmn_z | participant), 
                       family = bernoulli(link = "logit"), 
                       data = long_df %>% filter(group == "monolinguals"), 
                       file = here("data", "models", "mono_vwm_acc.rds"))


##### RT mods 

### L2 corsi 
corsi_mod_acc_l2 = brm(log_rt ~ spatial_verb:corsi_z + (spatial_verb*corsi_z | participant), 
                       data = long_df %>% filter(group == "L2 learners"), 
                       file = here("data", "models", "l2_corsi_rt.rds"))
### mono corsi 
corsi_mod_acc_l2 = brm(log_rt ~ spatial_verb:corsi_z + (spatial_verb*corsi_z | participant), 
                       data = long_df %>% filter(group == "monolinguals"), 
                       file = here("data", "models", "mono_corsi_rt.rds"))

### L2 vwm 
vwm_mod_acc_l2 = brm(log_rt ~ spatial_verb:vmn_z + (spatial_verb*vmn_z | participant), 
                     data = long_df %>% filter(group == "L2 learners"), 
                     file = here("data", "models", "l2_vwm_rt.rds"))
### mono vwm 
vwm_mod_acc_l2 = brm(log_rt ~ spatial_verb:vmn_z + (spatial_verb*vmn_z | participant), 
                     data = long_df %>% filter(group == "monolinguals"), 
                     file = here("data", "models", "mono_vwm_rt.rds"))



long_df %>% 
  ggplot(aes(x = log_rt, y = corsi_z)) + geom_point()


long_df %>% 
  ggplot(aes(x = log_rt, y = spatial_verb, fill = group)) + geom_boxplot()


long_df %>% 
  ggplot(aes(x = log_rt, y = corsi_z, color = group)) + geom_jitter(alpha = .1) + 
  facet_wrap(~spatial_verb) + geom_smooth(method = "lm")


long_df %>% 
  ggplot(aes(x = log_rt, y = vmn_z, color = spatial_verb)) + geom_jitter(alpha = .1) + 
  geom_smooth(method = "lm") + facet_wrap(~participant) + xlim(-2,2) + ylim(-1,1)


long_df %>% 
  ggplot(aes(x = log_rt, y = corsi_z, color = spatial_verb)) + geom_jitter(alpha = .1) + 
  geom_smooth(method = "lm") + facet_wrap(~participant) 

long_df %>% 
  group_by(group, spatial_verb) %>% 
  summarize(mean_rt = mean(log_rt))

