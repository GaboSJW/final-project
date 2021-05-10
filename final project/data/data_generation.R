#data stimulation
library(tidyverse)
library(lmerTest)  
# for mixed effect models
library(here)
# devtools::install_github("debruine/faux", build_vignettes = TRUE)
library(GGally)    
# makes it easy to plot relationships between variables
options("scipen"=10, "digits"=4) 
# control scientific notation
library(faux) 
library(afex)
# for anova and lmer
library(broom.mixed) 
# to make tidy tables of lmer output
library(kableExtra)
# to make nicer tables
set.seed(8675309) 
# this makes sure your script uses the same set of random numbers each time you run the full script (never set this inside a function or loop)

# Generate LHQ result, make a table
#40subs
#gender 20 - 2-
#age (20.4    SD 2.2)
# age of arrival (0-8)
# CS frequency (1-7, 5.8 SD 1.2)
# LP (1-7, 5.2, SD1.1)
# EN-ES (1-7, 6.1, SD 0.8)

# set parameter
age_sub_n      <- 40
age_acq_sub_n  <- 40
CSF_sub_n      <- 40
LP_sub_n       <- 40
EN_ES_sub_n    <- 40
age_mean       <- 20.4
age_acq_mean   <- 4
CSF_mean       <- 5.8
LP_mean        <- 5.2
EN_ES_mean     <- 6.1
age_sd         <- 2.2
age_acq_sd     <- 2.2
CSF_sd         <- 0.8
LP_sd          <- 0.7
EN_ES_sd       <- 0.5

# generate scores
age_scores     <- rnorm(age_sub_n, age_mean, age_sd)
age_acq_scores <- rnorm(age_acq_sub_n, age_acq_mean, age_acq_sd)
CSF_scores     <- rnorm(CSF_sub_n, CSF_mean, CSF_sd)
LP_scores      <- rnorm(LP_sub_n, LP_mean, LP_sd)
EN_ES_scores   <- rnorm(EN_ES_sub_n, EN_ES_mean, EN_ES_sd)
# create data table
dat_participants <- tibble(
  sub_partcipants_condition = rep( c("age", 
                         "age_acq",
                         "CSF", 
                         "LP", 
                         "EN_ES"), 
                       c(age_sub_n, 
                         age_acq_sub_n, 
                         CSF_sub_n, 
                         LP_sub_n, 
                         EN_ES_sub_n) ),
          score_participants = c(age_scores, 
                    age_acq_scores, 
                    CSF_scores, 
                    LP_scores, 
                    EN_ES_scores)
                          )
write.csv(dat_participants,'dat_participants.csv')
# summerize data
summary_dat_participants <- dat_participants %>%
  group_by(sub_partcipants_condition) %>%
  summarise(n = n() ,
            mean = mean(score_participants),
            sd = sd(score_participants),
            max = max(score_participants),
            min = min(score_participants)
            )
knitr::kable(summary_dat_participants)



# Generate RT results make 4 plots
# Fit GLMM model
# 40 Subs
# 4 groups of results
# 16 trials for each groups of results
# 800-1050mm
# subjects
sub_n  <- 40 # number of subjects in this simulation
sub_sd <- 100 # SD for the subjects' random intercept

sub <- tibble(
  sub_id = 1:sub_n,
  sub_i  = rnorm(sub_n, 0, sub_sd), # random intercept
  sub_cond = rep(c("story1","story2"), each = sub_n/2) # between-subjects factor
)
ggplot(sub, aes(sub_i, color = sub_cond)) +
  geom_density()
# stimuli
stim_n  <- 8 # number of stimuli in this simulation
stim_sd <- 10 # SD for the stimuli's random intercept

stim <- tibble(
  stim_id = 1:stim_n,
  stim_i = rnorm(stim_n, 0, stim_sd) # random intercept
)
ggplot(stim, aes(stim_i)) +
  geom_density()
# trials
trials <- crossing(
  sub_id = sub$sub_id, # get subject IDs from the sub data table
  stim_id = stim$stim_id, # get stimulus IDs from the stim data table
  stim_version = c("type1", "type2", "type3", "type4") # all subjects see all versions of all stimuli
) %>%
  left_join(sub, by = "sub_id") %>% # includes the intercept and conditin for each subject
  left_join(stim, by = "stim_id")   # includes the intercept for each stimulus

# fixed effects
# set variables to use in calculations below
grand_i           <- 800 # overall mean DV
sub_cond_eff      <- 10  # mean difference between conditions: story1 - story2
stim_version_eff  <- 150  # mean difference among versions
cond_version_ixn  <-  0  # interaction between version and condition
error_sd          <- 15  # residual (error) SD

dat <- trials %>%
  mutate(
    # effect-code subject condition and stimulus version
    sub_cond.e = recode(sub_cond, "story1" = -0, "story2" = +0),
    stim_version.e = recode(stim_version, "type1" = -0.5, 
                                          "type2" = +0.5, 
                                          "type3" = +0.9, 
                                          "type4" = +0.5),
    # calculate error term (normally distributed residual with SD set above)
    err = rnorm(nrow(.), 0, error_sd),
    # calculate DV from intercepts, effects, and error
    RT = grand_i + sub_i + stim_i + err +
      (sub_cond.e * sub_cond_eff) + #always 0
      (stim_version.e * stim_version_eff) + 
      (sub_cond.e * stim_version.e * cond_version_ixn) # in this example, this is always 0 and could be omitted
  )
# plot
ggplot(dat, aes(sub_cond, RT, color = stim_version)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.9))

write.csv(dat,'dat.csv')

# sum dat
knitr::kable(dat %>% 
  group_by(., stim_version) %>% 
  summarize(., meanRT = mean(RT), 
               SD = sd(RT), 
               minRT=min(RT), 
               matRT=max(RT)
            ), "simple", caption =  "Results summary of experiment 1")









