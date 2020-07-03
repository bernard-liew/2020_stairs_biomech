# Load packages ---------------------------------------------------------------------

# Helper
library (dplyr)
library (tidyr)

# modelling
library (gamlss)
library (gamlss.add)
library(gamlss.dist)


# Import data ------------------------------------------------------------------------

frac <- 0.3

dat <- readRDS("output/df_clean_allspeed.RDS")  %>%
  filter (joint == "ankle") %>% 
  filter(study != "lencioni") %>% # data reported dissimilar to others
  group_by(subj, speed, age, sex, ht, wt, study)%>%
  sample_frac(frac) %>%
  ungroup () %>%
  mutate(sex = as.factor(sex),
         subj = as.factor(subj),
         study = as.factor(study)) %>%
  as.data.frame()

form <-  "val ~ ba(~
             ti(cycle, speed) +
             ti(cycle, age) + 
             ti(age, speed) + 
             ti(ht, cycle) +
             s(age) + 
             s(speed) + 
             s(cycle, k = 30) + 
             s(cycle, by = study, bs = 're') +
             s(subj, bs = 're') +
             sex +
             s(ht))"

form_scale = "~ ba (~ s(cycle, k = 30, by = study))"
form_nu= "~ ba (~  s(cycle, k = 30, by = study))"

# Modelling ------------------------------------------------------------------------
mod <- gamlss(as.formula (form),
              sigma.fo = as.formula (form_scale),
              #nu.fo = as.formula (form_nu),
              family = "TF",
              data = dat,
              trace = TRUE)

saveRDS(mod, "output/gamlss_allspeed_ankle.RDS")