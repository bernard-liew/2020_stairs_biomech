# Load packages ---------------------------------------------------------------------

# Helper
library (tidyverse)

# modelling
library (gamlss)
library (gamlss.add)
library(gamlss.dist)

# plot
library (itsadug)

# Import data ------------------------------------------------------------------------

frac <- 1

dat <- readRDS("output/df_clean_self_outRm.RDS")  %>%
  filter (joint == "knee") %>% 
  filter(study != "lencioni") %>% # data reported dissimilar to others
  # mutate (speed_rd  = round (speed, 1)) %>%
  # group_by(subj, study, joint, cond, speed_rd, cycle, sex) %>%
  # summarize (val = mean (val),
  #            speed = round(mean (speed),1),
  #            age = age[1],
  #            ht = ht[1],
  #            wt = wt[1]) %>%
  # ungroup () %>%
  mutate(sex = factor(sex),
         subj = factor(subj),
         study = factor(study)) %>%
  as.data.frame()

bs <-  "cr"

form <-  val ~ ba(~ti(cycle, k = 30, bs = bs) + 
                    ti(age, k = 25, bs = bs) + 
                    ti(speed, k = 5, bs = bs) + 
                    ti(cycle, age, k = c(20, 12), bs = bs) + 
                    ti(cycle, speed, k = c(20, 5), bs = bs) + 
                    ti(age, speed, k = c(12, 5), bs = bs) + 
                    ti(cycle, speed, age, k = c(20, 5, 5), bs = bs) + 
                    ti(cycle, ht, k = c(20, 10), bs = bs) + 
                    ti(ht, k = 10, bs = bs) + 
                    ti(cycle, k = 20, by = study, bs = "re") + 
                    s(subj, bs = "re") + 
                    sex) 

# Modelling ------------------------------------------------------------------------
mod <- gamlss(form,
              sigma.fo = ~ ba (~ ti(cycle, bs = bs, k = 20)+
                                 ti (speed, k = 5, bs = bs) +
                                 ti (cycle, speed, k = c(20, 5),  bs = bs)),
              nu.fo = ~ ba (~ ti(cycle, bs = bs, k = 20)),
              family = "TF",
              discrete = TRUE,
              data = dat,
              n.cyc = 200,
              trace = TRUE)

# Plot inference -------------------------------------------------------------------

smo <- getSmo(mod)

plot_smooth(smo, view = "cycle", cond = list (age = c(30), speed = 1), n.grid = 101, rm.ranef = TRUE)


saveRDS(mod, "output/gamlss_self_knee_outRm.RDS")
