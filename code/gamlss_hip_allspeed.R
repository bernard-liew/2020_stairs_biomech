# Load packages ---------------------------------------------------------------------

# Helper
library (dplyr)
library (tidyr)

# modelling
library (gamlss)
library (gamlss.add)
library(gamlss.dist)

# plot
library (itsadug)

# Import data ------------------------------------------------------------------------

frac <- 1

dat <- readRDS("output/df_clean_allspeed_outRm.RDS")  %>%
  filter (joint == "hip") %>% 
  filter(study != "lencioni") %>% # data reported dissimilar to others
  mutate (speed_rd  = round (speed, 1)) %>%
  group_by(subj, study, joint, cond, speed_rd, cycle, sex) %>%
  summarize (val = mean (val),
             speed = mean (speed),
             age = age[1],
             ht = ht[1],
             wt = wt[1]) %>%
  ungroup () %>%
  mutate(sex = as.factor(sex),
         subj = as.factor(subj),
         study = as.factor(study)) %>%
  as.data.frame()

bs <-  "cr"

form <-  val ~ ba (~ ti (cycle, k = 30, bs = bs) +
                     ti (age, k = 5, bs = bs) +
                     ti (speed, k = 5, bs = bs) +
                     ti (cycle, age, k = c(30, 5), bs = bs) +
                     ti (cycle, speed, k = c(30, 5), bs = bs) +
                     ti (age, speed, k = c(5, 5), bs = bs) +
                     ti (cycle, speed, age, k = c(15, 5, 5), bs = bs) + # adds to the computation time
                     ti (cycle, ht, k = c(30, 5), bs = bs) +
                     ti(ht, k = 5, bs = bs) +
                     ti (cycle, k = 15, by = study, bs = "re") + 
                     s(subj, bs = 're') +
                     sex)

# Modelling ------------------------------------------------------------------------
mod <- gamlss(form,
              sigma.fo = ~ ba (~ ti(cycle, bs = bs, k = 30) +
                                 ti(speed, bs = bs, k = 5) +
                                 ti(cycle, speed, k = c(30, 5), bs = bs)),
              nu.fo = ~ ba (~ ti(cycle, bs = bs, k = 30)),
              family = "TF",
              discrete = TRUE,
              data = dat,
              n.cyc = 200,
              trace = TRUE)

# Plot inference -------------------------------------------------------------------

smo <- getSmo(mod)

plot_smooth(smo, view = "cycle", cond = list (age = c(30), speed = 1), n.grid = 101, rm.ranef = TRUE)


saveRDS(mod, "output/gamlss_allspeed_hip_outRm.RDS")
