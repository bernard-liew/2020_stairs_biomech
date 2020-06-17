# Load packages ---------------------------------------------------------------------

# Helper
library (tidyverse)

# modelling
library (gamlss)
library (gamlss.add)
library(gamlss.dist)
library (bamlss)

# parallel
library(doParallel)
library(foreach)
library (furrr)

# Plotting
library (distreg.vis)

# Import data ------------------------------------------------------------------------

output_dir <- "output"

df <- readRDS(file.path(output_dir, "df_clean_allspeed.RDS")) %>%
  filter (joint == "hip") %>% 
  mutate(sex = as.factor(sex),
         subj = as.factor(subj),
         study = as.factor(study))


f <- list (val ~ ti(cycle, speed) +
             ti(cycle, age) + 
             ti(age, speed) + 
             ti(ht, cycle) + 
             s(age) + 
             s(speed) + 
             s(cycle, k = 40) + 
             s(cycle, by = study, bs = 're') +
             s(subj, bs = 're') +
             sex +
             s(ht),
           sigma ~ s(cycle, by = study)) 

# Modelling ------------------------------------------------------------------------
mod <-  bamlss (f,
                data = df,
                family = TF,
                n.iter = 11000, 
                burnin = 1000, 
                thin = 1,
                maxit = 800,
                binning = TRUE)

saveRDS(mod, "output/bamlss_allspeed_hip.RDS")