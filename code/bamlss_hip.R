# Load packages ---------------------------------------------------------------------

# Helper
library (tidyverse)

# Exploration 
library (DataExplorer)
library (janitor)
library (arsenal)
library (ggforce)
library (moments)

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

df <- readRDS(file.path(output_dir, "df_hip.RDS"))

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
                binning = TRUE)

saveRDS(mod, "output/bamlss_finalmod2_hip.RDS")