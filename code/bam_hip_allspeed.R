# Load packages ---------------------------------------------------------------------

# Helper
library (dplyr)

# modelling
library (mgcv)
library (mgcViz)


# Import data ------------------------------------------------------------------------

frac <- 1

dat <- readRDS("output/df_clean_allspeed.RDS")  %>%
  filter (joint == "hip") %>% 
  filter(study != "lencioni") %>% # data reported dissimilar to others
  group_by(subj, speed, age, sex, ht, wt, study)%>%
  sample_frac(frac) %>%
  ungroup () %>%
  mutate(sex = as.factor(sex),
         subj = as.factor(subj),
         study = as.factor(study)) %>%
  as.data.frame()



form <-  val ~ te(cycle, speed, age, k = c(40, 5, 5), bs = c("cr",  "cr",  "cr")) +
  ti(ht, bs = "cr") +
  s(subj, bs = 're') +
  sex

# Modelling ------------------------------------------------------------------------


mod <- bam (form,
            data = dat,
            discrete = TRUE,
            family = scat())

# Diagnostics ----------------------------------------------------------------------
b <- getViz(mod)
check (b)

fit <- fitted(mod)
res <- resid (mod)

plot (fit, res)

saveRDS(mod, "output/bam_allspeed_hip.RDS")
