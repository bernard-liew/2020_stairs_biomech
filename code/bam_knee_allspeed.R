# Load packages ---------------------------------------------------------------------

# Helper
library (dplyr)

# modelling
library (mgcv)

# plots
library (mgcViz)
library(itsadug)


# Import data ------------------------------------------------------------------------

frac <- 1

dat <- readRDS("output/df_clean_allspeed.RDS")  %>%
  filter (joint == "knee") %>% 
  filter(study != "lencioni") %>% # data reported dissimilar to others
  group_by(subj, speed, age, sex, ht, wt, study)%>%
  sample_frac(frac) %>%
  ungroup () %>%
  mutate(sex = as.factor(sex),
         subj = as.factor(subj),
         study = as.factor(study)) %>%
  as.data.frame()



bs <-  "cr"

form <-  val ~  ti (cycle, k = 30, bs = bs) +
  ti (age, k = 5, bs = bs) +
  ti (speed, k = 5, bs = bs) +
  ti (cycle, age, k = c(30, 5), bs = bs) +
  ti (cycle, speed, k = c(30, 5), bs = bs) +
  ti (age, speed, k = c(5, 5), bs = bs) +
  ti (cycle, speed, age, k = c(15, 5, 5), bs = bs) + # adds to the computation time
  ti (cycle, ht, k = c(15, 5), bs = bs) +
  ti(ht, k = 5, bs = bs) +
  ti (cycle, k = 10, by = study, bs = "re") + 
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

# Plot inference -------------------------------------------------------------------

plot_smooth(mod, view = "cycle", cond = list (age = c(30), speed = 1), n.grid = 101, rm.ranef = TRUE)

saveRDS(mod, "output/bam_allspeed_knee2.RDS")
