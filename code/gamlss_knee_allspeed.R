# Load packages ---------------------------------------------------------------------

# Helper
library (dplyr)
library (tidyr)

# modelling
library (gamlss)
library (gamlss.add)
library(gamlss.dist)


# Import data ------------------------------------------------------------------------

frac <- 1

dat <- readRDS("output/df_clean_allspeed.RDS") %>%
  filter (joint == "knee") %>% 
  filter(study != "lencioni") %>% # data reported dissimilar to others
  group_by(subj, speed, age, sex, ht, wt, study)%>% 
  sample_frac(frac) %>%
  ungroup () %>%
  mutate(sex = as.factor(sex),
         subj = as.factor(subj),
         study = as.factor(study)) %>%
  as.data.frame()


form <-  val ~ ba(~
                    te(cycle, speed, ht, age, d = c(3, 1), k = c(40, 5), bs = c("cr",  "cr")) +
                    #ti(ht, bs = "cr") +
                    s(subj, bs = 're') +
                    sex) 

# Modelling ------------------------------------------------------------------------
mod <- gamlss(form,
              sigma.fo = ~ ba (~ ti(cycle, bs = "cr", k = 40, by = study)),
              family = "TF",
              discrete = TRUE,
              data = dat,
              n.cyc = 50,
              trace = TRUE)


saveRDS(mod, "output/gamlss_allspeed_knee.RDS")
