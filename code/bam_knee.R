# Load packages ---------------------------------------------------------------------

# Helper
library (tidyverse)
library (splitTools)

# modelling
library (mgcv)

# plots
library (mgcViz)
library(itsadug)

# Import data ------------------------------------------------------------------------

age_cut <- seq (0, 90, 30) # Generates the most even split based on visual inspection
age_labels <- paste0 (paste (age_cut, "-", age_cut[-1])[-length (age_cut)], " (yo)")

ID <- readRDS("output/df_clean_self_outRm.RDS")  %>%
  filter (joint == "knee") %>% 
  filter(study != "lencioni") %>% # data reported dissimilar to others
  mutate (age_cat  = cut (age, age_cut, labels = age_labels)) %>%
  filter (cycle == 1) %>%
  select (subj,  age_cat) %>%
  unique() 

# Generate split ID 40% train, 30% test, 30% validate ----------------------------------
split_id <- partition (ID$age_cat, p = c(0.4, 0.3, 0.3), type = "stratified")

id_train <- ID[split_id[[1]], ] %>%
  pull (subj)
id_test <- ID[split_id[[2]], ]%>%
  pull (subj)
id_val <- ID[split_id[[3]], ]%>%
  pull (subj)

rm (ID)

dat <- readRDS("output/df_clean_self_outRm.RDS")  %>%
  filter (joint == "knee") %>% 
  filter(study != "lencioni") %>% # data reported dissimilar to others
  mutate (speed_rd  = round (speed, 1)) %>%
  group_by(subj, study, joint, cond, speed_rd, cycle, sex) %>%
  summarize (val = mean (val),
             speed = round(mean (speed),1),
             age = age[1],
             ht = round (ht[1],2),
             wt = wt[1]) %>%
  ungroup () %>%
  mutate(sex = factor(sex)) 

# Split data ----------------------------------------------------------------------

dat_train <- dat %>%
  filter (subj %in% id_train) %>%
  mutate(subj = factor(subj),
         study = factor(study)) %>%
  as.data.frame()

dat_test <- dat %>%
  filter (subj %in% id_test)%>%
  mutate(subj = factor(subj),
         study = factor(study)) %>%
  as.data.frame()

dat_val <- dat %>%
  filter (subj %in% id_val) %>%
  mutate(subj = factor(subj),
         study = factor(study)) %>%
  as.data.frame()

# Build formula --------------------------------------------------------------------


bs <-  "cr"
n_cyc <- length (unique(dat$cycle))
n_age <- length (unique(dat$age))
n_speed <- length (unique(dat$speed))
n_ht <- length (unique(dat$ht))

form <-  val ~  ti (cycle, k = 40, bs = bs) +
  ti (age, k = 30, bs = bs) +
  ti (speed, k = 7, bs = bs) +
  ti (cycle, age, k = c(30, 15), bs = bs) +
  ti (cycle, speed, k = c(30, 5),  bs = bs) +
  ti (age, speed,  k = c(15, 5),  bs = bs) +
  ti (cycle, speed, age,  k = c(20, 5, 7), bs = bs) + # adds to the computation time
  ti (cycle, ht, k = c(20, 10), bs = bs) +
  ti(ht, k = 10, bs = bs) +
  ti (cycle, k = 20, by = study, bs = "re") + 
  s(subj, bs = 're') +
  sex

# Modelling ------------------------------------------------------------------------

system.time(
  
  mod <- bam (form,
              data = dat_train,
              discrete = TRUE,
              family = scat())
)
# Diagnostics ----------------------------------------------------------------------


b <- getViz(mod)
check (b)

fit <- fitted(mod)
res <- resid (mod)

plot (fit, res)

# Plot inference -------------------------------------------------------------------

plot_smooth(mod, view = "cycle", cond = list (age = c(30), speed = 1), n.grid = 101, rm.ranef = TRUE)

saveRDS(mod, "output/bam_selfspeed_knee_outRm.RDS")

