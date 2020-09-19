
rm (list = ls())

# Helper
library (tidyverse)

# modelling
library (gamlss)
library (gamlss.add)
library(gamlss.dist)

# plot
library (itsadug)
library(mgcViz)

# Plotting
library (cowplot)
library (ggforce)
library (rainbow)

# Original data
dat <- readRDS("output/df_clean_allspeed.RDS") %>%
  filter (cond %in% c("walkt05", "self", "c4")) %>% 
  group_by(subj, speed, age, sex, ht, wt, study)%>% 
  ungroup () %>%
  mutate(sex = as.factor(sex),
         subj = as.factor(subj),
         study = as.factor(study)) %>%
  as.data.frame() %>%
  arrange (study, subj, joint, cond, speed, cycle)  

# Ankle -------------------------------------------------------------------------

dat_train <-  dat %>%
  filter (joint == "ankle")  %>%
  group_by(subj, cond, speed, age, sex, ht, wt, stplen) %>%
  filter (val == max (val))

form <- val ~ ti (age, k = 5, bs = 'cr') + 
              ti (speed, k = 5, bs = 'cr') + 
              ti(ht, k = 5, bs = 'cr') + 
              ti(stplen, k = 5, bs = 'cr') + 
              sex + 
              s (study, bs = 're') + 
              s(subj, bs = 're')

mod <- bam (form,
            data = dat_train)

b <- getViz(mod)
check(b)


speed <- c(1, 1.5)
age <- seq (20, 80, 10)

a2 <- get_predictions(mod, 
                          cond = list(age = age, speed = speed),
                          rm.ranef = TRUE,
                          se = TRUE) %>%
  mutate (age = factor (age),
          speed = factor (speed))

a2_plot <- ggplot (a2) +
  geom_point(aes (x = age, y = fit, group = 1)) +
  geom_line (aes (x = age, y = fit, group = 1)) + 
  geom_errorbar(aes (x = age, ymin = fit - CI, ymax = fit + CI)) + 
  facet_wrap (~ speed, ncol = 2, scales = "fixed") + 
  labs (x = "Age (yo)",
        y = "Power (W/kg)") +
  theme_cowplot()

# Knee -------------------------------------------------------------------------

dat_train <-  dat %>%
  filter (joint == "knee")  %>%
  group_by(subj, cond, speed, age, sex, ht, wt, stplen) %>%
  filter (cycle > 10 & cycle < 40) %>%
  filter (val == max (val))

form <- val ~ ti (age, k = 5, bs = 'cr') + 
              ti (speed, k = 5, bs = 'cr') + 
              ti(ht, k = 5, bs = 'cr') + 
              ti(stplen, k = 5, bs = 'cr') + 
              sex + 
              s( study, bs = 're') + 
              s(subj, bs = 're')

mod <- bam (form,
            data = dat_train)

b <- getViz(mod)
check(b)

k2 <- get_predictions(mod, 
                      cond = list(age = age, speed = speed),
                      rm.ranef = TRUE,
                      se = TRUE) %>%
  mutate (age = factor (age),
          speed = factor (speed))

k2_plot <- ggplot (k2) +
  geom_point(aes (x = age, y = fit, group = 1)) +
  geom_line (aes (x = age, y = fit, group = 1)) + 
  geom_errorbar(aes (x = age, ymin = fit - CI, ymax = fit + CI)) + 
  facet_wrap (~ speed, ncol = 2, scales = "fixed") + 
  labs (x = "Age (yo)",
        y = "Power (W/kg)") +
  theme_cowplot()
 
 # Hip -------------------------------------------------------------------------
 
 # H1 -----------------
 
 dat_train <-  dat %>%
   filter (joint == "hip")  %>%
   group_by(subj, cond, speed, age, sex, ht, wt, stplen) %>%
   filter (cycle > 5 & cycle < 40) %>%
   filter (val == max (val))
 
 form <- val ~ ti (age, k = 11, bs = 'cr') + 
               ti (speed, k = 9, bs = 'cr') + 
               ti(ht, k = 6, bs = 'cr') + 
               ti(stplen, k = 12, bs = 'cr') + 
               sex + 
               ti (age, speed,  k = c(9,3),  bs = 'cr')+ 
               s ( study, bs = 're') + 
               s(subj, bs = 're')
 
 mod <- bam (form,
             data = dat_train)
 b <- getViz(mod)
 check(b)
 
h1 <- get_predictions(mod, 
                       cond = list(age = age, speed = speed),
                       rm.ranef = TRUE,
                       se = TRUE) %>%
   mutate (age = factor (age),
           speed = factor (speed))
 
h1_plot <- ggplot (h1) +
   geom_point(aes (x = age, y = fit, group = 1)) +
   geom_line (aes (x = age, y = fit, group = 1)) + 
   geom_errorbar(aes (x = age, ymin = fit - CI, ymax = fit + CI)) + 
   facet_wrap (~ speed, ncol = 2, scales = "fixed") + 
   labs (x = "Age (yo)",
         y = "Power (W/kg)") +
   theme_cowplot()
 
 # H3 -----------------
 
 dat_train <-  dat %>%
   filter (joint == "hip")  %>%
   group_by(subj, cond, speed, age, sex, ht, wt, stplen) %>%
   filter (cycle > 55 & cycle < 80) %>%
   filter (val == max (val))
 

 mod <- bam (form,
             data = dat_train)
 
 b <- getViz(mod)
 check(b)
 
 h3 <- get_predictions(mod, 
                       cond = list(age = age, speed = speed),
                       rm.ranef = TRUE,
                       se = TRUE) %>%
   mutate (age = factor (age),
           speed = factor (speed))
 
 h3_plot <- ggplot (h3) +
   geom_point(aes (x = age, y = fit, group = 1)) +
   geom_line (aes (x = age, y = fit, group = 1)) + 
   geom_errorbar(aes (x = age, ymin = fit - CI, ymax = fit + CI)) + 
   facet_wrap (~ speed, ncol = 2, scales = "fixed") + 
   labs (x = "Age (yo)",
         y = "Power (W/kg)") +
   theme_cowplot()
 
 pdf (width = 8, height = 5, file = "manuscript/fig5.pdf")
 
 plot_grid (a2_plot + 
              theme(
                axis.text.x = element_blank(),
                axis.title.x = element_blank()),
            k2_plot +
              theme(
                axis.text.x = element_blank(),
                axis.title.x = element_blank()),
            h1_plot,
            h3_plot ,
            nrow = 2,
            ncol = 2,
            vjust = 1.5,
            hjust = 0,
            labels = "auto")
 
 dev.off()