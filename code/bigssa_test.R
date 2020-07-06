library (bigsplines)
library (plotly)
library (tidyverse)
library (foreach)
library (doParallel)

# Import data --------------------------------------------------------------------------------
dat <- readRDS("output/df_clean_allspeed.RDS")  %>% 
  filter(study != "lencioni")  # data reported dissimilar to others

## Split into 3 dataframes -------------------------------------------------------------------

ank <- dat  %>%
  filter (joint == "ankle") %>% 
  mutate(sex = as.factor(sex),
         subj = as.factor(subj),
         study = as.factor(study)) %>%
  as.data.frame()

knee <- dat  %>%
  filter (joint == "knee") %>% 
  mutate(sex = as.factor(sex),
         subj = as.factor(subj),
         study = as.factor(study)) %>%
  as.data.frame()

hip <- dat  %>%
  filter (joint == "hip") %>% 
  mutate(sex = as.factor(sex),
         subj = as.factor(subj),
         study = as.factor(study)) %>%
  as.data.frame()

# Modelling  ---------------------------------------------------------------------------------

num_prm <- 0.01
ord_prm <- 1
knots_num <- 100

dat.list <- list (ank, knee, hip)

registerDoParallel(detectCores()-2)

mod.list <- foreach (n = 1:3, .packages = "bigsplines") %dopar% {
  
  bigssa(val ~ sex + cycle*speed*age,
         random = ~ (study|subj) , # subject nested in study
         type = list(sex = "nom", cycle = "per", age = "cub", speed ="cub"),
         rparm = list(cycle = num_prm, speed = num_prm, age = num_prm, sex = ord_prm),
         nknots = knots_num,
         data = dat.list[[n]],
         skip.iter = FALSE)
}

stopImplicitCluster()

saveRDS (mod.list, "output/bigssa_model.RDS")

# Plot residuals  ---------------------------------------------------------------------------------

mod.list <- readRDS("output/bigssa_model.RDS")


pdf ("output/big_ssa_diagnostics.pdf")
par(mfrow = c(2,2))
for (n in 1:3){
  
  s <- summary (mod.list[[n]], fitresid = TRUE)
  plot (s$fitted.values, s$residuals)
}
dev.off()

# Prediction ---------------------------------------------------------------------------------------
nd <- expand.grid(cycle = c(1:101),
                  age = seq (20, 80, 10),
                  speed = c(0.5, 1, 1.5))

registerDoParallel(detectCores()-2)
p <- foreach (n = 1:3, .packages = c("bigsplines", "tidyverse")) %dopar% {
  
  pred <- predict(mod.list [[n]],
          newdata = nd,
          se.fit=TRUE,
          include= c("age", "cycle", "speed"),
          includeint=T)
  
  pred_df <- bind_rows(pred) %>%
    bind_cols(nd) %>%
    mutate (lwr = fit - 2*se.fit,
            upr = fit + 2*se.fit)
  
  return (pred_df)
  
}
stopImplicitCluster()
  

names (p ) <- c("ankle", "knee", "hip")

p <- bind_rows(p, .id = "joint") %>%
  mutate (age = factor (age),
          speed = factor (speed))

# Results  ---------------------------------------------------------------------------------------

f <- p %>%
  ggplot () +
  geom_line(aes (x = cycle, y = fit, colour = age)) +
  geom_ribbon(aes (x = cycle, ymin = lwr, ymax = upr, fill = age), alpha = 0.4) + 
  facet_wrap(joint ~ speed)

windows()
f

a2 <- p %>%
  filter (joint == "ankle") %>%
  group_by(age, speed) %>%
  summarize (fit = max (fit),
             lwr = max (lwr),
             upr = max (upr)) %>%
  ggplot() + 
  geom_point(aes (x = age, y = fit)) +
  geom_errorbar(aes (x = age, ymin = lwr, ymax = upr)) +
  facet_wrap(~ speed)

a2


h3 <- p %>%
  filter (joint == "hip") %>%
  filter (cycle > 50) %>%
  group_by(age, speed) %>%
  summarize (fit = max (fit),
             lwr = max (lwr),
             upr = max (upr)) %>%
  ggplot() + 
  geom_point(aes (x = age, y = fit)) +
  geom_errorbar(aes (x = age, ymin = lwr, ymax = upr)) +
  facet_wrap(~ speed)

h3

h1 <- p %>%
  filter (joint == "hip") %>%
  filter (cycle < 50) %>%
  group_by(age, speed) %>%
  summarize (fit = max (fit),
             lwr = max (lwr),
             upr = max (upr)) %>%
  ggplot() + 
  geom_point(aes (x = age, y = fit)) +
  geom_errorbar(aes (x = age, ymin = lwr, ymax = upr)) +
  facet_wrap(~ speed)

h1

k2 <-p %>%
  filter (joint == "knee") %>%
  filter (cycle > 10 & cycle < 45) %>%
  group_by(age, speed) %>%
  summarize (fit = max (fit),
             lwr = max (lwr),
             upr = max (upr)) %>%
  ggplot() + 
  geom_point(aes (x = age, y = fit)) +
  geom_errorbar(aes (x = age, ymin = lwr, ymax = upr)) +
  facet_wrap(~ speed)

k2