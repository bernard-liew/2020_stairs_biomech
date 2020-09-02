# script to analyze BO results

library(ggplot2)
library(tidyverse)

##############################################################################
## ankle

bo_ankle <- readRDS("output/BO_ankle.RDS")
df_bo_ankle <- bo_ankle$scoreSummary %>% 
  # drop the ones that did not work
  filter(Score > -100) %>% 
  mutate(Score = -Score) %>% 
  filter(Score < 0.25)

df_bo_ankle %>% gather(key = "parameter", value = "value",
                       k_cycle:k_cycle_re) %>% 
  ggplot(aes(x = parameter, y = value, colour = Score)) + 
  geom_point(alpha = 0.3)

# => there does not seem to be a clear picture if 
# each parameter is checked on its own.

# look at the top configs

df_bo_ankle %>% arrange(Score) %>% select(k_cycle:Score) %>%  head()
# best score 0.2034274

# => 
# - cycle effects with max value, 
# - age effects with min value, 
# - speed effects with min value, 
# - ht effects with max value
# - cycle re min value
# - cycle+speed interaction
# - drop all other interactions
# seem to perform best.

##############################################################################
## hip

bo_hip <- readRDS("output/BO_hip.RDS")
df_bo_hip <- bo_hip$scoreSummary %>% 
  # drop the ones that did not work
  filter(Score > -100) %>% 
  mutate(Score = -Score) 

df_bo_hip %>% gather(key = "parameter", value = "value",
                       k_cycle:k_cycle_re) %>% 
  ggplot(aes(x = parameter, y = value, colour = Score)) + 
  geom_point(alpha = 0.3)

# => there does not seem to be a clear picture if 
# each parameter is checked on its own.

# look at the top configs

df_bo_hip %>% arrange(Score) %>% select(k_cycle:Score) %>% head()
# best score 0.1934729

# => 
# - cycle effects with max value, 
# - age effects with min value, 
# - speed effects with min value, 
# - ht effects with min value
# - cycle+age interaction with minimal flexibility
# - cylce+speed with max/min value
# - age+speed interaction with minimal value
# - 3-way TP with max cycle
# - cycle+height at min
# - ht at min
# - cylce re at min
# seem to perform best.

##############################################################################
## knee

bo_knee <- readRDS("output/BO_knee.RDS")
df_bo_knee <- bo_knee$scoreSummary %>% 
  # drop the ones that did not work
  filter(Score > -100) %>% 
  mutate(Score = -Score) %>% 
  filter(Score < 0.25)

df_bo_knee %>% gather(key = "parameter", value = "value",
                       k_cycle:k_cycle_re) %>% 
  ggplot(aes(x = parameter, y = value, colour = Score)) + 
  geom_point(alpha = 0.3)

# => there does not seem to be a clear picture if 
# each parameter is checked on its own.

# look at the top configs

df_bo_knee %>% arrange(Score) %>% select(k_cycle:Score) %>%  head()
# best score 0.1623965

# => 
# - all max except for speed, drop cycle+ht
# value seems to perform best.

# => here we could 