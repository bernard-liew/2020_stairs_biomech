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

df_bo_ankle %>% arrange(Score) %>% select(k_cycle:Score) %>%  head()
# best score 0.1306451

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

# look at the top configs

df_bo_hip %>% arrange(Score) %>% select(k_cycle:Score) %>% head()
# best score 0.1787804

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


# look at the top configs

df_bo_knee %>% arrange(Score) %>% select(k_cycle:Score) %>%  head()
# best score 0.1525697
