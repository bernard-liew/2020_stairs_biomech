source("code/settings_to_formula.R")
source("code/measures_johnson.R")

library(mgcv)
library(dplyr)
library(tidyr)
library(parallel)
library(gamlss)
library(gamlss.dist)
library(gamlss.add)

fams <- c(
  "NO", "GU", "RG" ,"LO", "NET", "TF", "TF2", "PE","PE2", 
  "SN1", "SN2", "exGAUS", "SHASH", "SHASHo","SHASHo2", 
  "EGB2", "JSU", "JSUo", "SEP1", "SEP2", "SEP3", "SEP4", 
  "ST1", "ST2", "ST3", "ST4", "ST5", "SST", "GT"
)

# check suitable distributions

## hip
data <- readRDS("output/hip_splitted.RDS")
train <- data[[1]]
test <- data[[2]]
test <- test %>% arrange(subj, cycle)
trueMat <- test %>% spread(cycle, val) %>% dplyr::select(`1`:`101`) %>% as.matrix()

bo_hip <- readRDS("output/BO_hip.RDS")
df_bo_hip <- bo_hip$scoreSummary %>% 
  # drop the ones that did not work
  filter(Score > -100) %>% 
  mutate(Score = -Score) %>% 
  filter(Score < 0.25)

best_setting <- df_bo_hip %>% 
  arrange(Score) %>% 
  dplyr::select(k_cycle:Score) %>%  
  dplyr::select(k_cycle:k_cycle_re) %>% 
  head(1) %>% c()

form <- do.call(settings_to_formula, best_setting)

form <- as.formula(paste0("val ~ ba(", form, ")"))

res <- mclapply(fams, function(fam){
  
  mod <- gamlss(form,
                family = fam,
                discrete = TRUE,
                data = train,
                n.cyc = 200,
                trace = TRUE)
  
  pr <- predict(mod, newdata = test, what = "mu")
  test$pr <- pr
  predMat <- test %>% dplyr::select(-val) %>% spread(cycle, pr) %>% dplyr::select(`1`:`101`) %>% as.matrix()
  score <- mean(relRMSE(trueMat, predMat), na.rm=T)
  
  return(score)
  
}, mc.cores = length(fams))

saveRDS(res, "output/dist_comparison_hip.RDS")

data.frame(family = fams, score = as.numeric(unlist(res))) %>% arrange(score)

# family     score
# 1       LO 0.1784204
# 2      NET 0.1785871
# 3       NO 0.1792777
# 4      SN2 0.1792972
# 5      TF2 0.1794587
# 6       TF 0.1794957
# 7      SN1 0.1798371
# 8       PE 0.1801061
# 9      SST 0.1802301
# 10     ST4 0.1804185
# 11     PE2 0.1804216
# 12    SEP4 0.1805084
# 13      GT 0.1805940
# 14    SEP2 0.1806330
# 15     JSU 0.1809679
# 16    EGB2 0.1816523
# 17     ST1 0.1819887
# 18    SEP3 0.1826011
# 19     ST5 0.1828416
# 20   SHASH 0.1837603
# 21     ST3 0.1839007
# 22    SEP1 0.1843054
# 23    JSUo 0.1845248
# 24  SHASHo 0.1849219
# 25 SHASHo2 0.1850649
# 26     ST2 0.1864950
# 27      GU 0.2051426
# 28  exGAUS 0.2138433
# 29      RG 0.2151027
