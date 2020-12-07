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
train <- do.call("rbind", data[c(1,3)])
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
  dplyr::select(k_cycle:k_strlen) %>% 
  head(1) %>% c()

form <- do.call(settings_to_formula, best_setting)

form <- as.formula(paste0("val ~ ba(", form, ")"))

form.sigma <- ~ ba( ~ ti(cycle, bs = "cr", k = 20))

res <- mclapply(fams, function(fam){
  
  mod <- gamlss(form, 
                sigma.formula = form.sigma,
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
# 1       NO 0.1981188
# 2      SN1 0.1990626
# 3       LO 0.2005512
# 4      SST 0.2017192
# 5      TF2 0.2017482
# 6       TF 0.2017486
# 7       GT 0.2018871
# 8      JSU 0.2019795
# 9      NET 0.2021791
# 10      PE 0.2024019
# 11     PE2 0.2024728
# 12     ST4 0.2024745
# 13     SN2 0.2027030
# 14  exGAUS 0.2031783
# 15   SHASH 0.2032378
# 16    SEP4 0.2034459
# 17  SHASHo 0.2051328
# 18 SHASHo2 0.2051394
# 19     ST1 0.2060418
# 20    SEP3 0.2069273
# 21     ST3 0.2071535
# 22    EGB2 0.2082343
# 23     ST5 0.2082932
# 24    SEP1 0.2090550
# 25    JSUo 0.2104479
# 26      GU 0.2122669
# 27    SEP2 0.2127214
# 28     ST2 0.2167919
# 29      RG 0.2244590
