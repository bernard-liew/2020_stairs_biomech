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
# 1       LO 0.1791893
# 2      NET 0.1800528
# 3   exGAUS 0.1801600
# 4      ST2 0.1803395
# 5     EGB2 0.1803969
# 6      TF2 0.1804509
# 7      ST1 0.1804751
# 8       TF 0.1804765
# 9      ST5 0.1804976
# 10     ST3 0.1805493
# 11     ST4 0.1806252
# 12     SST 0.1808461
# 13      NO 0.1809417
# 14     SN2 0.1809525
# 15     SN1 0.1809912
# 16    JSUo 0.1811765
# 17     JSU 0.1815009
# 18    SEP3 0.1847384
# 19    SEP2 0.1849411
# 20  SHASHo 0.1853010
# 21 SHASHo2 0.1853098
# 22    SEP1 0.1853949
# 23   SHASH 0.1858945
# 24     PE2 0.1866713
# 25    SEP4 0.1868874
# 26      GT 0.1869137
# 27      PE 0.1875112
# 28      RG 0.2043915
# 29      GU 0.2044006
