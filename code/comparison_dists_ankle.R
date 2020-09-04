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

## ankle
data <- readRDS("output/ankle_splitted.RDS")
train <- data[[1]]
test <- data[[2]]
test <- test %>% arrange(subj, cycle)
trueMat <- test %>% spread(cycle, val) %>% dplyr::select(`21`:`69`) %>% as.matrix()

bo_ankle <- readRDS("output/BO_ankle.RDS")
df_bo_ankle <- bo_ankle$scoreSummary %>% 
  # drop the ones that did not work
  filter(Score > -100) %>% 
  mutate(Score = -Score) %>% 
  filter(Score < 0.25)

best_setting <- df_bo_ankle %>% 
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
  predMat <- test %>% dplyr::select(-val) %>% spread(cycle, pr) %>% dplyr::select(`21`:`69`) %>% as.matrix()
  score <- mean(relRMSE(trueMat, predMat), na.rm=T)
  
  return(score)
  
}, mc.cores = length(fams))

saveRDS(res, "output/dist_comparison_ankle.RDS")

data.frame(family = fams, score = as.numeric(unlist(res))) %>% arrange(score)

# family     score
# 1      PE2 0.1278581
# 2       LO 0.1292479
# 3     SEP2 0.1297898
# 4     JSUo 0.1307492
# 5      ST2 0.1309096
# 6      ST3 0.1309126
# 7      ST4 0.1309401
# 8      ST5 0.1310988
# 9       TF 0.1313788
# 10     TF2 0.1314001
# 11    SEP1 0.1317841
# 12      NO 0.1319597
# 13     NET 0.1320162
# 14     JSU 0.1328559
# 15      GT 0.1333066
# 16     SN1 0.1333096
# 17     SST 0.1338726
# 18    EGB2 0.1344008
# 19    SEP4 0.1349272
# 20   SHASH 0.1356725
# 21      PE 0.1366461
# 22     ST1 0.1367283
# 23    SEP3 0.1368184
# 24     SN2 0.1431637
# 25  SHASHo 0.1485798
# 26      GU 0.1523686
# 27 SHASHo2 0.1583780
# 28      RG 0.1802263
# 29  exGAUS 0.2276932