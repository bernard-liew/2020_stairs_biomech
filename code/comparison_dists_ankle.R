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
  predMat <- test %>% dplyr::select(-val) %>% spread(cycle, pr) %>% dplyr::select(`21`:`69`) %>% as.matrix()
  score <- mean(relRMSE(trueMat, predMat), na.rm=T)
  
  return(score)
  
}, mc.cores = length(fams))

saveRDS(res, "output/dist_comparison_ankle.RDS")

data.frame(family = fams, score = as.numeric(unlist(res))) %>% arrange(score)

# family     score
# 1       NO 0.1276233
# 2   exGAUS 0.1276581
# 3       LO 0.1287771
# 4      SN2 0.1291027
# 5      SST 0.1291457
# 6      NET 0.1292363
# 7      JSU 0.1294145
# 8       GT 0.1295832
# 9      TF2 0.1295979
# 10      TF 0.1296064
# 11     ST4 0.1297157
# 12     ST1 0.1305030
# 13     ST3 0.1312044
# 14   SHASH 0.1314781
# 15     ST5 0.1315827
# 16    EGB2 0.1318828
# 17    SEP4 0.1320949
# 18    JSUo 0.1322061
# 19 SHASHo2 0.1322354
# 20  SHASHo 0.1322690
# 21     PE2 0.1324795
# 22      PE 0.1326027
# 23    SEP1 0.1343751
# 24     ST2 0.1344306
# 25    SEP3 0.1345648
# 26    SEP2 0.1371019
# 27      GU 0.1454116
# 28      RG 0.1535834
# 29     SN1 0.1728873
