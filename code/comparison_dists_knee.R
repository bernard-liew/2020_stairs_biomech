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

## knee
data <- readRDS("output/knee_splitted.RDS")
train <- data[[1]]
test <- data[[2]]
test <- test %>% arrange(subj, cycle)
trueMat <- test %>% spread(cycle, val) %>% dplyr::select(`1`:`101`) %>% as.matrix()

bo_knee <- readRDS("output/BO_knee.RDS")
df_bo_knee <- bo_knee$scoreSummary %>% 
  # drop the ones that did not work
  filter(Score > -100) %>% 
  mutate(Score = -Score) %>% 
  filter(Score < 0.25)

best_setting <- df_bo_knee %>% 
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

saveRDS(res, "output/dist_comparison_knee.RDS")

data.frame(family = fams, score = as.numeric(unlist(res))) %>% arrange(score)

# family     score
# 1       NO 0.1454420
# 2      SN2 0.1457254
# 3      SN1 0.1462006
# 4       LO 0.1468011
# 5      NET 0.1484632
# 6   exGAUS 0.1493981
# 7     EGB2 0.1494955
# 8      ST3 0.1497712
# 9      SST 0.1497963
# 10     TF2 0.1498084
# 11     ST2 0.1498148
# 12      TF 0.1498303
# 13     ST4 0.1499901
# 14     ST5 0.1500184
# 15     ST1 0.1502102
# 16     JSU 0.1505610
# 17    JSUo 0.1506189
# 18    SEP4 0.1514374
# 19      GT 0.1514413
# 20      PE 0.1517480
# 21    SEP2 0.1519066
# 22  SHASHo 0.1519179
# 23 SHASHo2 0.1519190
# 24   SHASH 0.1520222
# 25     PE2 0.1520669
# 26    SEP3 0.1522644
# 27    SEP1 0.1524177
# 28      GU 0.1598915
# 29      RG 0.1706999
