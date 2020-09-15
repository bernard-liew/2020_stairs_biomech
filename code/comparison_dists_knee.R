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
# 1       NO 0.1364879
# 2       LO 0.1373469
# 3       GT 0.1378533
# 4      NET 0.1378995
# 5       TF 0.1380209
# 6      TF2 0.1380227
# 7       PE 0.1387909
# 8      ST4 0.1388501
# 9      PE2 0.1390169
# 10   SHASH 0.1400204
# 11    SEP4 0.1407437
# 12     JSU 0.1408233
# 13     SST 0.1408523
# 14 SHASHo2 0.1422784
# 15  SHASHo 0.1422786
# 16     SN2 0.1456457
# 17    SEP3 0.1463297
# 18     ST3 0.1476345
# 19      GU 0.1539753
# 20      RG 0.1567152
# 21    EGB2 0.1589891
# 22    SEP2 0.1623936
# 23     ST1 0.1691913
# 24    SEP1 0.1693511
# 25     ST2 0.1696349
# 26    JSUo 0.1749583
# 27     ST5 0.1783186
# 28     SN1 0.1854085
# 29  exGAUS        NA
