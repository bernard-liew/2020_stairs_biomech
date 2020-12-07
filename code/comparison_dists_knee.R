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
train <- do.call("rbind", data[c(1,3)])
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

saveRDS(res, "output/dist_comparison_knee.RDS")

data.frame(family = fams, score = as.numeric(unlist(res))) %>% arrange(score)

# family     score
# 1       NO 0.1304466
# 2       PE 0.1304797
# 3       LO 0.1305075
# 4      PE2 0.1306196
# 5       GT 0.1307822
# 6       TF 0.1307914
# 7      TF2 0.1307916
# 8      NET 0.1309095
# 9      ST4 0.1311887
# 10    SEP4 0.1314425
# 11   SHASH 0.1315143
# 12     JSU 0.1317395
# 13     SST 0.1319233
# 14 SHASHo2 0.1323908
# 15  SHASHo 0.1323947
# 16    SEP3 0.1339485
# 17     SN2 0.1343594
# 18     ST3 0.1351703
# 19    EGB2 0.1375302
# 20     ST1 0.1381005
# 21    SEP1 0.1396180
# 22     ST5 0.1426728
# 23    SEP2 0.1435549
# 24    JSUo 0.1455936
# 25      RG 0.1465310
# 26      GU 0.1478137
# 27     ST2 0.1506922
# 28     SN1 0.1684039
# 29  exGAUS        NA
