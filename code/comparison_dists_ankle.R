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
train <- do.call("rbind", data[c(1,3)])
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
# 1       LO 0.1338184
# 2      NET 0.1338200
# 3      PE2 0.1338232
# 4       GT 0.1338554
# 5      TF2 0.1338608
# 6       TF 0.1338617
# 7       PE 0.1339428
# 8      ST4 0.1339991
# 9      SST 0.1342025
# 10     JSU 0.1343304
# 11    SEP4 0.1344540
# 12   SHASH 0.1344589
# 13     ST1 0.1345353
# 14     ST3 0.1346799
# 15  SHASHo 0.1349297
# 16 SHASHo2 0.1349911
# 17     ST5 0.1352110
# 18    SEP1 0.1353058
# 19    JSUo 0.1354620
# 20      NO 0.1355078
# 21    EGB2 0.1358130
# 22    SEP3 0.1358438
# 23    SEP2 0.1362451
# 24     ST2 0.1363298
# 25  exGAUS 0.1366848
# 26     SN2 0.1399939
# 27      RG 0.1475217
# 28      GU 0.1544423
# 29     SN1 0.1822153
