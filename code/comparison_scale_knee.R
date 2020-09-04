## check out variance parameters

source("code/settings_to_formula.R")
source("code/measures_johnson.R")

library(mgcv)
library(dplyr)
library(tidyr)
library(parallel)
library(gamlss)
library(gamlss.dist)
library(gamlss.add)

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

form <- as.formula(paste0("val ~ ba(", form, " + s(subj, bs = 're'))"))

fams <- c(
  "NO", "GU", "RG" ,"LO", "NET", "TF", "TF2", "PE","PE2", 
  "SN1", "SN2", "exGAUS", "SHASH", "SHASHo","SHASHo2", 
  "EGB2", "JSU", "JSUo", "SEP1", "SEP2", "SEP3", "SEP4", 
  "ST1", "ST2", "ST3", "ST4", "ST5", "SST", "GT"
)

bestFam <- fams[which.min(unlist(readRDS("output/dist_comparison_knee.RDS")))]

bs = "cr"

sig_formulas <- 
  list(
    ~ 1,
    ~ ba (~ ti(cycle, bs = bs, k = 20)),
    ~ ba (~ ti(cycle, bs = bs, k = 20)+
            ti (speed, k = 5, bs = bs)),
    ~ ba (~ ti(cycle, bs = bs, k = 20)+
            ti (speed, k = 5, bs = bs) +
            ti (cycle, speed, k = c(20, 5),  bs = bs))
  )

mclapply(1:length(sig_formulas), function(i){
  
  mod <- gamlss(form,
                family = bestFam,
                sigma.fo = sig_formulas[[i]],
                discrete = TRUE,
                data = train,
                n.cyc = 200,
                trace = TRUE)
  
  pdf(file = paste0("output/dist_comp_knee_", i, ".pdf"))
  plot(mod)
  dev.off()
  
}, mc.cores = length(sig_formulas))




