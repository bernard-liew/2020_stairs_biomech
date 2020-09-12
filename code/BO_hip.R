library(ParBayesianOptimization)
library(mgcv)
library(dplyr)
library(tidyr)
source("code/measures_johnson.R")

# Load data --------------------------------------------------------------------

data <- readRDS("output/hip_splitted.RDS")
train <- data[[1]]
test <- data[[2]]
test <- test %>% arrange(subj, cycle)
# create true functional matrix
# with 49 observations, 1 row = 1 subject
trueMat <- test %>% spread(cycle, val) %>% select(`1`:`101`) %>% as.matrix()
# does not look too good though (?)
matplot(t(trueMat), type="l", col = 
          test %>% 
          filter(cycle==21) %>% 
          pull(study) %>% as.numeric)
rm(data)

# Define objective function ----------------------------------------------------

scoringFunction <- function(
  k_cycle, 
  k_age,
  k_speed,
  k_cycle_age_1,
  k_cycle_age_2,
  k_cycle_speed_1,
  k_cycle_speed_2,
  k_age_speed_1,
  k_age_speed_2,
  k_cycle_age_speed_1,
  k_cycle_age_speed_2,
  k_cycle_age_speed_3,
  k_cycle_ht_1,
  k_cycle_ht_2,
  k_ht,
  k_cycle_stplen_1,
  k_cycle_re,
  cycle_age
) {
  
  
  bs <-  "cr"
  
  form <-  paste0("val ~ ",
                  "ti (cycle, k = k_cycle, bs = bs) + ",
                  "ti (age, k = k_age, bs = bs) + ",
                  "ti (speed, k = k_speed, bs = bs) + ",
                  "ti(ht, k = k_ht, bs = bs) + ",
                  "stplen + ",
                  "sex ")
  
  if(all(c(k_cycle_age_1, 
           k_cycle_age_2)>0))
    form <- paste0(form, 
                   "+ ti (cycle, age, k = c(k_cycle_age_1, k_cycle_age_2), bs = bs)")
  if(all(c(k_cycle_speed_1, 
           k_cycle_speed_2)>0))
    form <- paste0(form, 
                   "+ ti (cycle, speed, k = c(k_cycle_speed_1, k_cycle_speed_2),  bs = bs)")
  if(all(c(k_age_speed_1, 
           k_age_speed_2)>0))
    form <- paste0(form, 
                   "+ ti (age, speed,  k = c(k_age_speed_1, k_age_speed_2),  bs = bs)")
  if(all(c(
    k_cycle_age_1, 
    k_cycle_age_2,
    k_cycle_speed_1, 
    k_cycle_speed_2,
    k_age_speed_1, 
    k_age_speed_2,
    k_cycle_age_speed_1, 
    k_cycle_age_speed_2,
    k_cycle_age_speed_3)>0))
    form <- paste0(form, 
                   "+ ti (cycle, speed, age,  k = c(k_cycle_age_speed_1, k_cycle_age_speed_2, k_cycle_age_speed_3), bs = bs)"
    )
  if(all(c(k_cycle_ht_1, 
           k_cycle_ht_2)>0))
    form <- paste0(form, 
                   "+ ti (cycle, ht, k = c(k_cycle_ht_1, k_cycle_ht_2), bs = bs)")
  if(k_cycle_stplen_1>0)
    form <- paste0(form, 
                   "+ ti (cycle, by = stplen, k = k_cycle_stplen_1, bs = bs)")
  if(k_cycle_re>0)
    form <- paste0(form, 
                   "+ ti (cycle, k = k_cycle_re, by = study, bs = 're')")
  
  form <- as.formula(form)
  
  pr <- try({
    
    mod <- bam (form,
                data = train,
                discrete = TRUE,
                family = scat()
    )
    
    predict(mod, newdata = test)#, exclude = "s(subj)")
    
  })
  
  if(class(pr)=="try-error") score <- NA else{
    
    test$pr <- pr
    predMat <- test %>% select(-val) %>% spread(cycle, pr) %>% select(`1`:`101`) %>% as.matrix()
    score <- -mean(relRMSE(trueMat, predMat), na.rm=T)
    
    
  }
  
  return(list(Score = score))
}

bounds <- list( 
  k_cycle = c(5L,25L), 
  k_age = c(5L,20L),
  k_speed = c(4L,7L),
  k_cycle_age_1 = c(0L,25L),
  k_cycle_age_2 = c(0L,20L),
  k_cycle_speed_1 = c(0L,25L),
  k_cycle_speed_2 = c(0L,6L),
  k_age_speed_1 = c(0L,20L),
  k_age_speed_2 = c(0L,6L),
  k_cycle_age_speed_1 = c(0L,15L),
  k_cycle_age_speed_2 = c(0L,10L),
  k_cycle_age_speed_3 = c(0L,6L),
  k_cycle_ht_1 = c(0L,25L),
  k_cycle_ht_2 = c(0L,15L),
  k_cycle_stplen_1 = c(0L,15L),
  k_ht = c(5L,15L),
  k_cycle_re = c(5L,25L)
)

# Start BO ------------------------------------------------------------------------

nrClusters <- 30
nrEpochs <- 5

library(doParallel)
cl <- makeCluster(nrClusters)
registerDoParallel(cl)
clusterExport(cl,c('train','test','trueMat','relRMSE', 'RMSE', 'integrate_fun'))
clusterEvalQ(cl,expr= {
  library(mgcv)
  library(tidyr)
  library(dplyr)
})

set.seed(42)

tWithPar <- system.time(
  optObj <- bayesOpt(
    FUN = scoringFunction,
    bounds = bounds,
    initPoints = nrClusters,
    iters.n = nrClusters * nrEpochs,
    iters.k = nrClusters,
    parallel = TRUE,
    verbose = 2,
    errorHandling = "continue"
  )
)
stopCluster(cl)
registerDoSEQ()

saveRDS(optObj, file="output/BO_hip.RDS")
print(tWithPar)

getBestPars(optObj)