## get the final model on the hold-out data set
rm(list=ls())
gc()

library(mgcv)
library(dplyr)
library(tidyr)
library(parallel)
library(gamlss)
library(gamlss.dist)
library(gamlss.add)

source("code/settings_to_formula.R")
# function to create final model on hold-out set
# per joint
create_final_model <- function(joint = "ankle" , 
                               use_best_family = FALSE,
                               defined_family = "NO",
                               data = NULL, 
                               withRE = FALSE)
{
  
  ############# read hold-out set ##############
  if(is.null(data)){
    data <- readRDS(paste0("output/", joint, "_splitted.RDS"))
    this_data <<- do.call("rbind", data) # set global because gamlss does not find it otherwise
  }else{
    this_data <<- data
  }
  ##############################################
  
  ############# get best mu form ###############
  bo <- readRDS(paste0("output/BO_",joint,".RDS"))
  df_bo <- bo$scoreSummary %>% 
    # drop the ones that did not work
    filter(Score > -100) %>% 
    mutate(Score = -Score)
  
  best_setting <- df_bo %>% 
    arrange(Score) %>% 
    dplyr::select(k_cycle:Score) %>%  
    dplyr::select(k_cycle:k_cycle_re) %>% 
    head(1) %>% c()
  form <- do.call(settings_to_formula, best_setting)
  if(withRE=="subj"){ 
    form <- paste0("val ~ ba(", form, " + s(subj, bs = 're'))") 
  }else if(withRE=="study") 
  {
    form <- paste0("val ~ ba(", form, " + s(study, bs = 're'))") 
  }else{
    form <- paste0("val ~ ba(", form, ")")
  }
  ##############################################
  
  ############# get best family ################
  if(use_best_family)
  {
    
    fams <- c(
      "NO", "GU", "RG" ,"LO", "NET", "TF", "TF2", "PE","PE2", 
      "SN1", "SN2", "exGAUS", "SHASH", "SHASHo","SHASHo2", 
      "EGB2", "JSU", "JSUo", "SEP1", "SEP2", "SEP3", "SEP4", 
      "ST1", "ST2", "ST3", "ST4", "ST5", "SST", "GT"
    )
    
    bestFam <- fams[which.min(unlist(readRDS(paste0("output/dist_comparison_",joint,".RDS"))))]
    
  }else{
    
    bestFam <- defined_family
    
  }
  ##############################################
  
  
  #### define sig form and fit model ###########
  sig_formula <- "~ ba (~ ti(cycle, bs = 'cr', k = 20))"
  
  mod <- gamlss(formula = as.formula(form),
                family = bestFam,
                sigma.fo = as.formula(sig_formula),
                discrete = TRUE,
                data = this_data,
                n.cyc = 200,
                trace = TRUE)
  ##############################################
  
  return(list(mod,form))
  
}

# load original data
dat <- readRDS("output/df_clean_allspeed.RDS") %>%
  filter (cond %in% c("walkt05", "self", "c4")) %>% 
  group_by(subj, speed, age, sex, ht, wt, study)%>% 
  ungroup () %>%
  mutate(sex = as.factor(sex),
         subj = as.factor(subj),
         study = as.factor(study)) %>%
  as.data.frame() %>%
  arrange (study, subj, joint, cond, speed, cycle)  

# ankle
mod_ankle <- create_final_model(joint = "ankle", data = dat %>% filter(joint=="ankle") %>% 
  filter(cycle < 70 & cycle > 20))
pdf("output/plots_ankle.pdf")
# old.par <- par()
# par(old.par)
term.plot(mod_ankle[[1]], what = "mu", ask = FALSE)
term.plot(mod_ankle[[1]], what = "sigma")
try(plot(mod_ankle[[1]]))
dev.off()

# hip
mod_hip <- create_final_model(joint = "hip", data = dat %>% filter(joint=="hip"))
pdf("output/plots_hip.pdf")
# old.par <- par()
# par(old.par)
term.plot(mod_hip[[1]], what = "mu", ask = FALSE)
term.plot(mod_hip[[1]], what = "sigma")
plot(mod_hip[[1]])
dev.off()

# knee
mod_knee <- create_final_model(joint = "knee", data = dat %>% filter(joint=="knee"))
pdf("output/plots_knee.pdf")
# old.par <- par()
# par(old.par)
term.plot(mod_knee[[1]], what = "mu", ask = FALSE)
term.plot(mod_knee[[1]], what = "sigma")
plot(mod_knee[[1]])
dev.off()


save (mod_ankle,
      mod_knee,
      mod_hip,
      file = "output/final_model.RData")
