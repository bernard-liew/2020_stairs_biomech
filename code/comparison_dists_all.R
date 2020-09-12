library(tidyverse)
library(ggplot2)

res_hip <- readRDS("output/dist_comparison_hip.RDS")
res_ankle <- readRDS("output/dist_comparison_ankle.RDS")
res_knee <- readRDS("output/dist_comparison_knee.RDS")

fams <- c(
  "NO", "GU", "RG" ,"LO", "NET", "TF", "TF2", "PE","PE2", 
  "SN1", "SN2", "exGAUS", "SHASH", "SHASHo","SHASHo2", 
  "EGB2", "JSU", "JSUo", "SEP1", "SEP2", "SEP3", "SEP4", 
  "ST1", "ST2", "ST3", "ST4", "ST5", "SST", "GT"
)

joint <- rep(c("hip", "ankle", "knee"), each = length(fams))

data.frame(joint = joint, family = fams, 
           score = as.numeric(c(
             unlist(res_hip),
             unlist(res_ankle),
             unlist(res_knee)))) %>% group_by(family) %>% 
  summarise(sum_score = sum(score)) %>% arrange(sum_score)
