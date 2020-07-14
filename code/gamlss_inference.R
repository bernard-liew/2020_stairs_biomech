# Load packages ---------------------------------------------------------------------

# Helper
library (tidyverse)

# modelling
library (gamlss)

# plot
library (itsadug)
library(mgcViz)
library (cowplot)

source ("code/helper_fun.R")

# Import data ------------------------------------------------------------------------

smo_list <- list()
jts <- c("ankle", "knee", "hip")

for (n in seq_along(jts)) {
  
  smo_list[[n]] <- getSmo(readRDS(file.path("output", paste0("gamlss_allspeed_", jts[n],"3.RDS")))) 
  
}


# Plot inference -------------------------------------------------------------------

## Cycle inference -----------------------------------------------------------------

### New data -----------------------------------------------------------------------

p_list <- list()

for (n in seq_along(jts)) {
  
  p_list[[n]] <- get_predictions (smo_list[[n]],
                                  cond = list (age = seq(20, 80, 10),
                                               cycle = seq (1, 101, 1),
                                               speed = c(0.5, 1, 1.5),
                                               sex = factor ("m", levels = c("m", "f")),
                                               ht = 1.7),
                                  rm.ranef = TRUE,
                                  se = TRUE)
  
}

names(p_list) <- jts

### Plot -----------------------------------------------------------------------

CPCOLS <- c("#000000", "#33a02c", "#0000FF", "#EE2C2C", "#FF7F24", "#68228B", "#00BFFF")


cyc_pred_plot <- list()

for (n in seq_along(jts)) {
  
  cyc_pred_plot[[n]] <- cycle_infer(p_list[[n]])
  
}

legend <- get_legend(cyc_pred_plot[[1]] +
                       guides(color = guide_legend(nrow = 1)) +
                       theme(legend.direction = "horizontal",
                             legend.justification="center", 
                             legend.box.just = "bottom"))

p <- cowplot::plot_grid(plotlist = map (cyc_pred_plot, ~. + theme(legend.position="none")),
                         nrow = 3,
                        labels="auto" )

p1 <-  plot_grid(p, 
                 legend, 
                 ncol = 1, 
                 rel_heights = c(1, .05)) 

p1
## Peak value inference-------------------------------------------------------------
### New data -----------------------------------------------------------------------

p_list <- list()

for (n in seq_along(jts)) {
  
  p_list[[n]] <- get_predictions (smo_list[[n]],
                                  cond = list (age = seq(20, 85, 1),
                                               cycle = seq (1, 101, 1),
                                               speed = c (0.5, 1, 1.5),
                                               sex = factor ("m", levels = c("m", "f")),
                                               ht = 1.7),
                                  rm.ranef = TRUE,
                                  se = TRUE)
  
}

names(p_list) <- jts
## Find peaks ----------------------------------------------------------------------
a2 <- p_list$ankle %>%
  group_by(age, speed) %>%
  slice (which.max (fit))

h3 <- p_list$hip %>% 
  filter (cycle > 40 & cycle < 70) %>%
  group_by(age, speed) %>%
  slice (which.max (fit))

h1 <- p_list$hip %>% 
  filter (cycle < 40) %>%
  group_by(age, speed) %>%
  slice (which.max (fit))

k2 <- p_list$knee %>% 
  filter (cycle > 10 & cycle < 40) %>%
  group_by(age, speed) %>%
  slice (which.max (fit))

peak_list <- list(a2, k2, h1, h3)


## Plot -------------------------------------------------------------



peak_pred_plot <- list()

for (n in seq_along(peak_list)) {
  
  peak_pred_plot[[n]] <- point_infer(peak_list[[n]])
  
}

p <- plot_grid(plotlist = peak_pred_plot,
                        nrow = 2,
                        labels="auto" )

p
