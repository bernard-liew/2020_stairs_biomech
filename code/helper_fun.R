
extract_from_mat <- function (id, data) {
  
  idx <- id$s
  
  df <- data$s
  
  # demographics
  subj_name <- as.vector (idx[[1]])
  age <- as.vector (idx[[2]])
  sex <- as.vector (idx[[3]])
  ht <- as.vector (idx[[4]])
  wt <- as.vector (idx[[5]])
  
  # biomechanics names
  ang_name <- as.vector (idx[[10]])
  mom_name <- as.vector (idx[[11]])
  grf_name <- as.vector (idx[[12]])
  emg_name <- as.vector (idx[[13]])
  pwr_name <- as.vector (idx[[15]])
  com_name <- c("x", "y", "z")
  
  #conditions
  side_name <- df$Data$Foot
  task_name <- paste(df$Data$Task, c(1:length (df$Data$Task)), sep = "_")
  
  # variables
  speed_name <- df$Data$speed %>% unlist()
  strlen_name <- df$Data$strideLength %>% unlist()
  strwid_name <- df$Data$stepWidth %>% unlist()
  cadence_name <- df$Data$cadence %>% unlist()
  
  ang <- df$Data$Ang %>% 
    `names<-`(task_name) %>% 
    map (as.data.frame) %>% 
    bind_rows(.id = "task") %>% 
    mutate (var_axis = rep (ang_name, times = length (task_name)),
            speed = rep (speed_name, each = length (ang_name)),
            side = rep (side_name, each = length (ang_name)), 
            strlen = rep (strlen_name, each = length (ang_name)), 
            strwid = rep (strwid_name, each = length (ang_name)), 
            cadence = rep (cadence_name, each = length (ang_name))) %>%
    pivot_longer(cols = c("V1":"V101"),
                 names_to = "cycle",
                 values_to = "val") %>%
    mutate (cycle = str_remove(cycle, "V") %>% as.numeric())
  
  mom <- df$Data$Mom %>% 
    `names<-`(task_name) %>% 
    map (as.data.frame) %>% 
    bind_rows(.id = "task") %>% 
    mutate (var_axis = rep (mom_name, times = length (task_name)),
            speed = rep (speed_name, each = length (mom_name)),
            side = rep (side_name, each = length (mom_name)), 
            strlen = rep (strlen_name, each = length (mom_name)), 
            strwid = rep (strwid_name, each = length (mom_name)), 
            cadence = rep (cadence_name, each = length (mom_name))) %>%
    pivot_longer(cols = c("V1":"V101"),
                 names_to = "cycle",
                 values_to = "val") %>%
    mutate (cycle = str_remove(cycle, "V") %>% as.numeric())
  
  pwr <- df$Data$Pwr %>% 
    `names<-`(task_name) %>% 
    map (as.data.frame) %>% 
    bind_rows(.id = "task") %>% 
    mutate (var_axis = rep (pwr_name, times = length (task_name)),
            speed = rep (speed_name, each = length (pwr_name)),
            side = rep (side_name, each = length (pwr_name)), 
            strlen = rep (strlen_name, each = length (pwr_name)), 
            strwid = rep (strwid_name, each = length (pwr_name)), 
            cadence = rep (cadence_name, each = length (pwr_name))) %>%
    pivot_longer(cols = c("V1":"V101"),
                 names_to = "cycle",
                 values_to = "val") %>%
    mutate (cycle = str_remove(cycle, "V") %>% as.numeric())
  
  com <- df$Data$Com %>% 
    `names<-`(task_name) %>% 
    map (as.data.frame) %>% 
    bind_rows(.id = "task") %>% 
    mutate (var_axis = rep (com_name, times = length (task_name)),
            speed = rep (speed_name, each = length (com_name)),
            side = rep (side_name, each = length (com_name)), 
            strlen = rep (strlen_name, each = length (com_name)), 
            strwid = rep (strwid_name, each = length (com_name)), 
            cadence = rep (cadence_name, each = length (com_name))) %>%
    pivot_longer(cols = c("V1":"V101"),
                 names_to = "cycle",
                 values_to = "val") %>%
    mutate (cycle = str_remove(cycle, "V") %>% as.numeric())
  
  biomech <- bind_rows(ang, mom, pwr, com) %>%
    mutate (subj = rep (subj_name, nrow(.)),
            age = rep (age, nrow(.)),
            sex = rep (sex, nrow(.)),
            ht = rep (ht, nrow(.)),
            wt = rep (wt, nrow(.))) %>%
    dplyr::select (subj, age, sex, ht, wt, everything())
  
  df_restruct <- list (subj = subj_name,
                       age = age,
                       sex = sex,
                       ht = ht,
                       wt = wt,
                       side = side_name,
                       task = task_name,
                       speed = speed_name,
                       strlen = strlen_name,
                       strwid = strwid_name,
                       cadence = cadence_name,
                       biomech = biomech)
  
  return (df_restruct)
  
  
  
}

wide_2_long <- function (x) {
  
  subj <- colnames(x)
  
  y <- as.data.frame (t(x)) %>%
    mutate (subj = subj) %>%
    pivot_longer(cols = starts_with("V"),
                 names_to = "cycle",
                 names_prefix = "V",
                 names_ptypes = list (cycle = numeric()),
                 values_to = "val")
  
  
}


gather_reps_cols2rows <- function (x) {
  
  if (ncol(x) > 2) {
    x %>%
      pivot_longer(-ITEM, 
                   names_to = c(".value", "reps"),
                   names_pattern = "(.)_(.*)",
                   values_drop_na = TRUE) 
  } else {
    
    x %>%
      mutate (reps = 1)
    
    
  }
  

}


my_outlier <- function (x) {
  
  Data <- t(x)
  Data = array (Data, dim = c(nrow (Data), ncol(Data), 1))
  Result <- fOutl(Data, type = "fDO", diagnostic = TRUE)
  out <- Result$locOutlX == 1
  outData <- x
  outData[out] <- NA
  
  return(outData)
  
}


select_middle <- function(x) {
  
  if(n_distinct(x$speed) %% 2 == 0 & n_distinct(x$speed) > 2){
    
    slct_speed <- unique (x$speed) 
    speed_med <- slct_speed[((n_distinct(slct_speed)) / 2) - 1]
    x2 <- x %>% filter (speed == speed_med)
    
  } else if (n_distinct(x$speed) %% 2 == 0 & n_distinct(x$speed) == 2) {
    
    slct_speed <- unique (x$speed) 
    speed_med <- slct_speed[n_distinct(slct_speed) - 1]
    x2 <- x %>% filter (speed == speed_med)
    
  } else {
    
    speed_med <- median (unique (x$speed))
    x2 <- x %>% filter (speed == speed_med)
    
  }
  
  return (x2)
  
}

