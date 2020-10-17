settings_to_formula <- function(
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
  k_strlen,
  k_cycle_strlen_1,
  k_cycle_strlen_2,
  k_cycle_re,
  cycle_age
  ) {
    
  # funargs <- as.list(match.call())[-1]
  # if(any(unlist(funargs)<3 & unlist(funargs)>0)) 
  #   return(do.call("settings_to_formula", lapply(funargs, function(x) pmax(3,x))))
  
  k_cycle <- roundBelow3(k_cycle)
  k_age <- roundBelow3(k_age)
  k_speed <- roundBelow3(k_speed)
  k_cycle_age_1 <- roundBelow3(k_cycle_age_1)
  k_cycle_age_2 <- roundBelow3(k_cycle_age_2)
  k_cycle_speed_1 <- roundBelow3(k_cycle_speed_1)
  k_cycle_speed_2 <- roundBelow3(k_cycle_speed_2)
  k_age_speed_1 <- roundBelow3(k_age_speed_1)
  k_age_speed_2 <- roundBelow3(k_age_speed_2)
  k_cycle_age_speed_1 <- roundBelow3(k_cycle_age_speed_1)
  k_cycle_age_speed_2 <- roundBelow3(k_cycle_age_speed_2)
  k_cycle_age_speed_3 <- roundBelow3(k_cycle_age_speed_3)
  k_cycle_ht_1 <- roundBelow3(k_cycle_ht_1)
  k_cycle_ht_2 <- roundBelow3(k_cycle_ht_2)
  k_ht <- roundBelow3(k_ht)
  k_strlen <- roundBelow3(k_strlen)
  k_cycle_strlen_1 <- roundBelow3(k_cycle_strlen_1)
  k_cycle_strlen_2 <- roundBelow3(k_cycle_strlen_2)
  k_cycle_re <- roundBelow3(k_cycle_re)
  
  form <-  paste0("~ ",
                  "ti (cycle, k = ", k_cycle, ", bs = 'cr') + ",
                  "ti (age, k = ", k_age, ", bs = 'cr') + ",
                  "ti (speed, k = ", k_speed, ", bs = 'cr') + ",
                  "ti(ht, k = ",k_ht, ", bs = 'cr') + ",
                  "ti(strlen, k = ", k_strlen, ", bs = 'cr') + ",
                  "sex ")
  
  if(all(c(k_cycle_age_1, 
           k_cycle_age_2)>0))
    form <- paste0(form, 
                   "+ ti (cycle, age, k = c(",k_cycle_age_1,",", k_cycle_age_2, "), bs = 'cr')")
  if(all(c(k_cycle_speed_1, 
           k_cycle_speed_2)>0))
    form <- paste0(form, 
                   "+ ti (cycle, speed, k = c(",k_cycle_speed_1,",", k_cycle_speed_2, "),  bs = 'cr')")
  if(all(c(k_age_speed_1, 
           k_age_speed_2)>0))
    form <- paste0(form, 
                   "+ ti (age, speed,  k = c(",k_age_speed_1,",", k_age_speed_2, "),  bs = 'cr')")
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
                   "+ ti (cycle, speed, age,  k = c(",k_cycle_age_speed_1,",", k_cycle_age_speed_2,",",
                   k_cycle_age_speed_3,"), bs = 'cr')"
    )
  if(all(c(k_cycle_ht_1, 
           k_cycle_ht_2)>0))
    form <- paste0(form, 
                   "+ ti (cycle, ht, k = c(",k_cycle_ht_1,",", k_cycle_ht_2,"), bs = 'cr')")
  if(all(c(k_cycle_strlen_1, 
           k_cycle_strlen_2)>0))
    form <- paste0(form, 
                   "+ ti (cycle, strlen, k = c(",k_cycle_strlen_1,",", k_cycle_strlen_1,"), bs = 'cr')")
  if(k_cycle_re>0)
    form <- paste0(form, 
                   "+ ti (cycle, k = ", k_cycle_re,", by = study, bs = 're')")
  
  return(form)
  
}

roundBelow3 <- function(x) ifelse(x<3,0,x)
