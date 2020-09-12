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
    k_cycle_stplen_1,
    k_cycle_re,
    cycle_age
  ) {
    

    form <-  paste0("~ ",
                    "ti (cycle, k = ", k_cycle, ", bs = 'cr') + ",
                    "ti (age, k = ", k_age, ", bs = 'cr') + ",
                    "ti (speed, k = ", k_speed, ", bs = 'cr') + ",
                    "ti(ht, k = ",k_ht, ", bs = 'cr') + ",
                    "stplen + ",
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
    if(k_cycle_stplen_1>0)
      form <- paste0(form, 
                     "+ ti (cycle, by = stplen, k = ", k_cycle_stplen_1, ", bs = 'cr')")
    if(k_cycle_re>0)
      form <- paste0(form, 
                     "+ ti (cycle, k = ", k_cycle_re,", by = study, bs = 're')")
    
    return(form)
  
}
