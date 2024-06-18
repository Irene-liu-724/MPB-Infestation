# Birth-death process at the microscale
#periodic growth
#linear death
#pop_size: population size of the beetles
#birth_rate: birth rate
#death_rate: death rate
#tau: number of microscale timesteps between two macroscale timesteps
#tau should be greater than or equal to capacity
#capacity: population capacity in each cell

one_bdp <- function(pop_size, birth_rate, birth_time, death_rate, tau, capacity){
  #pop_size is a vector of population size in the data
  for (i in seq_len(tau)){
    pop_size <- ifelse(pop_size < 1, 0, pop_size)
    death_num <- death_rate * pop_size 
    b_0 <- birth_rate * pop_size * (1 - pop_size/capacity)
    b_1 <- 2/3*b_0
    birth_num <-  b_0 + b_1*sin(2*pi*birth_time/20)
    death_prob <- death_num / tau
    birth_prob <- birth_num / tau
    incr <- rep(0, length(pop_size)) # increment for each time step
    
    for (j in seq_along(pop_size)){
      incr[j] <- sample(c(-1, 0, 1), size = 1, 
                        prob = c(death_prob[j], 1 - birth_prob[j] - death_prob[j],
                                 birth_prob[j]))
    }
    
    pop_size <- pop_size + incr
  }
  return(pop_size)
}

#multiple bdp in each cell, take average of the output
#incell_rep: number of replicates in each cell
birth_death_process <- function(dat, birth_rate, birth_time, death_rate, tau, 
                                capacity, incell_rep){
  pi <- dat$density
  n <- length(pi)
  pop_size <- pi * capacity
  pop_matrix <- matrix(rep(pop_size, incell_rep), ncol = n, byrow = TRUE)
  pop_temp <- apply(pop_matrix, 1, one_bdp, birth_rate = birth_rate, birth_time =
                      birth_time,
                    death_rate = death_rate, tau = tau, capacity = capacity)
  pop_temp_mean <- apply(pop_temp, 1, mean)
  pi_new <- pop_temp_mean / capacity
  dat$density <- pi_new
  return(dat)
}

