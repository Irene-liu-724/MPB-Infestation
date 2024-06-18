# Birth-death process at the microscale
#periodic growth
#periodic death
#pop_size: population size of the beetles
#b0: base birth rate
#b1: extra birth rate during hot seasons
#d0: base death rate
#d1: extra death rate during cold seasons
#f1: level of birth rate to switch between hot and cold seasons
#f2: level of death rate to switch between hot and cold seasons
#n_cycle: number of cycles in a simulation
#t: macroscale time step
#tau: number of microscale timesteps between two macroscale timesteps
#tau should be greater than or equal to capacity
#t_star: total number of macroscale timesteps
#p: exponent of the birth/death rates, controls the shape of the birth/death rates

one_bdp <- function(pop_size, b0, b1, d0, d1, f1, f2, n_cycle, t, tau, t_star, p){
  #pop_size is a vector of population size in the data
  #t_cycle <- seq(1,t_star/n_cycle,1)
  norm_t <- n_cycle/t_star*t
  for (i in seq_len(100)){
    pop_size <- ifelse(pop_size < 1, 0, pop_size)
    birth_rate <- ifelse(pop_size >0, 
                         ifelse(abs(sin(pi*norm_t)) < f1, b0 + b1*(abs(sin(pi*norm_t)))^p, 
                                b0 + b1*(abs(sin(pi*norm_t)))^(1/p)), 0)
    birth_num <- birth_rate*pop_size * (1 - pop_size/100)
    death_rate <- ifelse(pop_size >0, 
                         ifelse(abs(cos(pi*norm_t)) < f2, d0 + d1*(abs(cos(pi*norm_t)))^p, 
                                d0 + d1*(abs(cos(pi*norm_t)))^(1/p)), 0)
    death_num <- death_rate * pop_size 
    #birth_num <- birth_rate * pop_size
    death_prob <- death_num/tau
    birth_prob <- birth_num/tau
    incr <- rep(0, length(pop_size)) # increment for each time step
    for (j in seq_along(pop_size)){
      incr[j] <- sample(c(-1, 0, 1), size = 1, 
                        prob = c(death_prob[j], 
                                 max(0, 1 - birth_prob[j] - death_prob[j]),
                                 birth_prob[j]))
    }
    
    pop_size <- pop_size + incr
  }
  return(pop_size)
}

#multiple bdp in each cell, take average of the output
#capacity: population capacity in each cell
#incell_rep: number of replicates in each cell
birth_death_process <- function(dat, b0, b1, d0, d1, f1, f2, n_cycle, t, t_star, 
                                p, tau, capacity, incell_rep){
  pi <- dat$density
  n <- length(pi)
  pop_size <- pi * capacity
  pop_matrix <- matrix(rep(pop_size, incell_rep), ncol = n, byrow = TRUE)
  pop_temp <- apply(pop_matrix, 1, one_bdp, b0 = b0, b1 = b1, d0 = d0, d1 = d1, 
                    f1 = f1, f2 = f2, t = t, n_cycle = n_cycle, t_star = t_star, p = p)
  pop_temp_mean <- apply(pop_temp, 1, mean)
  pi_new <- pop_temp_mean / capacity
  dat$density <- pi_new
  return(dat)
}

