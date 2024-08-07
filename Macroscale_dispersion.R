# define neighboring parameter - absorbing boundary condition
#-------------------------------------------------------------------------------
# input x is a dataframe
# neighboring parameter k
incl_wgt <- function(x) { #input x is a cluster
  n <- nrow(x)
  w <- vector()
  
  for (i in seq_len(n)){
    r <- x[i, ]$row
    c <- x[i, ]$col
    # neighboring cells depend on whether column is even or odd
    if (c %% 2 == 1){
      mini_cluster <- complete(x[i, ],
                               nesting(row = c(rep(r-1, 3), rep(r, 2), r+1),
                                       col = c(c-1, c, c+1, c-1, c+1, c)))
    }
    
    if (c %% 2 == 0){
      mini_cluster <- complete(x[i, ],
                               nesting(row = c(r-1, r, r, rep(r+1, 3)),
                                       col = c(c, c-1, c+1, c-1, c+1, c)))
    }
    
    extended_cluster <- merge(mini_cluster, x, all.x = TRUE, by = c("row", "col"))
    w[i] <-  sum(extended_cluster$density.y > 0, na.rm = TRUE)
  }
  
  return(w)
}

#-------------------------------------------------------------------------------
# maximum of relative difference density between adjacent cells
density_rel_diff <- function(x) { # input x is a dataframe
  n <- nrow(x)
  d <- vector()
  for (i in seq_len(n)){
    r <- x[i, ]$row
    c <- x[i, ]$col
    if (c %% 2 == 1){
      mini_cluster <- complete(x[i, ],
                               nesting(row = c(rep(r-1, 3), rep(r, 2), r+1),
                                       col = c(c-1, c, c+1, c-1, c+1, c)))
    }
    
    if (c %% 2 == 0){
      mini_cluster <- complete(x[i, ],
                               nesting(row = c(r-1, r, r, rep(r+1, 3)),
                                       col = c(c, c-1, c+1, c-1, c+1, c)))
    }
    
    extended_cluster <- merge(mini_cluster, x, all.x = TRUE, by = c("row", "col"))
    pi <- x[i, ]$density
    pi_temp_dat <- extended_cluster[!is.na(extended_cluster$density.y), ]
    pi_temp <- pi_temp_dat$density.y
    d[i] <- max(abs(pi - pi_temp))
  }
  
  return(d)
}

#Dispersion process (uniform dispersion)
#x: dataframe of the cell indexes and densities
#alpha: parameter in the inclusion probability
#beta: parameter in the inclusion probability
#gamma1: percentage of population density to be dispersed
#gamma2: percentage of population density to be accepted
#phi: parameter in the survival probability
#rho: parameter in the survival probability
#xi: parameter in the survival probability

sca_model <- function(x, alpha, beta, gamma1, gamma2, phi, rho, xi){ #x is a dataframe
  w <- incl_wgt(x)
  d <- density_rel_diff(x)
  incl_prob <- (w^alpha * d / (1 + w^alpha))^beta
  incl_index <- vector()
  surv_index <- vector()
  
  for (i in seq_along(incl_prob)){
    #flipping coin on cells
    incl_index[i] <- rbinom(1, size = 1, prob = incl_prob[i])} 
  
  if (sum(incl_index) == 0) {x_update <- x  #if all 0s, no dispersion
  } else {
    x$index <- incl_index
    temp <- filter(x, index == 1) #only include selected cells
    avg <- mean(temp$density)
    # contributing cells, lose pop density
    temp_high <- temp[temp$density >= avg, ] 
    # non-contributing cells, receive pop density
    temp_low <- temp[temp$density < avg, ]
    N <- nrow(temp)
    #pop density available to be redistributed
    pop_redistr <- gamma1 * sum(temp_high$density - avg)
    #temporary non-contributing cells receive pop density
    temp_low$density <- with(temp_low, density + gamma2 * pop_redistr / N)
    #survival prob for non-contributing cells
    surv_prob <- phi + rho * atan(xi * temp_low$density)
    for (h in seq_along(surv_prob)){
      surv_index[h] <- rbinom(1, size = 1, prob = surv_prob[h])
    }
    temp_low$surv <- surv_index
    temp_low$density <- ifelse(temp_low$surv == 0, 0, temp_low$density)
    temp_low <- select(temp_low, -surv)
    temp_high$density <- with(temp_high, 
                              density - gamma1 * (density - avg) + pop_redistr / N)
    #updated cell density
    temp_update <- rbind(temp_high, temp_low)
    x_update_ <- temp_update %>% select(-index) %>% arrange(row, col)
    x_update <- merge(x, x_update_, all.x = TRUE, by = c("row", "col"))
    x_update$density <- with(x_update,
                             ifelse(is.na(density.y)==FALSE, density.y, density.x))
    
  }
  x_update <- x_update %>% select(row, col, density)
  return(x_update)
}
#-------------------------------------------------------------------------------
# Dispersion process (directional dispersion)
# moving direction
directional_move <- function(x, direction, dir_prob){ #dir_prob prob of density dispersing to a certain direction
  avg_temp <- mean(x$density)
  avg_row <- mean(x$row)
  avg_col <- mean(x$col)
  
  if (direction == "NA"){
    dir_coef <- rep(1, nrow(x))
  }
  if (direction =="East"){
    dir_coef <- ifelse(x$density < avg_temp & x$col < avg_col, dir_prob, 1)
  }
  if (direction =="West"){
    dir_coef <- ifelse(x$density < avg_temp & x$col > avg_col, dir_prob, 1)
  }
  if (direction =="North"){
    dir_coef <- ifelse(x$density < avg_temp & x$row < avg_row, dir_prob, 1)
  }
  if (direction =="South"){
    dir_coef <- ifelse(x$density < avg_temp & x$row > avg_row, dir_prob, 1)
  }  
  if (direction =="Northeast"){
    dir_coef <- ifelse(x$density < avg_temp & x$col < avg_col & x$row < avg_row, dir_prob, 1)
  }
  if (direction =="Northwest"){
    dir_coef <- ifelse(x$density < avg_temp & x$col > avg_col & x$row < avg_row, dir_prob, 1)
  }  
  if (direction =="Southeast"){
    dir_coef <- ifelse(x$density < avg_temp & x$col < avg_col & x$row > avg_row, dir_prob, 1)
  }
  if (direction =="Southwest"){
    dir_coef <- ifelse(x$density < avg_temp & x$col > avg_col & x$row > avg_row, dir_prob, 1)
  }   
  return(dir_coef)  
}


#-------------------------------------------------------------------------------
sca_model_dir <- function(x, direction, dir_prob, alpha, beta, gamma1, gamma2, phi, rho, xi){
  w <- incl_wgt(x)
  d <- density_rel_diff(x)
  dir_coef <- directional_move(x, direction = direction, dir_prob = dir_prob)
  incl_prob <- (w^alpha * d / (1 + w^alpha))^beta * dir_coef
  incl_index <- vector()
  surv_index <- vector()
  
  for (i in seq_along(incl_prob)){
    #flipping coin on cells
    incl_index[i] <- rbinom(1, size = 1, prob = incl_prob[i])} 
  
  if (sum(incl_index) == 0) {x_update <- x  #if all 0s, no spreading
  } else {
    x$index <- incl_index
    temp <- filter(x, index == 1) #only include certain cells
    avg <- mean(temp$density)
    # contributing cells, lose pop density
    temp_high <- temp[temp$density >= avg, ] 
    # non-contributing cells, receive pop density
    temp_low <- temp[temp$density < avg, ]
    N <- nrow(temp)
    #pop density available to be redistributed
    pop_redistr <- gamma1 * sum(temp_high$density - avg)
    temp_low$density <- with(temp_low, density + gamma2 * pop_redistr / N)
    surv_prob <- phi + rho * atan(xi * temp_low$density)
    for (h in seq_along(surv_prob)){
      surv_index[h] <- rbinom(1, size = 1, prob = surv_prob[h])
    }
    temp_low$surv <- surv_index
    temp_low$density <- ifelse(temp_low$surv == 0, 0, temp_low$density)
    temp_low <- select(temp_low, -surv)
    temp_high$density <- with(temp_high, 
                              density - gamma1 * (density - avg) + pop_redistr / N)
    #updated cell density
    temp_update <- rbind(temp_high, temp_low)
    x_update_ <- temp_update %>% select(-index) %>% arrange(row, col)
    x_update <- merge(x, x_update_, all.x = TRUE, by = c("row", "col"))
    x_update$density <- with(x_update,
                             ifelse(is.na(density.y)==FALSE, density.y, density.x))
    
  }
  x_update <- x_update %>% select(row, col, density)
  return(x_update)
}
