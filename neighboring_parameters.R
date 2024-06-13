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
