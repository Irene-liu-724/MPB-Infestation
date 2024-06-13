# mini and extended cluster for hexagons

# For vertical layout hexagons with odd columns down.
# The number of immediate neighbors is 6.
# The number of neighbors in a two-cell radius is 17.
# The indexes depends on whether the column number is even or odd.


#-------------------------------------------------------------------------------
#mini-clusters for each cell
#extended clusters for a group of cells
#create clusters
#-------------------------------------------------------------------------------
#mini cluster (all immediate neighbors) for one infested cell
# x is data frame
mini.cluster = function(x){
  r = x$row
  c = x$col
  if (c %% 2 == 1){
    mini = complete(x,
                    nesting(row = c(rep(r-1, 3), rep(r, 3), r+1),
                            col = c(c-1, c, c+1, c-1, c, c+1, c)),
                    fill = list(density = 0, ind = 0))
  }
  if (c %% 2 == 0){
    mini = complete(x,
                    nesting(row = c(r-1, rep(r, 3), rep(r+1, 3)),
                            col = c(c, c-1, c, c+1, c-1, c, c+1)),
                    fill = list(density = 0, ind = 0))
  }
  return(mini)
}

#-------------------------------------------------------------------------------
# extended cluster (all immediate neighbors) for a group of infested cells
ext.cluster = function(x){
  x = x[which(x$density != 0), ]
  n = nrow(x)
  ext = data.frame()
  for (i in seq_len(n)){
    ext = rbind(ext, mini.cluster(x[i,]))
  }
  ext = ext %>% 
    arrange(row, col, desc(density)) %>%
    distinct(row, col, .keep_all = TRUE)
  return(ext)
}















