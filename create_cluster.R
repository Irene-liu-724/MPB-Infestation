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
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#Vertical hexagons with odd columns down
# Plot hexagonal grid
hexagon_grid = function(x){  #x is a data frame
  #for a right triangle, a - shortest leg, b - the other leg, c - hypotenuse
  b = tan(pi/3) 
  a = 1 
  c = sqrt(a^2 + b^2)
  #set up coordinates for six vertexes and center point
  v1 = c(0, b)
  v2 = c(a, 2*b)
  v3 = c(a+c, 2*b)
  v4 = c(2*c, b)
  v5 = c(a+c, 0)
  v6 = c(a, 0)
  center = c(c, b)
  
  dat = data.frame()
  for (i in seq_len(nrow(x))){
    id = factor(i)
    row = x[i,]$row
    col = x[i,]$col
    #cluster = x[i,]$cluster
    time = x[i,]$time
    
    #the position of hexagons depend on whether its column number is even or odd
    if (col %% 2 == 1){
      x_shift = 3*(col - 1)/2
      y_shift = 2*(row - 1)
      vertex_x = c(v1[1], v2[1], v3[1], v4[1], v5[1], v6[1]) + center[1]*x_shift
      vertex_y = c(v1[2], v2[2], v3[2], v4[2], v5[2], v6[2]) + center[2]*y_shift
      cpoint_x = center[1] + center[1]*x_shift
      cpoint_y = center[2] + center[2]*y_shift
    } 
    
    if (col %% 2 == 0){
      x_shift = (col - 1)
      y_shift = 2*row - 1
      vertex_x = c(v1[1], v2[1], v3[1], v4[1], v5[1], v6[1]) + (center[1]+0.5*c)*x_shift
      vertex_y = c(v1[2], v2[2], v3[2], v4[2], v5[2], v6[2]) + center[2]*y_shift
      cpoint_x = center[1] + (center[1]+0.5*c)*x_shift
      cpoint_y = center[2] + center[2]*y_shift
    }
    
    values = data.frame(
      ID = rep(id, each = 6),
      #cluster = rep(cluster, each = 6),
      time = rep(time, each = 6),
      row = rep(x[i,]$row, each = 6),
      col = rep(x[i,]$col, each = 6),
      density = rep(x[i,]$density, each = 6),
      vx = vertex_x,
      vy = vertex_y,
      cpointx = rep(cpoint_x, each = 6),
      cpointy = rep(cpoint_y, each = 6)      )
    
    dat = rbind(dat, values)
  }
  
  return(dat)
}














