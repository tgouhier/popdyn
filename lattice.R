# ---- periodic-boundaries ----
periodic <- function(x, dimension = 128) {
  if (x > dimension)
    x <- x - dimension
  else if (x < 1)
    x <- dimension + x
  return(x)  
}
# ---- coords-1D-from-2D ----
pos.from.coords <- function(x, y, dimension = 128) {
  pos <- y + (x-1) * dimension
  return(pos)
}
# ---- coords-2D-from-1D ----
coords.from.pos <- function(pos, dimension = 128) {
  x <- (pos - 1) %/% dimension + 1
  y <- (pos - 1) %% dimension + 1
  return(list(y = y, x = x))
}
# ---- find-neighbor-coords ----
find.neighs4 <- function(x, y, dimension = 128) {
  xplus <- periodic(x + 1, dimension)
  yplus <- periodic(y + 1, dimension)
  xminus <- periodic(x - 1, dimension)
  yminus <- periodic(y - 1, dimension)
  
  # Return coordinates of neighbors
  return(list(x = c(x, x, xplus, xminus),
              y = c(yplus, yminus, y, y)))
}
# ---- find-neighbor-position ----
find.pos.neighs4 <- function(x, y, dimension = 128) {
  neighs <- find.neighs4(x, y, dimension)
  neighs.mat <- cbind(neighs$x, neighs$y)
  positions <- apply(neighs.mat, 
                     MARGIN = 1, 
                     FUN = function(x, dim = dimension) pos.from.coords(x[1], x[2], dim))
  
  # Return positions of neighbors
  return(positions)
}
# ---- set-neighbors ----
set.neighbors <- function(dimension = 128) {
  size <- dimension^2
  n.neighs <- 4
  neighs <- matrix(nrow = size, ncol = n.neighs + 1)
  for (p in 1:size) {
    # Find coordinates for position p
    p.coords <- coords.from.pos(p, dimension = dimension)
    # Find neighbors for position p
    all.neighs <- find.pos.neighs4(y = p.coords$y, 
                                   x = p.coords$x, 
                                   dimension = dimension)
    neighs[p, ] <- c(p, all.neighs)
  }
  return(neighs)
}
# ---- solve-system ----
solve.rm <- function(t, y, parms) {
  y[y < 0] <- 0
  with(parms, {
    prey <- y[1:parms$size]
    pred <- y[(parms$size + 1):(2 * parms$size)]
    
    prey.migration <- disp * 0.25 * 
      (prey[neighs[ , 2]] + prey[neighs[ , 3]] + 
         prey[neighs[ , 4]] + prey[neighs[ , 5]])
    pred.migration <- disp * 0.25 * 
      (pred[neighs[ , 2]] + pred[neighs[ , 3]] + 
         pred[neighs[ , 4]] + pred[neighs[ , 5]])
    
    delta.prey <- r * prey * (1 - prey / K) - a*prey*pred/(1+ a * h * prey) - 
      disp * prey + prey.migration
    delta.pred <- b * a * prey * pred / (1 + a * h * prey) - m * pred - 
      disp * pred + pred.migration
    return(list(c(delta.prey, delta.pred)))
   })
}
# ---- extract-map ----
get.map.timestep <- function(results, timestep = 1, dimension = 128) {
  prey <- results[timestep, 2:(dimension^2 + 1)]
  pred <- results[timestep, (dimension^2 + 2):(2 * dimension^2 + 1)]
  prey <- matrix(prey, nrow = dimension)
  pred <- matrix(pred, nrow = dimension)
  return(list(prey = prey, pred = pred))
}
# ---- extract-timeseries ----
get.timeseries <- function(results, start.time = 1, end.time = NROW(results), dimension = 128) {
  prey <- results[start.time:end.time, 2:(dimension^2 + 1)]
  pred <- results[start.time:end.time, (dimension^2 + 2):(2 * dimension^2 + 1)]
  return(list(prey = prey, pred = pred))  
}
