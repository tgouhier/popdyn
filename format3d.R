## Format data for easy 3D plotting
## 
## r = format3d (data)
##
##    data = n x 3 dataframe with col1=x, col2=y, col3=z
##    x    = x col
##    y    = y col
##    z    = z col

## Author: Tarik C. Gouhier
## Last modified: June 15, 2007

format3d <- function(data) {
  data <- as.data.frame(data)
  colnames(data) <- c("x", "y", "z")
  x <- sort(unique(data[ , 1]))
  y <- sort(unique(data[ , 2]))
  all.cases <- expand.grid(x = x, y = y)
  all.cases <- merge(all.cases, data, all = TRUE)
  all.cases <- all.cases[order(all.cases[ , 2], all.cases[ , 1]), ]
  z <- matrix(all.cases[ , 3], nrow = length(x), ncol = length(y))
  return (list(x = x, y = y, z = z))
}
