grid_up <- function(dataset, grid_size, rnd_dist){
  
  rd <- 0
  rd2 <- 0
  
  if (rnd_dist) {
    
    # draw random distance values 
    rd <- runif(n = 1, min = 0, max = grid_size)
    rd2 <- runif(n = 1, min = 0, max = grid_size)
    
  }
  
  # add rd to lat.grid and long.grid variables 
  dataset$lat.grid <- floor((dataset$latitude - rd) / grid_size)
  dataset$long.grid <- floor((dataset$longitude - rd2) / grid_size)
  min.long <- min(dataset$long.grid)
  width.long <- max(dataset$long.grid) - min.long + 1
  min.lat <- min(dataset$lat.grid)
  
  dataset$cell <- (dataset$lat.grid - min.lat) * width.long + dataset$long.grid - min.long
  
  dataset
  
}
