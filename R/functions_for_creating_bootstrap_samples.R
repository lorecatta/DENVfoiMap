grid_and_boot <- function(i, a, b){
  
  xx <- grid_up(a, b, rnd_dist = FALSE)
  
  yy <- do_boostrap(xx)
  
  cbind(unique_id = seq_len(nrow(yy)), yy)

}

do_boostrap <- function(dataset){
  #browser()
  idx <- unname(split(seq_len(nrow(dataset)), dataset$cell))
  pick <- sample(idx, size = length(idx), replace = TRUE)
  dataset[unlist(pick), ]
}
