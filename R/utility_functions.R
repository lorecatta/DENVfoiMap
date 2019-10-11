loop <- function(..., parallel) {
  if (parallel) {
    parallel::parLapply(NULL, ...)
  } else {
    lapply(...)
  }
}

loop_simplify <- function(..., what) {
  vapply(loop(...), identity, what)
}

write_out_rds <- function(dat, my_path, file_name) {
  
  dir.create(my_path, FALSE, TRUE)
  
  saveRDS(dat, file.path(my_path, file_name))
  
}

write_out_csv <- function(dat, my_path, file_name, ...) {
  
  dir.create(my_path, FALSE, TRUE)
  
  write.csv(dat, 
            file.path(my_path, file_name),
            ...)
  
}

df_to_list <- function (x, use_names) {
  keep <- c("names", "class", "row.names")
  at <- attributes(x)
  attributes(x) <- at[intersect(names(at), keep)]
  ret <- unname(lapply(split(x, seq_len(nrow(x))), as.list))
  if (!use_names) {
    ret <- lapply(ret, unname)
  }
  if (is.character(at$row.names)) {
    names(ret) <- at$row.names
  }
  ret
}

save_plot <- function(plot_obj, out_pth, out_fl_nm, wdt, hgt){
  
  dir.create(out_pth, FALSE, TRUE)
  png(file.path(out_pth, paste0(out_fl_nm, ".png")),
      width = wdt,
      height = hgt,
      units = "cm",
      pointsize = 12,
      res = 300)
  print(plot_obj)
  on.exit(dev.off())
  
}
