pre_process_vaccine_lookup_table <- function(look_up_table, R0_preds) {

  look_up_table <- look_up_table[,-1]

  max_R0_to_lookup <- ceiling(max(R0_preds))

  new_first_row <- cbind(R0 = 0, look_up_table[1, 2:18])
  new_last_row <- cbind(R0 = max_R0_to_lookup, look_up_table[nrow(look_up_table), 2:18])

  look_up_table_2 <- rbind(new_first_row, look_up_table, new_last_row)

  as.matrix(look_up_table_2)

}
