delete_damage <- function(input_DID, current_damage) {
  #current_damage <- ADM_CURRENT_DMG
  # print("deldamage")
  # print(input_DID)
  # print(current_damage)
 
  current_damage[, row_no := (seq_len(.N))]
  current_damage[, dmg_pair := ceiling(seq_len(.N) / 2)]
  delete_pair <- current_damage[row_no == input_DID , dmg_pair]
  deleted_data <- current_damage[dmg_pair != delete_pair][, dmg_pair := NULL][, row_no := NULL]
  return(deleted_data)
}
