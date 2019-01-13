delete_damage <- function(input_DID, current_damage) {
  #current_damage <- ADM_CURRENT_DMG
  current_damage[, dmg_pair := ceiling(seq_len(.N) / 2)]
  delete_pair <- current_damage[DID == input_DID, dmg_pair]
  deleted_data <- current_damage[dmg_pair != delete_pair]
  return(deleted_data)
}
