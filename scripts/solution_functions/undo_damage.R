undo_damage <- function(current_dmg, dmg_input_omistaja) {
  #current_dmg <-ADM_CURRENT_DMG
  #dmg_input_omistaja <- "Lauri"
  max_DID <- current_dmg[Input_Omistaja_NM == dmg_input_omistaja, max(DID)]
  new_dmg_table <- current_dmg[DID != max_DID]
  return(new_dmg_table)
}
