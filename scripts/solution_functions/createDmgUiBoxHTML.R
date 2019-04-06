createDmgUiBoxHTML <- function(accpetd_dmg_row) {

if (nrow(accpetd_dmg_row) > 1) {
  #accpetd_dmg_row <- accepted_rows[1]
  colori <- ifelse(accpetd_dmg_row[, Amount] > 0, "maroon", "green")
  ikoni <- ifelse(accpetd_dmg_row[, Combat_dmg] == 1, "fist", "bolt")
  targetti <- str_sub(accpetd_dmg_row[, Target_player], 1, 1)
  soursa <- str_sub(accpetd_dmg_row[, Dmg_source], 1, 1)
  maara <- abs(accpetd_dmg_row[, Amount])
  #suunta riippuen damagen vastaanottajasta
  if(targetti == "Lauri") {
    teksti <- paste0(targetti, "<-", soursa)
  } else {
    teksti <-  paste0( targetti, "->", soursa)
  }
  
 result <-  valueBox(maara, teksti, icon = icon(ikoni),
           color = colori,
           width = NULL)
} else {
  result <- ""
}
 return(result)
 
}
