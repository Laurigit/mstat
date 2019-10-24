required_data("STG_PAKKA_COMPONENTS")


connDB <- function(con) {
  con <- tryCatch({
    
    res <- dbFetch(dbSendQuery(con, "SHOW TABLES"))
    
    con
  }, error = function(ef) {
    
    con <<- dbConnect(MySQL(),
                      user = 'root',
                      password = 'betmtg_pw',
                      host = '35.228.73.82',
                      port = 3306,
                      dbname = 'betmtg2')
  })
  return(con)
}
con  <-connDB(con)



fixapo <- STG_PAKKA_COMPONENTS[, .(Pakka_form_ID, Card_ID, Count, Name, Maindeck = ifelse(Maindeck == TRUE, 1, 0))]
mid <- MANASTACK_CARDS[, .(MID, Card_ID)]
joinmid <- fixapo[mid, on = "Card_ID"]

sscols_pfi <- STG_PFI[, .(Pakka_ID, Pakka_form_ID)]

joinPID <- joinmid[sscols_pfi, on = "Pakka_form_ID"]

#levita Counttien maaran mkaan

levita_muut2 <- joinPID[rep(seq_len(nrow(joinPID)), Count), ][, Count := NULL]

#luo draft_id. Laske eka montako samaa korttia on pakassa ja tee sen jalkee ID yhdistamalla jarjestys ja MID
#levita_muut2[, jarj := seq_len(.N), by = .(Pakka_form_ID, MID)]
#levita_muut2[, DRAFT_CARDS_ID := -1 * as.numeric(paste0(jarj * 10, MID))]
# levita_muut2[Name %in% c("Plains",
#                          "Mountain",
#                          "Swamp",
#                          "Island",
#                          "Forest",
#                          "Wastes"), DRAFT_CARDS_ID := ifelse(Name == "Plains",
#                                                              -1,
#                                                              ifelse(Name == "Mountain", -2,
#                                                                     ifelse(Name == "Swamp", -3,
#                                                                            ifelse(Name == "Island", -4,
#                                                                                   ifelse(Name == "Forest", -5,
#                                                                                          ifelse(Name == "Wastes", -6,
#                                                                                                 DRAFT_CARDS_ID))))))]
#levita_muut2[, jarj := NULL]

landitaulu <- data.table(Name = c("Forest","Swamp","Plains",                                  "Mountain",
                                  "Island",
                                  "Wastes")
                         ,  land_MID = c(289327,
                                    473220,
                                    473212,
                                    221305,
                                    386333,
                                    407693))


levita_muut2[, DRAFT_CARDS_ID := seq_len(.N) * -1]
joinlandi <- landitaulu[levita_muut2, on = "Name"]
joinlandi[, MID := ifelse(is.na(land_MID), MID, land_MID)]
joinlandi[, land_MID := NULL]
#fixapo[, Name := gsub('\'','\'\'', Name)]
#dbIns("CARDS", joinmid)
dbWriteTable(con, "CARDS", joinlandi, append = TRUE, row.names = FALSE, fileEncoding = "UTF-8")

