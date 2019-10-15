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

#fixapo[, Name := gsub('\'','\'\'', Name)]
#dbIns("CARDS", joinmid)
dbWriteTable(con, "CARDS", joinPID, append = TRUE, row.names = FALSE, fileEncoding = "UTF-8")


