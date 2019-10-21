#load divari to db one time


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

required_data("SRC_DIVARI")
add_omistaja_ID <- SRC_DIVARI[1 != 0]

add_omistaja_ID[, Omistaja_ID := ifelse(Omistaja == 1, "L", "M")]

dbWriteTable(con, "DECKS_DIM", add_omistaja_ID, row.names = FALSE, append = TRUE)
