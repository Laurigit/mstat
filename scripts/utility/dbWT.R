#dbWT
dbWT <- function(con, table) {
  result <- tryCatch({
    dbWriteTable(con, deparse(substitute(table)), table, row.names = FALSE, overwrite = TRUE) 
    result <- "SUCCESS"
 
  }, error = function(e) {
    warning(e)
    result <- "ERROR"
    warning("COULD NOT SAVE RETRYING")
    sys.sleep(2)
    dbWt(con, table)
    result
  })
  return(result)
  
}
