#dbWT
dbWT <- function(con, table, db_table_name = NULL) {
  result <- tryCatch({
   
    if (!is.null(db_table_name)) {
      db_table_name_in_syntax <- db_table_name
    } else {
      db_table_name_in_syntax <- deparse(substitute(table))
      }
    dbWriteTable(con, db_table_name_in_syntax, table, row.names = FALSE, overwrite = TRUE) 
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
