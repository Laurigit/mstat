connDB <- function(con) {

 res <- tryCatch({
   dbGetQuery(con, "SELECT 1")
   }, error = function(e) {
     "error"
   })  
 
 if(res == "error") {
  bm <- config::get("bm")
  
 con <- dbConnect(MySQL(),
                   user = bm$uid,
                   password = bm$pwd,,
                   dbname = bm$database,
                   host= bm$server)
 }
  
return(con)
  
}

# con <- dbConnect(MySQL(),
#                  user = "Lauri_db",
#                  password = "bruc3tti_Db",
#                  dbname = "betmtgdb",
#                  host= "betmtgdb.cq1pjet0dxmr.eu-north-1.rds.amazonaws.com")
# }
