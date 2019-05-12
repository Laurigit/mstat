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

