#ota yhteys uteen ja vanhaan

#lue vanhan taulut

#

con_from <- dbConnect(MySQL(),
                  user = 'root',
                  password = 'betmtg_pw',
                  host = '34.88.252.73',
                  port = 3306,
                  dbname = 'betmtg2')



#for safety, use test as default

dbFetch(dbSendQuery(con_from, "USE betmtg2;"))
res <- data.table(dbFetch(dbSendQuery(con_from, "SHOW TABLES")))[, Tables_in_betmtg2]
dbFetch(dbSendQuery(con_from, "USE betmtg_test2023;"))
for (loop_table in res) {
  drop_query <- dbFetch(dbSendQuery(con_from, paste0("DROP TABLE IF EXISTS betmtg_test2023.", loop_table, ";")))
inside_query <- paste0("CREATE TABLE betmtg_test2023.", loop_table, " LIKE betmtg2.", loop_table, ";")
create_table <- data.table(dbFetch(dbSendQuery(con_from, inside_query)))

insert_query <- paste0("INSERT betmtg_test2023.", loop_table, " SELECT * FROM betmtg2.", loop_table, ";")
insert_rows <-  data.table(dbFetch(dbSendQuery(con_from, insert_query)))
rows <-  dbFetch(dbSendQuery(con_from, paste0("SELECT count(1) FROM betmtg_test2023.", loop_table, ";")))
print(rows)
}
