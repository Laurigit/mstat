#fix missing CARDS_DIM data
# con <- connDB(con)
# #cards <- dbSelectAll("CARDS_DIM", con)
# cards <- MANASTACK_CARDS[, .(Name)]
# 
# loop_total <- NULL
# loop_card <- "Fire // Ice"
# for(loop_card in cards[, Name]) {
#   print(loop_card)
#   new_row <- getCard_from_SF(loop_card)
#   loop_total <- rbind(new_row, loop_total)
# }
# loop_total
# dt_loop <- as.data.table(loop_total)
# dt_loop_copy <- dt_loop[1!=0]
# dt_loop_copy[, Colors := paste0(unlist(Colors), collapse =""), by = Name]
# dt_loop_copy[, Colors := NULL]
# dt_loop_copy[, Type := iconv(x = Type, to = "UTF-8")]
#save(loop_total, file = "loop_total.RData")
#dbWriteTable(con, "CARDS_DIM", loop_total, row.names = FALSE, append = TRUE)

