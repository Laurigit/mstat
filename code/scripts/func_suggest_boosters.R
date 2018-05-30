#inputtina booster.csv
#outputtina objekti, missä tekstinä boosterit, mitä avataan ja objektina juttu, jonka voi tunkea toiseen funktioon
#mikä päivttää csv:n

suggest_boosters <- function() {
  booster_data <- luecsv("boosters.csv")
  
  
  
  draftit <- booster_data[, .N, by = draft_no][draft_no> 0, draft_no]
  total_muutos <- NULL
  for (draft_loop in draftit) {
    #stock before draft
    current_stock <- booster_data[draft_no < draft_loop, .(sum_boosters = sum(muutos)), by = setti]
    current_stock[, pct_left := sum_boosters/ sum(sum_boosters)]
    current_stock[, drafti := draft_loop]
    drafted_boosters <- booster_data[draft_no ==draft_loop, .(muutos = muutos, setti)]
    #add change
    join_change <- drafted_boosters[current_stock,on = "setti"]
    join_change[, muutos := ifelse(is.na(muutos), 0, muutos)]
    
    total_muutos <- rbind(total_muutos,join_change)
  }
  
  current_stock <- booster_data[, .(sum_boosters = sum(muutos)), by = setti]
  print(current_stock)
  current_stock[, booster_kpi := sum_boosters/ sum(sum_boosters, na.rm =TRUE)][,sum_boosters := NULL]
  current_situation <- total_muutos[, .(booster_kpi = sum(muutos) + sum(pct_left)), by = setti]
  print(current_situation)
  append_total <- rbind(current_situation, current_stock)
  aggr_append <- append_total[, .(kpi_candidates = sum(booster_kpi)), by = setti][order(-kpi_candidates)][1:4][,setti]
  return(aggr_append)
}
