#inputtina booster.csv
#outputtina objekti, missä tekstinä boosterit, mitä avataan ja objektina juttu, jonka voi tunkea toiseen funktioon
#mikä päivttää csv:n

#kolmas funcio lisää boostereita

suggest_boosters <- function(how_many_boosters) {
  required_data("STG_BOOSTERS")
  booster_data <- luecsv("boosters.csv")

  booster_data <- STG_BOOSTERS
  booster_data[, rivi := seq_len(.N)]
  #find previous increase
  last_increase_row <- booster_data[draft_no <0,max(rivi)]
  
  #find next draft after increase
  next_draft <- booster_data[rivi > last_increase_row, (min(draft_no, na.rm = TRUE))]
  
  #calculate kulmakerroin for booster use. Use original amount from last increase
  current_stock <- booster_data[draft_no < next_draft, .(sum_boosters = sum(muutos)), by = setti]
 
  current_stock[, kulmakerroin := sum_boosters/ sum(sum_boosters)]
  
  #count how many boosters have drafted since last increase
  count_dafted_boosters <- booster_data[rivi > last_increase_row, .(drafted_count = - sum(muutos, na.rm = TRUE)), by = setti]
  count_dafted_boosters_tot <- count_dafted_boosters[, sum(drafted_count)]
  #join count
  joined_count <-count_dafted_boosters[current_stock, on = "setti"]
  joined_count[,drafted_count := ifelse(is.na(drafted_count), 0, drafted_count)]
  kpi_status <- joined_count[, .(kulmakerroin, setti, kpi_status = count_dafted_boosters_tot * kulmakerroin - drafted_count, kulmakerroin)]
  loop_kpi <- current_stock[, .(setti, kulmakerroin)]
  set_vector <- NULL
  for(booster_loop in 1:how_many_boosters) {
    kpi_status[, kpi_status := kpi_status + kulmakerroin]
    boosted_set <- kpi_status[which.max(kpi_status), setti]

    kpi_status[setti == boosted_set, kpi_status := kpi_status - 1]
    set_vector <- c(set_vector, boosted_set)
  }
  
  return(set_vector)
}
