create_data_for_win_disribution <- function(peliData_ja_pfi) {

maxTurnaus <- peliData_ja_pfi[, max(TurnausNo)]
all_data <- NULL
for (loop_no in (maxTurnaus - 10):(maxTurnaus-1)) {
  kierrosData <- create_forecast_data_for_stats(peliData_ja_pfi,loop_no)
  kierrosData[, TurnausNo := loop_no]
  all_data <- rbind(all_data, kierrosData)
}
return(all_data)
}