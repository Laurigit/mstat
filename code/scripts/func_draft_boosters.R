#draft boosters
draft_boosters <- function(vect_boosters) {
  required_data("STG_BOOSTERS")
  booster_data <-STG_BOOSTERS
  booster_data[, rivi  := NULL]
  max_draft <- booster_data[, max(draft_no, na.rm = TRUE)]
  new_rows <- data.table(setti = vect_boosters,
                         muutos = -1,
                         Aikaleima = as.character(now(tz = "EET")),
                         draft_no = max_draft + 1)

  app_rows <- rbind(booster_data, new_rows)
  kircsv(app_rows, "boosters.csv", TRUE)
  required_data("ADM_DI_HIERARKIA")
  updateData("SRC_BOOSTERS", ADM_DI_HIERARKIA, input_env = globalenv(), FALSE)
}
