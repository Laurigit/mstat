#draft boosters
draft_boosters <- function(vect_boosters) {
  max_draft <- booster_data[, max(draft_no, na.rm = TRUE)]
  new_rows <- data.table(setti = vect_boosters,
                         muutos = -1,
                         Aikaleima = as.character(now(tz = "EET")),
                         draft_no = max_draft + 1)
  booster_data <- luecsv("boosters.csv")
  app_rows <- rbind(booster_data, new_rows)
  kircsv(app_rows, "boosters.csv", TRUE)
}
