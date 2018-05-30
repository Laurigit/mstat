#draft boosters
vect_boosters <- suggest_boosters()
vect_boosters
draft_boosters(vect_boosters)

draft_boosters <- function(vect_boosters) {
  booster_data <- luecsv("boosters.csv")
  max_draft <- booster_data[, max(draft_no, na.rm =TRUE)]
  new_rows <- data.table(setti = vect_boosters,
                         muutos = -1,
                         Aikaleima = as.character(today(tzone = "EET")),
                         draft_no = max_draft+1)
  app_rows <- rbind(booster_data, new_rows)
  kircsv(app_rows, "boosters.csv", FALSE)
}
