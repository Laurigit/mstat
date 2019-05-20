#I have bought more boosters and I want you to know about it
add_boosters_to_inventory <- function(setti, lkm) {
  booster_data <- luecsv("boosters.csv")
  

  new_rows <- data.table(setti = setti,
                         muutos = lkm,
                         Aikaleima = as.character(today(tzone = "EET")),
                         draft_no = -1)
  app_rows <- rbind(booster_data, new_rows)
  kircsv(app_rows, "boosters.csv")
  
}
