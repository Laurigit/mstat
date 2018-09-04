#SRC_BOOSTERS.
required_functions("luecsv")
temp <- luecsv("boosters.csv")
SRC_BOOSTERS <- temp[!is.na(muutos), .(setti,
                                       muutos,
                                       Aikaleima = as.POSIXct(Aikaleima, tz = "EET"),
                                       draft_no)]

