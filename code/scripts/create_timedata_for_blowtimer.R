#make blow timer
create_timedata_for_blowtimer <- function(){
aika <- as.ITime(now(tz = "Europe/Helsinki"))
pvm <- as.IDate(now(tz = "Europe/Helsinki"))
blow_dt <- data.table(Puhallusaika = aika, Puhalluspvm = pvm)
kircsv(blow_dt, "blow_timer.csv", upload = TRUE)
}