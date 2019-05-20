#make blow timer
create_timedata_for_blowtimer <- function(odotusaika){
  aikakorjaus <- now(tz = "Europe/Helsinki") + odotusaika * 60 
aika <- as.ITime(aikakorjaus) 
pvm <- as.IDate(aikakorjaus)
blow_dt <- data.table(Puhallusaika = aika, Puhalluspvm = pvm)
kircsv(blow_dt, "blow_timer.csv")
}
