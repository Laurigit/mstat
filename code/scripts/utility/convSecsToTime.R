#convert itime seconds to time

convSecsToTime <- function(seconds, days, tzfix = 0) {
  total <- seconds + days * 60 * 60 * 24
  result <- anytime(total,tz = "UTC") - tzfix
  attr(result, "tzone") <- "EET"
  return(result)
}
convSecsToTime(73167, as.integer(as.IDate("2018-08-18")))
#M_9_2018-08-18_73167

convSecsToTime(74542,17761)
	
