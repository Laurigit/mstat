luecsv <- function(tiedostonimi) {
  tulos <- as.data.table(read.csv(paste0("./external_files/", tiedostonimi),
                                  sep = ";",
                                  stringsAsFactors = FALSE,
                                  dec = ",",
                                  fileEncoding = "UTF-8-BOM"))
  return(tulos)
}