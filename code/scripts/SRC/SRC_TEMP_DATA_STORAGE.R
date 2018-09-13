#SRC_TEMP_DATA_STORAGE
readtds <- luecsv("temp_data_storage.csv")
#readtds[muuttuja == "Aloitus_DT", arvo := as.POSIXct(Aloitus_DT)] #tätä ei voi tehdä, kun muuttujan tyyppi on txt

SRC_TEMP_DATA_STORAGE <- readtds
