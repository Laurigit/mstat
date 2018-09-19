#save_temp_data_storage_to_inputs
required_data("ADM_TEMP_DATA_STORAGE")
save_temp_data_storage_to_inputs <- function(ADM_TEMP_DATA_STORAGE) {

  lp <- ADM_TEMP_DATA_STORAGE[muuttuja == "Laurin_pakka", arvo]
  mp <- ADM_TEMP_DATA_STORAGE[muuttuja == "Martin_pakka", arvo]
  lm <- ADM_TEMP_DATA_STORAGE[muuttuja == "Laurin_mulligan", arvo]
  mm <- ADM_TEMP_DATA_STORAGE[muuttuja == "Martin_mulligan", arvo]
  input_local <- list(select_laurin_pakka  = lp,
                      select_martin_pakka = mp,
                      slider_laurin_mulligan = lm,
                      slider_martin_mulligan = mm)
  assign("input",
         input_local,
         pos = globalenv())
  
}
