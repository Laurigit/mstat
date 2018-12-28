#load_scripts.R

sourceLoop <- function(sourcelist, input_kansio) {
  dir_list <- sourcelist[kansio == input_kansio, polku]
  for(filename in dir_list) {
    result = tryCatch({
      source(paste0("./scripts/", filename), local = FALSE)
    }, error = function(e) {
      print(paste0("error in loading file: ", filename))
    })
  }
sourcelist[kansio == input_kansio, ajettu := 1]
return(sourcelist)
}
  
sourcelist <- data.table(polku = c(dir("./scripts/", recursive = TRUE)))
sourcelist[, rivi := seq_len(.N)]
suppressWarnings(sourcelist[, kansio := strsplit(polku, split = "/")[1], by = rivi])
sourcelist <- sourcelist[!grep("load_scripts.R", polku)]
sourcelist[, kansio := ifelse(str_sub(kansio, -2, -1) == ".R", "root", kansio)]
sourcelist <- sourceLoop(sourcelist, "utility")

sourcelist <- sourceLoop(sourcelist, "solution_functions")
sourcelist <- sourceLoop(sourcelist, "UID")
sourcelist <- sourceLoop(sourcelist, "tabstatic")
sourcelist <- sourceLoop(sourcelist, "tab")
sourcelist <- sourceLoop(sourcelist, "root")


