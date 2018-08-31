#load_scripts.R
sourcelist <- dir("./scripts/")
sources_rest_no_ui_no_loop <-  sourcelist[!grepl("load_scripts", sourcelist)]
sources_rest_no_ui_no_loop_only_R <- sources_rest_no_ui_no_loop[str_sub(sources_rest_no_ui_no_loop, -2 , -1) == ".R"]
notab <- sources_rest_no_ui_no_loop_only_R[!grepl("tab", sources_rest_no_ui_no_loop_only_R)]
tab <-  sources_rest_no_ui_no_loop_only_R[grepl("tab", sources_rest_no_ui_no_loop_only_R)]
for(filename in notab) {
  result = tryCatch({
    suppressWarnings(source(paste0("./scripts/", filename), local = TRUE))
  }, error = function(e) {
    print(paste0("error in loading file: ", filename))
  })
}

for(filename in tab) {
  result = tryCatch({
    suppressWarnings(source(paste0("./scripts/", filename), local = TRUE))
  }, error = function(e) {
    print(paste0("error in loading file: ", filename))
  })
}