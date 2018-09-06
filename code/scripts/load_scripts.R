#load_scripts.R
sourcelist <- c(dir("./scripts/", recursive = TRUE))
sources_rest_no_ui_no_loop <-  sourcelist[!grepl("load_scripts", sourcelist)]
sources_rest_no_ui_no_loop_only_R <- sources_rest_no_ui_no_loop[str_sub(sources_rest_no_ui_no_loop, -2 , -1) == ".R"]
sources_rest_no_ui_no_loop_only_R_no_ADM <- sources_rest_no_ui_no_loop_only_R[!grepl("ADM", sources_rest_no_ui_no_loop_only_R)]
sources_rest_no_ui_no_loop_only_R_no_ADM_STG <- sources_rest_no_ui_no_loop_only_R_no_ADM[!grepl("STG", sources_rest_no_ui_no_loop_only_R_no_ADM)]
sources_rest_no_ui_no_loop_only_R_no_ADM_STG_SRC <- sources_rest_no_ui_no_loop_only_R_no_ADM_STG[!grepl("SRC", sources_rest_no_ui_no_loop_only_R_no_ADM_STG)]
sources_rest_no_ui_no_loop_only_R_no_ADM_STG_SRC_INT <- sources_rest_no_ui_no_loop_only_R_no_ADM_STG_SRC[!grepl("INT", sources_rest_no_ui_no_loop_only_R_no_ADM_STG_SRC)]
sources_rest_no_ui_no_loop_only_R_no_ADM_STG_SRC_INT_STAT <- sources_rest_no_ui_no_loop_only_R_no_ADM_STG_SRC_INT[!grepl("STAT", sources_rest_no_ui_no_loop_only_R_no_ADM_STG_SRC_INT)]
notab <- sources_rest_no_ui_no_loop_only_R_no_ADM_STG_SRC_INT_STAT[!grepl("tab", sources_rest_no_ui_no_loop_only_R_no_ADM_STG_SRC_INT_STAT)]
tab <-  sources_rest_no_ui_no_loop_only_R_no_ADM_STG_SRC_INT_STAT[grepl("tab", sources_rest_no_ui_no_loop_only_R_no_ADM_STG_SRC_INT_STAT)]
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
