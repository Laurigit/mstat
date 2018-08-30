#load_scripts.R
sourcelist <- dir("./scripts/")
sources_rest <-  sourcelist[!grepl("tab", sourcelist)]
sources_rest_no_ui <-  sources_rest[!grepl("UI", sources_rest)]
sources_rest_no_ui_no_loop <-  sources_rest_no_ui[!grepl("load_scripts", sources_rest_no_ui)]
for(filename in sources_rest_no_ui_no_loop) {
  edellinen <- filename 
  source(paste0("./scripts/", filename), local = TRUE)
}
