#required_data
#data_vector <- "SRC_PELIT"
required_data <- function(data_vector, force_update = FALSE) {
  #load("shiny_env.R")

  used_env <- parent.frame()
  for(data_file in data_vector) {
  if(!exists(data_file, envir = used_env ) | force_update == TRUE) {
  list_of_object <- c(ls(envir = used_env), data_file)

    find_and_source(data_file, used_env)
 
  list_after_sourcing <- ls(envir = used_env)
  newly_created <- setdiff(list_after_sourcing,
                           list_of_object)
  funlist <- lsf.str(envir = used_env)
  delete_list <- setdiff(newly_created, funlist)
  rm(list = delete_list, envir = used_env)
  }
  }
}
