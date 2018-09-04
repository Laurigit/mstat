#required_data
#data_vector <- "SRC_PELIT"
required_data <- function(data_vector) {
  load("shiny_env.R")
  used_env <- parent.frame()
  list_of_object <- c(ls(envir = used_env), data_vector)

    find_and_source(data_vector, used_env)
 
  list_after_sourcing <- ls(envir = used_env)
  newly_created <- setdiff(list_after_sourcing,
                           list_of_object)
  funlist <- lsf.str(envir = used_env)
  delete_list <- setdiff(newly_created, funlist)
  rm(list = delete_list, envir = used_env)
}
