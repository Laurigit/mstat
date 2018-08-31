#required_data
#data_vector <- "SRC_PELIT"
required_data <- function(data_vector) {
  load("shiny_env.R")
  list_of_object <- c(ls(envir = shiny_env), data_vector)

    find_and_source(data_vector)
 
  list_after_sourcing <- ls(envir = shiny_env)
  newly_created <- setdiff(list_after_sourcing,
                           list_of_object)
  funlist <- lsf.str(envir = shiny_env)
  delete_list <- setdiff(newly_created, funlist)
  rm(list = delete_list, envir = shiny_env)
}
