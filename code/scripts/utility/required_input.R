required_input <- function(input_nm, list_nm_vector, default_value_vector) {
 # https://stackoverflow.com/questions/32231835/assign-element-to-list-in-parent-frame
  
  if(!exists(list_nm,
             where = input_nm,
             envir = parent.frame())) {
    assign(input_nm,
           "def_value_not_used",
           envir = parent.frame())
    counter <- 0
    for (list_nm in list_nm_vector){
      counter <- counter + 1
      default_value <- default_value_vector[[counter]]
    suppressWarnings(
    eval(parse(text = sprintf("%s$%s <- %d", input_nm, list_nm, default_value)), envir = parent.frame())
    )
    }
  }
  
}
