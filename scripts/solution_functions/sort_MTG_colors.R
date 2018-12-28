#sort_MTG_colors
sort_MTG_colors <- function(color_string) {
numbers <- data.table(numbers = seq(1:10), colornames =  c("W" , "U", "B", "R", "G", "W" , "U", "B", "R", "G"))
if(length(color_string) == 1) {
  input_temp <- strsplit(color_string, "")
} else {
  input_temp <- color_string
}
#only upper 
to_dt <- data.table(char = input_temp[[1]])
to_dt[, upper_comp := toupper(char)]
input <- to_dt[upper_comp == char, char]
input_nocaps <-  to_dt[upper_comp != char, char]

inner_func <- function(input) {
count_colors <- length(input)
input <- toupper(input)
data <- numbers[colornames %in% input]
data[, diff := shift(numbers, type = "lead") - numbers]
dataf <- data[!is.na(diff)]
dataf[, res := rollsum(diff, k = 2, fill = NA, align = "left")]
dataf[, row := seq_len(.N)]
first_row <- dataf[which.min(res), row]
last_row <- first_row + count_colors - 1
result <- paste0(dataf[first_row:last_row, colornames], collapse = "")
return(result)
}
if(length(input) > 1) {
caps_res <- inner_func(input)
} else {
  caps_res <- input
}
if(length(input_nocaps) > 1) {
  low_res <- tolower(inner_func(input_nocaps))
} else {
  low_res <- input_nocaps
}
total_result <- paste0(caps_res, low_res)

}
