#source("./dev_tools/get_current_file_location.R")


initial.options <- commandArgs(trailingOnly = FALSE)
file.arg.name <- "--file="
script.name <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
script.basename <- dirname(script.name)
other.name <- file.path(script.basename, "other.R")
print(paste("Sourcing",other.name,"from",script.name))
source("./dev_tools/get_current_file_location.R")
