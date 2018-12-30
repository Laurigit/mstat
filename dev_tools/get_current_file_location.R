op <- function(Printattava_teksti){
full.fpath <- tryCatch(normalizePath(parent.frame(2)$ofile),  # works when using source
                       error=function(e) # works when using R CMD
                         normalizePath(unlist(strsplit(commandArgs()[grep('^--file=', commandArgs())], '='))[2]))
#print(paste0(Printattava_teksti, ": ", full.fpath))
return(full.fpath)
}

op()
