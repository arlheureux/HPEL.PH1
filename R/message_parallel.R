
#https://stackoverflow.com/questions/17345837/printing-from-mclapply-in-r-studio

## _________________ Notes
#
#' Function which prints a message using shell echo; useful for printing messages from inside mclapply when running in Rstudio
#
## ---------------------------


message_parallel <- function(...){
  system(sprintf('echo "\n%s\n"', paste0(..., collapse="")))
}
