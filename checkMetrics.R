# Check metrics validity

files <- list.files("validity_metrics", full.names = TRUE)

for (fl in files) {
  head(readLines(fl), 10)
  lang <- regmatches(fl, regexpr("(?<=/).*?(?=_)", fl, perl=TRUE))
  # print(lang)
  
  metrics <- read.table(fl, header = FALSE, sep="\t")
  lower_bound_k = all(4*metrics$n - 6 <= metrics$nk2)
  upper_bound_k = all(metrics$k2 <= metrics$n - 1)
  
  print(paste0(lang, " ", lower_bound_k, " ", upper_bound_k))
  
  if(!lower_bound_k) {
    error_index <- which((4*metrics$n - 6 > metrics$nk2))
    print(metrics[error_index,])
    print(metrics[error_index,]$nk2 - (4*metrics[error_index,]$n - 6))
  }
  
  if(!upper_bound_k) {
    error_index <- which(metrics$k2 > metrics$n - 1)
    print(metrics[error_index,])
    print(metrics[error_index,]$k2 - metrics[error_index,]$n - 1)
  }
}


