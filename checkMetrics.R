# Check metrics validity

files <- list.files("validity_metrics", full.names = TRUE)

for (fl in files) {
  lang <- regmatches(fl, regexpr("(?<=/).*?(?=_)", fl, perl=TRUE))
  
  metrics <- read.table(fl, header = FALSE, sep="\t")
  
  lower_bound_k = all(4*metrics$n - 6 <= metrics$nk2)
  upper_bound_k = all(metrics$k2 <= metrics$n - 1)
  
  lower_bound_d = all(metrics$nk2 + metrics$q <= 4*metrics$sum_d)
  upper_bound_d = all(4*metrics$sum_d <= 3*(metrics$n - 1)^2 + 1 - metrics$n%%2)
  
  print(paste0(lang, ": <k2> ", (lower_bound_k & upper_bound_k), ", <d> ", (lower_bound_d & upper_bound_d)))
}


