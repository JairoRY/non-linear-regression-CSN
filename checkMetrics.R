# Check metrics validity

files <- list.files("validity_metrics", full.names = TRUE)

for (fl in files) {
  head(readLines(fl), 10)
  lang <- regmatches(fl, regexpr("(?<=/).*?(?=_)", fl, perl=TRUE))
  # print(lang)
  
  metrics <- read.table(fl, header = FALSE, sep="\t")
  lower_bound_k = all(4*metrics$n - 6 <= metrics$nk2)
  upper_bound_k = all(metrics$k2 <= metrics$n - 1)
  
  print(paste0(lang, ": ", (lower_bound_k & upper_bound_k)))
}


