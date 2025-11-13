source = list.files(
  path = "data",
  pattern = "\\.txt$",
  full.names = TRUE
)

for (x in source) {
  stats <- read.table(x, header = FALSE)
  check_klower = all(4-6/stats$V1 <= stats$V2)
  check_lupper = all(stats$V2 <= stats$V1-1)
  print(paste0(check_klower, " ", check_lupper))
}

stats <- read.table("data/Turkish_dependency_tree_metrics.txt", header = FALSE)
check_klower_v = (4-6/stats$V1 <= stats$V2)
which(check_klower_v==FALSE)

for (n, k, d in stats) {
  check_klower = (4-6/n <= k)
  print(check_klower)
}
