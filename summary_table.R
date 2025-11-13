write_summary <- function(file) {
  stats <- read.table(file, header = FALSE)
  lang_name <- str_match(file, "([A-Za-z]+)_dependency_tree_metrics.txt")[1, 2]
  data.frame(
    Language = lang_name,
    N = length(stats$V1),
    mean_n = mean(stats$V1),
    sd_n = sd(stats$V1),
    mean_k2 = mean(stats$V2),
    sd_k2 = sd(stats$V2)
  )
}

source = list.files(
  path = "data",
  pattern = "\\.txt$",
  full.names = TRUE
)

# Initialize empty data frame
summary_df <- data.frame(Language=character(),
                         N=integer(),
                         mean_n=numeric(),
                         sd_n=numeric(),
                         mean_k2=numeric(),
                         sd_k2=numeric())

# Fill with for loop
for (x in source) {
  row <- write_summary(x)
  summary_df <- rbind(summary_df, row)
}

print(summary_df)
