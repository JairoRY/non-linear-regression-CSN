# Generate the summary table

library("xtable")

lang_dict <- c(
  ar = "Arabic",
  cs = "Czech",
  de = "German",
  en = "English",
  es = "Spanish",
  fi = "Finnish",
  fr = "French",
  gl = "Galician",
  hi = "Hindi",
  id = "Indonesian",
  is = "Icelandic",
  it = "Italian",
  ja = "Japanese",
  ko = "Korean",
  pl = "Polish",
  pt = "Portuguese",
  ru = "Russian",
  sv = "Swedish",
  th = "Thai",
  tr = "Turkish",
  zh = "Chinese"
)

files <- list.files("data", full.names = TRUE, pattern = "\\.txt$")

write_summary <- function(file) {
  metrics <- read.table(file, header = FALSE)
  colnames(metrics) <- c("n", "k2")
  lang <- regmatches(file, regexpr("(?<=/).*?(?=_)", file, perl = TRUE))
  lang <- unname(lang_dict[lang])
  
  data.frame(
    Language = lang,
    N = length(metrics$n),
    mean_n = mean(metrics$n),
    sd_n = sd(metrics$n),
    mean_k2 = mean(metrics$k2),
    sd_k2 = sd(metrics$k2)
  )
}

summary_list <- lapply(files, write_summary)
summary_df <- do.call(rbind, summary_list)

print(summary_df)

digits <- c(0, 0, 2, 2, 2, 2, 2)  # Per canviar els decimals en la taula
latex_table <- xtable(summary_df, type = "latex", row.names = FALSE, digits = digits)
print(latex_table, file = "summary_table.tex", include.rownames = FALSE)
