# Generate syntactic dependency trees

library(igraph)

unlink("data", recursive=TRUE)
dir.create("data")

unlink("validity_metrics", recursive=TRUE)
dir.create("validity_metrics")

files <- list.files("samples", full.names = TRUE, pattern = "\\.conllu$")

for (fl in files) {
  lang <- regmatches(fl, regexpr("(?<=/).*?(?=_)", fl, perl=TRUE))
  print(lang)
  
  data <- read.table(fl, header = FALSE, quote="", sep="\t", comment.char="")
  data <- data[ , c(1,2,7)]
  data <- data[grepl("^\\d+$", data$V1), ]
  data$V7 <- as.integer(data$V7)
  data$V1 <- as.integer(data$V1)
  
  idx <- c(1, diff(data$V1))
  idx <- c(1,which(idx < 0),nrow(data) + 1)
  data$sentence_id <- findInterval(seq_along(data$V1), idx)
  sentences <- split(data, data$sentence_id)
  
  metrics <- lapply(sentences, function(s) {
    
    # edgs <- data.frame(v1 = s$V7, v2 = s$V1)
    edgs <- data.frame(v1 = s$V1[s$V7 != 0], v2 = s$V7[s$V7 != 0]) # removing the root?
    g <- graph_from_data_frame(edgs, directed = FALSE)
    g <- simplify(g) # Remove self-loops and duplicate edges
    
    n <- vcount(g)
    degrees <- degree(g)
    k2 <- mean(degrees^2)
    nk2 <- sum(degrees^2)
    
    valid_k <- (4 - 6/n <= k2 & k2 <= n - 1)
    if (!valid_k) {
      print("WARNING: Invalid k2 found !!")
      plot(g)
    }
    
    return(c(n = n, k2 = k2, nk2 = nk2))
  })
  
  metrics_df <- do.call(rbind, metrics)
  # output_file <- file.path("data", paste0(lang, "_dependency_tree_metrics.txt"))
  # write.table(metrics_df, file = output_file, row.names = FALSE, col.names = FALSE, sep = "\t")
  
  output_file_k2 <- file.path("data", paste0(lang, "_dependency_tree_metrics.txt"))
  write.table(metrics_df[, c("n", "k2")], file = output_file_k2, row.names = FALSE, col.names = FALSE, sep = "\t")
  
  output_file_nk2 <- file.path("validity_metrics", paste0(lang, "_metrics_for_validity_check.txt"))
  write.table(metrics_df, file = output_file_nk2, row.names = FALSE, sep = "\t")
  
}

