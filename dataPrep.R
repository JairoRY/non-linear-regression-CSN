# Data Preparation

library(igraph)
library(udpipe)
library(stringr)

unlink("data", recursive=TRUE)
dir.create("data")

unlink("validity_metrics", recursive=TRUE)
dir.create("validity_metrics")

files <- list.files("ud-treebanks-v2.16", full.names = TRUE, pattern = "\\.conllu$", recursive = TRUE)

for (fl in files) {
  lang <- str_match(fl, "UD_([A-Za-z]+)-PUD")[1, 2]
  print(lang)
  
  data <- udpipe_read_conllu(fl)
  data <- data[ , c(5,6,11)]
  data <- data[grepl("^\\d+$", data$token_id), ]
  data$head_token_id <- as.integer(data$head_token_id)
  data$token_id <- as.integer(data$token_id)
  
  idx <- c(1, diff(data$token_id))
  idx <- c(1,which(idx < 0),nrow(data) + 1)
  data$sentence_id <- findInterval(seq_along(data$token_id), idx)
  sentences <- split(data, data$sentence_id)
  
  metrics <- lapply(sentences, function(s) {
    
    # edgs <- data.frame(v1 = s$head_token_id, v2 = s$token_id)
    edgs <- data.frame(v1 = s$token_id[s$head_token_id != 0], v2 = s$head_token_id[s$head_token_id != 0]) # removing the root?
    g <- graph_from_data_frame(edgs, directed = FALSE)
    g <- simplify(g) # Remove self-loops and duplicate edges
    
    n <- vcount(g)
    
    degrees <- degree(g)
    k2 <- mean(degrees^2)
    nk2 <- sum(degrees^2)
    
    dists <- abs(edgs$v1 - edgs$v2)
    d_mean <- mean(dists)
    sum_d <- sum(dists)
    
    q <- sum(degrees%%2)
    
    return(c(n = n, k2 = k2, nk2 = nk2, d_mean = d_mean, sum_d = sum_d, q = q))
  })
  
  metrics_df <- do.call(rbind, metrics)
  
  output_file <- file.path("data", paste0(lang, "_dependency_tree_metrics.txt"))
  write.table(metrics_df[, c("n", "k2", "d_mean")], file = output_file, row.names = FALSE, col.names = FALSE, sep = "\t")
  
  output_file_for_validity_check <- file.path("validity_metrics", paste0(lang, "_metrics_for_validity_check.txt"))
  write.table(metrics_df, file = output_file_for_validity_check, row.names = FALSE, sep = "\t")
  
}
