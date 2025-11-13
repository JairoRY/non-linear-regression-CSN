library(udpipe)
library(igraph)
library(dplyr)
library(stringr)

enable_deletion <- TRUE
ud_treebanks_path <- "ud-treebanks-v2.16"

if (enable_deletion) {
  all_dirs <- list.dirs(path = ud_treebanks_path, recursive = FALSE, full.names = TRUE)
  
  # Keep only PUD treebanks
  pud_dirs <- all_dirs[grepl("-PUD", basename(all_dirs))]
  dirs_to_delete <- setdiff(all_dirs, pud_dirs)
  sapply(dirs_to_delete, unlink, recursive = TRUE, force = TRUE)
}

# Create output directory
if (!dir.exists("data")) {
  dir.create("data")
  print("Created 'data' directory.")
}

pud_files <- list.files(
  path = ud_treebanks_path,
  pattern = "\\.conllu$",
  recursive = TRUE,
  full.names = TRUE
)
pud_files <- pud_files[grepl("PUD", pud_files)]

# Function to process a single PUD file
process_pud_file <- function(file_path) {
  lang_name <- str_match(file_path, "UD_([A-Za-z]+)-PUD")[1, 2]
  ud_data <- udpipe_read_conllu(file_path)
  
  # Remove punctuation
  ud_data <- ud_data %>% filter(upos != "PUNCT")
  
  metrics <- data.frame(n = numeric(), k2_mean = numeric(), d_mean = numeric())
  
  for (sent_id in unique(ud_data$sentence_id)) {
    sent <- ud_data %>% filter(sentence_id == sent_id)
    
    if (nrow(sent) < 2) next  # skip too short sentences
    
    # Build edge list (head-dependent pairs)
    edges <- sent %>%
      filter(head_token_id != 0) %>%
      select(from = token_id, to = head_token_id)
    
    if (nrow(edges) == 0) next
    
    g <- graph_from_data_frame(edges, directed = TRUE, vertices = sent$token_id)
    
    # n = number of tokens
    n <- gorder(g)
    
    # degrees (undirected to capture connectivity)
    deg <- degree(g, mode = "all")
    k2_mean <- mean(deg^2)
    
    # average dependency distance ⟨d⟩
    dists <- abs(as.numeric(edges$from) - as.numeric(edges$to))
    d_mean <- mean(dists)
    
    metrics <- rbind(metrics, data.frame(n = n, k2_mean = k2_mean, d_mean = d_mean))
  }
  
  if (nrow(metrics) > 0) {
    output_filename <- file.path("data", paste0(lang_name, "_dependency_tree_metrics.txt"))
    write.table(metrics, file = output_filename, row.names = FALSE, col.names = FALSE)
    return(paste(lang_name, output_filename))
  } else {
    return(NULL)
  }
}

created_filenames <- c("language file")

# Process all PUD files
for (file_path in pud_files) {
  result <- process_pud_file(file_path)
  if (!is.null(result)) {
    created_filenames <- c(created_filenames, result)
  }
}

if (length(created_filenames) > 1) {
  writeLines(created_filenames, "list_in.txt")
}

print("Data preparation complete.")
