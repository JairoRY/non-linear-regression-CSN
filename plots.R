# Generate plots for visualizing

library(ggplot2)
library(gridExtra)  # For arranging multiple plots

unlink("plots", recursive=TRUE)
dir.create("plots")

files <- list.files("data", full.names = TRUE, pattern = "\\.txt$")

point_size <- 1

for (file in files) {
  lang <- regmatches(file, regexpr("(?<=/).*?(?=_)", file, perl=TRUE))
  
  metrics <- read.table(file, header = FALSE, sep="\t")
  colnames(metrics) = c("vertices","degree_2nd_moment", "mean_length")
  metrics = metrics[order(metrics$vertices), ]
  
  # Simple plot
  
  plot_simple <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    geom_point(size = point_size) +
    labs(x = "Vertices", y = "Degree 2nd Moment", title = "x -- y")
  
  plot_simple_log_x <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    geom_point(size = point_size) +
    labs(x = "Vertices", y = "Degree 2nd Moment", title = "log(x) -- y") +
    scale_x_log10()
  
  plot_simple_log_y <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    geom_point(size = point_size) +
    labs(x = "Vertices", y = "Degree 2nd Moment", title = "x -- og(y)") +
    scale_y_log10()
  
  plot_simple_log_xy <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    geom_point(size = point_size) +
    labs(x = "Vertices", y = "Degree 2nd Moment", title = "log(x) -- log(y)") +
    scale_x_log10() +
    scale_y_log10()
  
  combined_plot_simple <- grid.arrange(plot_simple, plot_simple_log_x, plot_simple_log_y, plot_simple_log_xy, 
                                nrow = 2, ncol = 2)
  ggsave(filename = paste0("plots/", lang, "_combined_plots_simple.png"), plot = combined_plot_simple)
  
  # Averaged plot
  
  plot_averaged <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", alpha = 0.2) +  # Deviation shaded area
    labs(x = "Vertices", y = "Mean Degree 2nd Moment", title = "x -- y")
  
  plot_averaged_log_x <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", alpha = 0.2) +  # Deviation shaded area
    labs(x = "Vertices", y = "Mean Degree 2nd Moment", title = "log(x) -- y") +
    scale_x_log10()
  
  plot_averaged_log_y <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", alpha = 0.2) +  # Deviation shaded area
    labs(x = "Vertices", y = "Mean Degree 2nd Moment", title = "x -- log(y)") +
    scale_y_log10()
    
    plot_averaged_log_xy <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", alpha = 0.2) +  # Deviation shaded area
    labs(x = "Vertices", y = "Mean Degree 2nd Moment", title = "log(x) -- Log(y)") +
    scale_x_log10() +
    scale_y_log10()
    
  
  # Arrange all four plots in a 2x2 grid
  combined_plot_averaged <- grid.arrange(plot_averaged, plot_averaged_log_x, plot_averaged_log_y, plot_averaged_log_xy, 
                                nrow = 2, ncol = 2)
  ggsave(filename = paste0("plots/", lang, "_combined_plot_averaged.png"), plot = combined_plot_averaged)
}

# Sembla que el plot on es veu mes proper a una recta es el log(x) -- y
# Aixo indicaria que la <k2> segueix un log: <k2> ~ log(n)
