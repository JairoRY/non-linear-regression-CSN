library(ggplot2)
library(gridExtra)
library(latex2exp)

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
    labs(x = "Vertices (n)", 
         y = TeX("$\\langle k^2 \\rangle$")) +
    theme_minimal() +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=10))
  
  plot_simple_log_x <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    geom_point(size = point_size) +
    labs(x = "Log(n)", y = TeX("$\\langle k^2 \\rangle$")) +
    scale_x_log10() +
    theme_minimal() +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=10))
  
  plot_simple_log_y <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    geom_point(size = point_size) +
    labs(x = "Vertices (n)", y = TeX("Log($\\langle k^2 \\rangle)$")) +
    scale_y_log10() +
    theme_minimal() +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=10))
  
  plot_simple_log_xy <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    geom_point(size = point_size) +
    labs(x = "Log(n)", y = TeX("Log($\\langle k^2 \\rangle)$")) +
    scale_x_log10() +
    scale_y_log10() +
    theme_minimal() +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=10))
  
  combined_plot_simple <- grid.arrange(plot_simple, plot_simple_log_x, plot_simple_log_y, plot_simple_log_xy, 
                                       nrow = 2, ncol = 2)
  ggsave(filename = paste0("plots/", lang, "_combined_plots_simple.png"), plot = combined_plot_simple)
  
  # Averaged plot
  plot_averaged <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", alpha = 0.2) +  # Deviation shaded area
    labs(x = "Vertices (n)", y = TeX("Avg. $\\langle k^2 \\rangle$")) +
    theme_minimal() +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=10))
  
  plot_averaged_log_x <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", alpha = 0.2) +  # Deviation shaded area
    labs(x = "Log(n)", y = TeX("Avg. $\\langle k^2 \\rangle$")) +
    scale_x_log10() +
    theme_minimal() +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=10))
  
  plot_averaged_log_y <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", alpha = 0.2) +  # Deviation shaded area
    labs(x = "Vertices (n)", y = TeX("Log(Avg. $\\langle k^2 \\rangle)$")) +
    scale_y_log10() +
    theme_minimal() +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=10))
  
  plot_averaged_log_xy <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = "mean_cl_boot", geom = "ribbon", alpha = 0.2) +  # Deviation shaded area
    labs(x = "Log(n)", y = TeX("Log(Avg. $\\langle k^2 \\rangle)$")) +
    scale_x_log10() +
    scale_y_log10() +
    theme_minimal() +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=10))
  
  # Arrange all four plots in a 2x2 grid
  combined_plot_averaged <- grid.arrange(plot_averaged, plot_averaged_log_x, plot_averaged_log_y, plot_averaged_log_xy, 
                                         nrow = 2, ncol = 2)
  ggsave(filename = paste0("plots/", lang, "_combined_plot_averaged.png"), plot = combined_plot_averaged)
  
  plot_scaling_visualization <- ggplot(metrics, aes(x = vertices, y = degree_2nd_moment)) +
    geom_point(size = point_size) +
    stat_summary(fun = mean, geom = "line", aes(color = "Avg k2")) +
    geom_line(data = metrics, aes(x = vertices, y = (1 - 1/vertices) * (5 - 6/vertices), color = "Red")) +
    geom_line(data = metrics, aes(x = vertices, y = 4 - 6/vertices, color = "Blue1")) +
    geom_line(data = metrics, aes(x = vertices, y = vertices - 1, color = "Blue2")) +
    labs(x = "Vertices (n)", 
         y = TeX("Degree 2nd Moment $\\langle k^2 \\rangle$")) +
    scale_y_log10() + 
    scale_x_log10() +
    scale_color_manual(
      values = c("Avg k2" = "green", 
                 "Blue1" = "blue", 
                 "Red" = "red", 
                 "Blue2" = "blue"),
      labels = c(TeX("Avg. $\\langle k^2 \\rangle$"),
                 TeX("$n - 1$"),
                 TeX("$4 - \\frac{6}{n}$"),
                 TeX("$\\left( 1 - \\frac{1}{n} \\right) \\left( 5 - \\frac{6}{n} \\right)$"))
    ) +
    theme_minimal() +
    theme(axis.text=element_text(size=8), axis.title=element_text(size=10)) +
    labs(color = "")
  
  ggsave(filename = paste0("plots/", lang, "_scaling_visualization.png"), plot = plot_scaling_visualization)
}

# Els plots que semblen mes una recta son els que tenen log a l'eix x
# Aixo podria indicar que <k2> ~ log(n)

