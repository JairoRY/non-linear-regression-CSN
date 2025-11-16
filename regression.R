library(dplyr)

AIC <- function(rss, n, p) {
  n*log(2*pi) + n*log(rss/n) + n + 2*(p + 1)
}

check_homoscedasticity <- function(n, y) {
  # quick linear fit as baseline
  fit <- lm(y ~ n)
  res <- abs(resid(fit))
  
  # regress absolute residuals on n
  test <- lm(res ~ n)
  
  # if slope significant and positive, then heteroscedastic
  pval <- summary(test)$coefficients["n","Pr(>|t|)"]
  slope <- coef(test)["n"]
  
  is_homo <- !(pval < 0.05 && slope > 0)
  return(is_homo)
}

files <- list.files("data", pattern = "_dependency_tree_metrics.txt$", full.names = TRUE)
langs <- sub("_dependency_tree_metrics.txt$", "", basename(files))

# Models (same as before, but correct per lab)
f0 <- function(n) (1 - 1/n)*(5 - 6/n)
f1 <- function(n, b) (n/2)^b
f2 <- function(n, a, b) a*n^b
f3 <- function(n, a, c) a*exp(c*n)
f4 <- function(n, a) a*log(n)
f1p <- function(n, b, d) (n/2)^b + d
f2p <- function(n, a, b, d) a*n^b + d
f3p <- function(n, a, c, d) a*exp(c*n) + d
f4p <- function(n, a, d) a*log(n) + d

# Models list
model_names <- c("M0","M1","M2","M3","M4","M1+","M2+","M3+","M4+")

# Prepare tables
table_homo <- matrix(NA, nrow = length(langs), ncol = 1, dimnames = list(langs, c("Homoscedastic")))
table_s <- matrix(NA, nrow = length(langs), ncol = length(model_names), dimnames = list(langs, model_names))
table_AIC <- matrix(NA, nrow = length(langs), ncol = length(model_names), dimnames = list(langs, model_names))
table_delta <- matrix(NA, nrow = length(langs), ncol = length(model_names), dimnames = list(langs, model_names))
param_names <- c("M1_b","M2_a","M2_b","M3_a","M3_c","M4_a","M1+_b","M1+_d","M2+_a","M2+_b","M2+_d","M3+_a","M3+_c","M3+_d","M4+_a","M4+_d")
table_params <- matrix(NA, nrow = length(langs), ncol = length(param_names), dimnames = list(langs, param_names))

# Loop languages
for (i in seq_along(files)) {
  fp <- files[i]
  lang <- langs[i]
  df <- read.table(fp, header = FALSE, col.names = c("vertices","degree_2nd_moment","mean_length"))
  df <- df %>% filter(!is.na(vertices), !is.na(degree_2nd_moment))
  
  # Check homoscedasticity
  is_homo <- check_homoscedasticity(df$vertices, df$degree_2nd_moment)
  table_homo[lang, "Homoscedastic"] <- is_homo
  
  if (is_homo) {
    use_df <- df
  } else {
    use_df <- aggregate(degree_2nd_moment ~ vertices, data = df, FUN = mean)
  }
  
  n_vec <- use_df$vertices
  y_vec <- use_df$degree_2nd_moment
  N <- length(y_vec)
  
  # Model 0
  pred0 <- f0(n_vec)
  rss0 <- sum((y_vec - pred0)^2)
  s0 <- sqrt(rss0/(N - 0))
  table_AIC[lang, "M0"] <- AIC(rss0, N, 0)
  table_s[lang, "M0"] <- s0
  
  # Model 1
  linear_model <- lm(log(degree_2nd_moment)~log(vertices/2), data = use_df)
  b1_initial <- coef(linear_model)[2]
  fit1 <- try(nls(y_vec ~ f1(n_vec, b), start = list(b = b1_initial), trace = TRUE))
  if (inherits(fit1, "nls")) {
    rss <- deviance(fit1)
    p <- length(coef(fit1))
    table_AIC[lang, "M1"] <- AIC(rss, N, p)
    table_s[lang, "M1"] <- sqrt(rss/df.residual(fit1))
    table_params[lang, "M1_b"] <- coef(fit1)["b"]
  }
  
  # Model 2
  linear_model <- lm(log(degree_2nd_moment)~log(vertices), data = use_df)
  a2_initial <- exp(coef(linear_model)[1])
  b2_initial <- coef(linear_model)[2]
  fit2 <- try(nls(y_vec ~ f2(n_vec, a, b), start = list(a = a2_initial, b = b2_initial), trace = TRUE))
  if (inherits(fit2, "nls")) {
    rss <- deviance(fit2)
    p <- length(coef(fit2))
    table_AIC[lang, "M2"] <- AIC(rss, N, p)
    table_s[lang, "M2"] <- sqrt(rss/df.residual(fit2))
    table_params[lang, "M2_a"] <- coef(fit2)["a"]
    table_params[lang, "M2_b"] <- coef(fit2)["b"]
  }
  
  # Model 3
  linear_model <- lm(log(degree_2nd_moment)~vertices, data = use_df)
  a3_initial <- exp(coef(linear_model)[1])
  c3_initial <- coef(linear_model)[2]
  fit3 <- try(nls(y_vec ~ f3(n_vec, a, c), start = list(a = a3_initial, c = c3_initial), trace = TRUE))
  if (inherits(fit3, "nls")) {
    rss <- deviance(fit3)
    p <- length(coef(fit3))
    table_AIC[lang, "M3"] <- AIC(rss, N, p)
    table_s[lang, "M3"] <- sqrt(rss/df.residual(fit3))
    table_params[lang, "M3_a"] <- coef(fit3)["a"]
    table_params[lang, "M3_c"] <- coef(fit3)["c"]
  }
  
  # Model 4
  linear_model <- lm(degree_2nd_moment~log(vertices), data = use_df)
  a4_initial <- coef(linear_model)["log(vertices)"]
  fit4 <- try(nls(y_vec ~ f4(n_vec, a), start = list(a = a4_initial), trace = TRUE))
  if (inherits(fit4, "nls")) {
    rss <- deviance(fit4)
    p <- length(coef(fit4))
    table_AIC[lang, "M4"] <- AIC(rss, N, p)
    table_s[lang, "M4"] <- sqrt(rss/df.residual(fit4))
    table_params[lang, "M4_a"] <- coef(fit4)["a"]
  }
  
  # Model 1+
  fit1p <- try(nls(y_vec ~ f1p(n_vec, b, d), start = list(b = b1_initial, d = 0), trace = TRUE))
  if (inherits(fit1p, "nls")) {
    rss <- deviance(fit1p)
    p <- length(coef(fit1p))
    table_AIC[lang, "M1+"] <- AIC(rss, N, p)
    table_s[lang, "M1+"] <- sqrt(rss/df.residual(fit1p))
    table_params[lang, "M1+_b"] <- coef(fit1p)["b"]
    table_params[lang, "M1+_d"] <- coef(fit1p)["d"]
  }
  
  # Model 2+
  fit2p <- try(nls(y_vec ~ f2p(n_vec, a, b, d), start = list(a = a2_initial, b = b2_initial, d = 0), control = nls.control(maxiter=500), trace = TRUE))
  if (inherits(fit2p, "nls")) {
    rss <- deviance(fit2p)
    p <- length(coef(fit2p))
    table_AIC[lang, "M2+"] <- AIC(rss, N, p)
    table_s[lang, "M2+"] <- sqrt(rss/df.residual(fit2p))
    table_params[lang, "M2+_a"] <- coef(fit2p)["a"]
    table_params[lang, "M2+_b"] <- coef(fit2p)["b"]
    table_params[lang, "M2+_d"] <- coef(fit2p)["d"]
  }
  
  # Model 3+
  fit3p <- try(nls(y_vec ~ f3p(n_vec, a, c, d), start = list(a = a3_initial, c = c3_initial, d = 0), control = nls.control(maxiter=500), trace = TRUE))
  if (inherits(fit3p, "nls")) {
    rss <- deviance(fit3p)
    p <- length(coef(fit3p))
    table_AIC[lang, "M3+"] <- AIC(rss, N, p)
    table_s[lang, "M3+"] <- sqrt(rss/df.residual(fit3p))
    table_params[lang, "M3+_a"] <- coef(fit3p)["a"]
    table_params[lang, "M3+_c"] <- coef(fit3p)["c"]
    table_params[lang, "M3+_d"] <- coef(fit3p)["d"]
  }
  
  # Model 4+
  fit4p <- try(nls(y_vec ~ f4p(n_vec, a, d), start = list(a = a4_initial, d = 0), trace = TRUE))
  if (inherits(fit4p, "nls")) {
    rss <- deviance(fit4p)
    p <- length(coef(fit4p))
    table_AIC[lang, "M4+"] <- AIC(rss, N, p)
    table_s[lang, "M4+"] <- sqrt(rss/df.residual(fit4p))
    table_params[lang, "M4+_a"] <- coef(fit4p)["a"]
    table_params[lang, "M4+_d"] <- coef(fit4p)["d"]
  }
  
  # Compute Δ (delta) = AIC - min AIC in that language
  rowA <- table_AIC[lang, ]
  minA <- min(rowA, na.rm = TRUE)
  table_delta[lang, ] <- rowA - minA
}

df_homo <- as.data.frame(table_homo)
df_s <- as.data.frame(table_s)
df_AIC <- as.data.frame(table_AIC)
df_delta <- as.data.frame(table_delta)
df_params <- as.data.frame(table_params)

cat("\n=== Homoscedasticity test ===\n")
print(df_homo)

cat("\n=== s ===\n")
print(df_s)

cat("\n=== AICs ===\n")
print(df_AIC)

cat("\n=== Δ AIC ===\n")
print(df_delta)

cat("\n=== Parameter estimates ===\n")
print(df_params)
