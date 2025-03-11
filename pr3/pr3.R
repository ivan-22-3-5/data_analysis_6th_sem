library(psych)
library(glue)

data <- read.csv("gym.csv", header = TRUE)

str(data)

get_mode <- function(col) {
  return(names(sort(table(col), decreasing = TRUE))[1])
}

metric_vars <- list("session_duration", "fat_percentage")

for (v in metric_vars) {
  cat("\nStatistics for:", v, "\n")

  v_data <- data[[v]]
  min_val <- min(v_data, na.rm = TRUE)
  max_val <- max(v_data, na.rm = TRUE)

  cat("Mean:", mean(v_data, na.rm = TRUE), "\n")
  cat("Median:", median(v_data, na.rm = TRUE), "\n")
  cat("Mode:", get_mode(v_data), "\n")
  cat("Standard Deviation:", sd(v_data, na.rm = TRUE), "\n")
  cat("Variance:", var(v_data, na.rm = TRUE), "\n")
  cat("Min:", min_val, "\n")
  cat("Max:", max_val, "\n")
  cat("Range:", max_val - min_val, "\n")
  cat("25th:", quantile(v_data, 0.25, na.rm = TRUE), "\n")
  cat("75th:", quantile(v_data, 0.75, na.rm = TRUE), "\n")

  hist(v_data, main = glue("Histogram for {v}"), xlab = v, col = "lightblue", breaks = 20)
  boxplot(v_data, main = glue("Boxplot for {v}"), col = "orange")

  print(describe(v_data))
}




