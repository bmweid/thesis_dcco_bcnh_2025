# Load required packages
library(ggplot2)
library(moments)  # for skewness and kurtosis tests
library(nortest)  # for additional normality tests

# Function to test distribution of growth index data
test_distribution <- function(growth_index) {
  # Print number of NA values if any exist
  na_count <- sum(is.na(growth_index))
  if(na_count > 0) {
    cat("Warning: Found", na_count, "missing values\n\n")
  }
  
  # Remove NA values for analysis
  growth_index_clean <- na.omit(growth_index)
  
  # Basic summary statistics
  summary_stats <- summary(growth_index)
  cat("Summary Statistics:\n")
  print(summary_stats)
  cat("\n")
  
  # Calculate skewness and kurtosis
  sk <- skewness(growth_index_clean)
  kt <- kurtosis(growth_index_clean)
  cat("Skewness:", round(sk, 3), "\n")
  cat("Kurtosis:", round(kt, 3), "\n\n")
  
  # Perform multiple normality tests
  # Shapiro-Wilk test
  sw_test <- shapiro.test(growth_index_clean)
  cat("Shapiro-Wilk normality test:\n")
  print(sw_test)
  
  # Anderson-Darling test
  ad_test <- ad.test(growth_index_clean)
  cat("\nAnderson-Darling normality test:\n")
  print(ad_test)
  
  # Create diagnostic plots
  par(mfrow = c(2, 2))
  
  # Histogram with density curve
  hist(growth_index_clean, probability = TRUE, main = "Histogram with Normal Curve",
       xlab = "Growth Index", col = "lightblue")
  curve(dnorm(x, mean = mean(growth_index_clean), sd = sd(growth_index_clean)), 
        add = TRUE, col = "red", lwd = 2)
  
  # Q-Q plot
  qqnorm(growth_index_clean)
  qqline(growth_index_clean, col = "red")
  
  # Density plot
  plot(density(growth_index_clean), main = "Density Plot")
  curve(dnorm(x, mean = mean(growth_index_clean), sd = sd(growth_index_clean)), 
        add = TRUE, col = "red", lty = 2)
  
  # Boxplot
  boxplot(growth_index_clean, main = "Boxplot", ylab = "Growth Index")
  
  # Reset plotting parameters
  par(mfrow = c(1, 1))
  
  # Create and save a ggplot version of the histogram
  p <- ggplot(data.frame(x = growth_index_clean), aes(x = x)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(color = "red", size = 1) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(growth_index_clean), sd = sd(growth_index_clean)),
                  color = "blue", linetype = "dashed", size = 1) +
    labs(title = "Distribution of Growth Index",
         x = "Growth Index",
         y = "Density") +
    theme_minimal()
  
  print(p)
  
  return(list(summary_stats = summary_stats,
              skewness = sk,
              kurtosis = kt,
              shapiro_test = sw_test,
              anderson_darling_test = ad_test))
}

# Example usage:
# results <- test_distribution(df$growth_index)

# Example usage:
# Assuming your data is in a dataframe called 'df' with a column 'growth_index'
# If your data is in a CSV file:
data <- read.csv("cormorant-colony-analysis/current working files/working files for GitHub/test_thesis_repo/combined_dataset.csv")
results <- test_distribution(data$bcnh_growthindex)