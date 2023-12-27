
# (a) Import the csv data file in R
library(readr)
Clt_data <- read_csv("C:/Users/shree/Downloads/Clt-data.csv")
View(Clt_data)

# (b) Validate data for correctness
# Count number of rows
n_rows <- nrow(Clt_data)
print(paste("Number of rows: ", n_rows))
# View the top ten rows of the dataset
head(Clt_data, 10)
# Data about the column names
colnames(Clt_data)
str(Clt_data)


# (c) Calculate the population mean
population_mean <- mean(Clt_data$`Wall Thickness`)
print(population_mean)

# Plot the histogram of the observations
hist(Clt_data$`Wall Thickness`, breaks = 20, col = "lightblue", xlab = "Wall Thickness", main = "Histogram of Wall Thickness")
# Add a vertical line for population mean
abline(v = population_mean, col = "red", lwd = 2)

# Function to draw sufficient samples and plot histograms
draw_samples_and_plot <- function(sample_size) {
  # Generate 1000 samples of specified size
  sample_means <- replicate(1000, mean(sample(Clt_data$'Wall Thickness', size = sample_size, replace = TRUE)))
  
  # Plot histogram of sample means
  hist(sample_means, breaks = 30, col = "lightgreen", xlab = "Sample Means", main = paste("Sampling Distribution (Sample Size =", sample_size, ")"))
  
  # Add a vertical line for the population mean
  abline(v = population_mean, col = "red", lwd = 2)
}

# (a) Draw samples of size 10 and plot their means
draw_samples_and_plot(10)

# (b) Repeat for sample sizes 50, 500, and 9000
draw_samples_and_plot(50)
draw_samples_and_plot(500)
draw_samples_and_plot(9000)
