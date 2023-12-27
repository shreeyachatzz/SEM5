#Q1.
# Number of dice rolls
n <- 12
# Probability of getting a 6 on one roll
p_success <- 1/6

# Calculate the cumulative probabilities using binomial distribution
cum_prob_6 <- pbinom(6, size = n, prob = p_success)
cum_prob_9 <- pbinom(9, size = n, prob = p_success)

# Calculate the probability of getting 7, 8, or 9 sixes
prob_7_to_9 <- cum_prob_9 - cum_prob_6

cat("Probability of getting 7, 8, or 9 sixes:", prob_7_to_9, "\n")

###USING DBINOM
# Number of dice rolls
n <- 12
# Probability of getting a 6 on one roll
p_success <- 1/6

# Calculate the probabilities using binomial distribution (PDF)
prob_7 <- dbinom(7, size = n, prob = p_success)
prob_8 <- dbinom(8, size = n, prob = p_success)
prob_9 <- dbinom(9, size = n, prob = p_success)

# Calculate the probability of getting 7, 8, or 9 sixes
prob_7_to_9 <- prob_7+prob_8+prob_9

cat("Probability of getting 7, 8, or 9 sixes:", prob_7_to_9, "\n")

#Q2
# Mean and standard deviation
mean_score <- 72
standard_deviation <- 15.2

# Score threshold
threshold <- 84

# cumulative distribution function (CDF)
probability_above_threshold <- 1 - pnorm(threshold, mean = mean_score, sd = standard_deviation)

# probability to percentage
percentage_above_threshold <- probability_above_threshold * 100

cat("Percentage of students scoring 84 or more:", percentage_above_threshold, "%\n")

#Q3.
# library for Poisson distribution calculations
library(stats)

# Parameters for the Poisson distribution
lambda_x <- 5  # Average number of cars from 10AM to 11AM
lambda_y <- 50 # Average number of customers from 8AM to 6PM

# Probability that no car arrives during 10AM to 11AM
prob_x <- dpois(0, lambda = lambda_x)

# Probability of having between 48 and 50 customers (inclusive) from 8AM to 6PM
prob_y <- sum(dpois(48:50, lambda = lambda_y))

cat("Probability that no car arrives during 10AM to 11AM:", prob_x, "\n")
cat("Probability of having between 48 and 50 customers from 8AM to 6PM:", prob_y, "\n")

#Q4
# Parameters for hypergeometric distribution
total_processors <- 250
defective_processors <- 17
sample_size <- 5

# p(x=3)
prob_3 <- dhyper(3, m = defective_processors, n = total_processors - defective_processors, k = sample_size)

cat("Probability of exactly 3 defective processors in the sample:", prob_3, "\n")

#Q5
# Given probability
p_success <- 0.447

# Sample size
n <- 31

# (a) X is distributed as a binomial distribution
# (b) Sketch the probability mass function (PMF)
xx <- seq(0,31,1)
pmf_values <- numeric()
cdf_values <- numeric()

for(i in 1:length(xx))
{
  pmf_values[i] = dbinom(xx[i],n,p_success)
}

plot(xx,pmf_values)
# (c) Sketch the cumulative distribution function (CDF)

  for(i in 1:length(xx))
  {
    cdf_values[i] = pbinom(xx[i],n,p_success)
  }
# (d) Mean, variance, and standard deviation
mean_x <- n * p_success
variance_x <- n * p_success * (1 - p_success)
std_dev_x <- sqrt(variance_x)

# Print the results
cat("Mean of X:", mean_x, "\n")
cat("Variance of X:", variance_x, "\n")
cat("Standard Deviation of X:", std_dev_x, "\n")

# Plot PMF and CDF
plot(xx, pmf_values, xlab = "Number of Students (X)", ylab = "Probability", main = "Probability Mass Function (PMF) of X")
plot(xx, cdf_values, xlab = "Number of Students (X)", ylab = "Cumulative Probability", main = "Cumulative Distribution Function (CDF) of X")


