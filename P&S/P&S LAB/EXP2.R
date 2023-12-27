
##(1)
coins <- c(rep("gold", 20), rep("silver", 30), rep("bronze", 50))

sampleSpace <- sample(coins, size = 10, replace = FALSE)

print(sampleSpace)

##(2)
outcomes <- c("Success", "Failure")
probab <- c(0.9, 0.1)

# Generate a sample space for 10 surgical procedures
sample_space <- sample(outcomes, size = 10, replace = TRUE, prob = probab)

# Display
cat("Sample space for next 10 Procedures:\n", sample_space)

##(3)
###TAKING USER INPUT OF N
n <- as.integer(readline("Number of people in the room: "))

# Calculate probability
prob_no_shared <- 1
for (i in 1:n) {
  prob_no_shared <- prob_no_shared * (365 - i + 1) / 365
}

prob_shared <- 1 - prob_no_shared

cat("Probability that at least two people share a birthday in a room with", n, "people:", prob_shared, "\n")

##USING AN ARRAY OF VALUES FOR N
num_simulations <- 10000

for (n in c(5, 10, 15, 20, 25)) {
  shared_birthday_count <- 0
  
  for (sim in 1:num_simulations) {
    birthdays <- sample(1:365, size = n, replace = TRUE)
    if (length(birthdays) != length(unique(birthdays))) {
      shared_birthday_count <- shared_birthday_count + 1
    }
  }
  
  prob_shared <- shared_birthday_count / num_simulations
  cat("Estimated probability of shared birthday with", n, "people:", prob_shared, "\n")
}

##SMALLEST VALUE OF n FOR WHICH THE PROBABILITY IS GREATER THAN 0.5
num_simulations <- 10000
n <- 1
while (TRUE) {
  shared_birthday_count <- 0
  
  for (sim in 1:num_simulations) {
    birthdays <- sample(1:365, size = n, replace = TRUE)
    if (length(birthdays) != length(unique(birthdays))) {
      shared_birthday_count <- shared_birthday_count + 1
    }
  }
  
  prob_shared <- shared_birthday_count / num_simulations
  if (prob_shared > 0.5) {
    break
  }
  
  n <- n + 1
}

cat("Smallest value of n for which the probability is greater than 0.5:", n, "\n")


###(3)
conditional_probability <- function(prob_a, prob_b_given_a, prob_b) {
  prob_a_given_b <- (prob_b_given_a * prob_a) / prob_b
  return(prob_a_given_b)
}

# Given probabilities
prob_cloudy <- 0.4
prob_rain <- 0.2
prob_clouds_given_rain <- 0.85

# Compute the probability of rain given that it's cloudy
prob_rain_given_cloudy <- conditional_probability(prob_rain, prob_clouds_given_rain, prob_cloudy)

cat("Probability of rain given that it's cloudy:", prob_rain_given_cloudy, "\n")


###(4)
#Loading the dataset
data(iris)

# (a) Print first few rows of the dataset
head(iris)

# (b) Find the structure of the dataset
str(iris)

# (c) Find the range of sepal length
range_sepal_length <- range(iris$Sepal.Length)
cat("Range of sepal length:", range_sepal_length, "\n")

# (d) Find the mean of sepal length
mean_sepal_length <- mean(iris$Sepal.Length)
cat("Mean of sepal length:", mean_sepal_length, "\n")

# (e) Find the median of sepal length
median_sepal_length <- median(iris$Sepal.Length)
cat("Median of sepal length:", median_sepal_length, "\n")

# (f) Find the first and third quartiles and the interquartile range
quartiles_sepal_length <- quantile(iris$Sepal.Length, probs = c(0.25, 0.75))
iqr_sepal_length <- quartiles_sepal_length[2] - quartiles_sepal_length[1]
cat("First Quartile:", quartiles_sepal_length[1], "\n")
cat("Third Quartile:", quartiles_sepal_length[2], "\n")
cat("Interquartile Range:", iqr_sepal_length, "\n")

# (g) Find the standard deviation and variance of sepal length
sd_sepal_length <- sd(iris$Sepal.Length)
var_sepal_length <- var(iris$Sepal.Length)
cat("Standard Deviation of sepal length:", sd_sepal_length, "\n")
cat("Variance of sepal length:", var_sepal_length, "\n")

# (h) Perform the above exercises for other attributes (sepal.width, petal.length, petal.width)
##SEPAL.WIDTH
#Range
range_sepal_width <- range(iris$Sepal.Width)
cat("Range of sepal width:", range_sepal_width, "\n")
#Mean
mean_sepal_width <- mean(iris$Sepal.Width)
cat("Mean of sepal width:", mean_sepal_width, "\n")
#Median
median_sepal_width <- median(iris$Sepal.Width)
cat("Median of sepal width:", median_sepal_width, "\n")
#Quartiles
quartiles_sepal_width <- quantile(iris$Sepal.Width, probs = c(0.25, 0.75))
iqr_sepal_width <- quartiles_sepal_width[2] - quartiles_sepal_width[1]
cat("First Quartile of sepal width:", quartiles_sepal_width[1], "\n")
cat("Third Quartile of sepal width:", quartiles_sepal_width[2], "\n")
cat("Interquartile Range of sepal width:", iqr_sepal_width, "\n")
##Standard Deviation and Variance
sd_sepal_width <- sd(iris$Sepal.Width)
var_sepal_width <- var(iris$Sepal.Width)
cat("Standard Deviation of sepal width:", sd_sepal_width, "\n")
cat("Variance of sepal width:", var_sepal_width, "\n")

##PETAL.WIDTH
#Range
range_petal_width <- range(iris$Petal.Width)
cat("Range of petal width:", range_petal_width, "\n")
#Mean
mean_petal_width <- mean(iris$Petal.Width)
cat("Mean of petal width:", mean_petal_width, "\n")
#Median
median_petal_width <- median(iris$Petal.Width)
cat("Median of petal width:", median_petal_width, "\n")
#Quartiles
quartiles_petal_width <- quantile(iris$Petal.Width, probs = c(0.25, 0.75))
iqr_petal_width <- quartiles_petal_width[2] - quartiles_petal_width[1]
cat("First Quartile of petal width:", quartiles_petal_width[1], "\n")
cat("Third Quartile of petal width:", quartiles_petal_width[2], "\n")
cat("Interquartile Range of petal width:", iqr_petal_width, "\n")
##Standard Deviation and Variance
sd_petal_width <- sd(iris$Petal.Width)
var_petal_width <- var(iris$Petal.Width)
cat("Standard Deviation of sepal width:", sd_petal_width, "\n")
cat("Variance of sepal width:", var_petal_width, "\n")

##PETAL.LENGTH
#Range
range_petal_length <- range(iris$Petal.Length)
cat("Range of petal length:", range_petal_length, "\n")
#Mean
mean_petal_length <- mean(iris$Petal.Length)
cat("Mean of petal length:", mean_petal_length, "\n")
#Median
median_petal_length <- median(iris$Petal.Length)
cat("Median of petal length:", median_petal_length, "\n")
#Quartiles
quartiles_petal_length <- quantile(iris$Petal.Length, probs = c(0.25, 0.75))
iqr_petal_length <- quartiles_petal_length[2] - quartiles_petal_length[1]
cat("First Quartile of petal length:", quartiles_petal_length[1], "\n")
cat("Third Quartile of petal length:", quartiles_petal_length[2], "\n")
cat("Interquartile Range of petal length:", iqr_petal_length, "\n")
##Standard Deviation and Variance
sd_petal_length <- sd(iris$Petal.Length)
var_petal_length <- var(iris$Petal.Length)
cat("Standard Deviation of petal length:", sd_petal_length, "\n")
cat("Variance of petal length:", var_petal_length, "\n")


# (i) Use the built-in function summary on the dataset Iris
summary(iris)

#R does not have a standard in-built function to calculate mode. 
#So we create a user function to calculate mode of a data set in R. 
#This function n takes the vector as input and gives the mode value as output.
calculate_mode <- function(data) {
  table_data <- table(data)
  mode <- as.numeric(names(table_data[table_data == max(table_data)]))
  return(mode)
}

# Test the function
dataset <- c(1,0,2,1,0,3,4,4,7)
mode_value <- calculate_mode(dataset)
cat("Mode of the dataset:", mode_value, "\n")
