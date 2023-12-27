#(1)
v1<-c(5,10,15,20,25,30)
print(paste("Maximum Number is: ", max(v1)))
print(paste("Minimum Number is: ", min(v1)))

#(2)
factorial_result <- 1
n <- as.integer(readline(prompt = "Enter integer: "))

if (n <= 0) {
  print('Error')
} else {
  for (i in 1:n) {
    factorial_result <- factorial_result * i
  }
  print(paste("Factorial of", n, "is", factorial_result))
}


#(3)
n <- as.integer(readline("Enter the value of n: "))
if (n <= 2) {
  print('Error')
}else {
  fib <- numeric(n)
  fib[1] <- 0
  fib[2] <- 1
  
  for (i in 3:n) {
    fib[i] <- fib[i - 1] + fib[i - 2]
  }
  
  cat("Fibonacci sequence of", n, "terms:", fib)
}


#(4)
num1 <- as.numeric(readline("Enter the first number: "))
num2 <- as.numeric(readline("Enter the second number: "))

cat("Select operation:\n1. Add\n2. Subtract\n3. Multiply\n4. Divide\n")
choice <- as.integer(readline("Enter choice (1/2/3/4): "))

result <- switch(choice,
                 "1" = num1 + num2,
                 "2" = num1 - num2,
                 "3" = num1 * num2,
                 "4" = {
                   if (num2 == 0) {
                     stop("Error: Division by zero is not allowed.")
                   }
                   num1 / num2
                 },
                 stop("Error: Invalid choice."))

cat("Result:", result)

#(5)
# Load necessary library for plotting
install.packages("plotrix")
library(plotrix)

cities <- c("Kolkata", "Mumbai", "Delhi", "Chennai", "Patiala")
values <- c(5, 8, 30, 22, 55)

print(pie(values, labels = cities, main = "City Distribution"))

#BAR GRAPH
bar_colors <- c("red", "green", "blue", "orange", "purple")
barplot(values, names.arg = cities, main = "City Distribution")

#HISTOGRAM
data <- rnorm(100)
hist(data, main = "Histogram of Random Data", xlab = "Value", ylab = "Frequency", col = "blue")

#SCATTER PLOT
x <- rnorm(50)
y <- 2 * x + rnorm(50)

plot(x, y, main = "Scatter Plot", xlab = "X", ylab = "Y", col = "red", pch = 19)

#LINE PLOT
x <- seq(0, 2 * pi, length.out = 100)
y <- sin(x)

plot(x, y, type = "l", main = "Sine Function", xlab = "X", ylab = "Y", col = "green")

#BOX PLOT
data <- matrix(rnorm(200), ncol = 4)#random data
boxplot(data, main = "Box Plot of Random Data", col = c("red", "blue", "green", "purple"))

