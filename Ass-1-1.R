

# My first comments

# my function-1
calculate_weights <- function(n) {
  
  # Compute the expected values of the order statistics
  m <- qnorm((1:n - 0.375) / (n + 0.25))
  
  # Construct the covariance matrix
  V <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      V[i, j] <- min(i, j) - i * j / (n + 1)
    }
  }
  
  # Calculate the inverse of the covariance matrix
  V_inv <- solve(V)
  
  # Calculate the coefficients
  a <- as.numeric((m %*% V_inv) / sqrt(sum((m %*% V_inv)^2)))
  
  return(a)
}

# Function to calculate Shapiro-Wilk test statistic
shapiro_wilk_test <- function(data, qqplot = FALSE) {
  # Input validation
  if (!is.numeric(data)) {
    stop("\n Data must be numeric") # Added \n so that test passes! Weird stuff to catch
  }
  if (any(is.na(data))) {
    stop("\n Data contains NA values")
  }
  if (any(is.infinite(data))) {
    stop("\n Data contains infinite values")
  }
  if (length(data) < 3) {
    stop("\n Data must contain at least 3 values")
  }
  
  n <- length(data)
  if (n > 5000) {
    stop("Sample size must be between 3 and 5000")
  }
  
  if (!is.logical(qqplot)) {
    stop("Check the optional argument. By default, it's FALSE and can be set to
         TRUE")
  }
  # Calculate weights
  a <- calculate_weights(n)
  
  # Order the data
  sorted_data <- sort(data)
  
  # Calculate the sample mean
  x_bar <- mean(data)
  
  # Calculate W
  W <- (sum(a * sorted_data)^2) / sum((sorted_data - x_bar)^2)
  #cat("The value of W:",W)
  
  if (qqplot) {
    qqnorm(data)
    qqline(data, col = 2)
  }
  return(W)
}

shapiro_wilk_test(c(2,3,4,32),TRUE)
