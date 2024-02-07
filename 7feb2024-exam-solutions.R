
#1(a)
Definition:
Function is a reuseable code in a program. Function makes code 
reuseable. It is easy to maintain, instead repeating same code over and over again. It follows the rule DRY principle.
Function takes input and give us input by processing the input. 

General syntax:
function_name <- function(param1, param2) {
  # process input
  return (param1 + param2)
}

factorial <- function(n) {
  if (n < 0) {
    return ("Invalid input");
  }
  if (n == 0) {
    return (1);
  }
  fact = 1;
  for (i in 1:n) {
    fact = fact * i;
  }
  return (fact);
}
print(factorial(10))
  

#1(b)
arithMean <- function(vector) {
  # size of the given vector
  n = length(vector)
  sum = 0;
  for (value in vector) {
    sum = sum + value
  }
  return (sum / n)
}

# HM = n / ((1/x₁) + (1/x₂) + (1/x₃) + ... + (1/xₙ))
harmMean <- function(vector) {
  n = length(vector)
  # sum of (1/x₁) + (1/x₂) ...
  sum_reciprocals = 0 # 1/n consider reciprocals
  for (x in vector) {
    sum_reciprocals = sum_reciprocals + (1/x);
  }
  hm = n / sum_reciprocals
  return (hm)
}

# Weighted Mean
weightedMean = function(weights, values) {
  n = length(values);
  sumWiXi = 0;
  for (i in 1:n) {
    Wi = weights[i];
    Xi = values[i];
    sumWiXi = sumWiXi + (Wi * Xi);
  }
  sumWi = 0;
  for (i in weights) {
    sumWi = sumWi + i;
  }
  return (sumWiXi / sumWi);
}

get_mean <- function(vector) {
  n = length(vector)
  sum = 0;
  for (value in vector) {
    sum = sum + value
  }
  return (sum / n)
}

variance <- function(dataset) {
  n <- length(dataset)
  mean <- get_mean(dataset)
  sqr_diff_sum <- sum((dataset - mean)^2) # Σ (xᵢ - μ)²
  var <- sqr_diff_sum / n
  return(variance)
}


#2 (b)
for (index in 1:n) { # 1:n is a vector
  # iteration
}

# flowchart draw it

# countAndPrint
countAndPrint <- function(vector) {
  count = 0
  for (i in 1:length(vector)) {
    if (vector[i] %% 2 == 0) {
      count = count + 1;
    }
  }
  if (count == 0) {
    print("No even number exists.");
    return ();
  }
  evens = seq(1, count)
  index = 1
  for (i in vector) {
    if (i %% 2 == 0) {
      evens[index] = i;
      index = index + 1;
    }
  }
  
  print("Count:");
  print(count);
  print("Even numbers");
  print(evens);
}

countAndPrint(c(1,3,3,5,7))


2 (b)
write a R functions that prints out mean, standard deviation, IQR and coefficient of variation without using any builtin functions. 


get_mean <- function(vector) {
  # size of the given vector
  n = length(vector)
  sum = 0;
  for (value in vector) {
    sum = sum + value
  }
  return (sum / n)
}

median <- function(vector) {
  vector = sort(vector)
  n = length(vector);
  if (n %% 2 == 1) {
    index = floor(n/2) + 1;
    return(vector[index]);
  }
  index1 = n/2
  index2 = (n/2) + 1
  med = (vector[index1] + vector[index2]) / 2;
  return (med);
}

variance <- function(dataset) {
  n <- length(dataset)
  mean <- get_mean(dataset)
  sqr_diff_sum <- sum((dataset - mean)^2) # Σ (xᵢ - μ)²
  var <- sqr_diff_sum / n
  return(var)
}


IQR <- function(x) {
  Q3 <- quantile(x, 0.75)  # Calculate the third quartile
  Q1 <- quantile(x, 0.25)  # Calculate the first quartile
  IQR_value <- Q3 - Q1  # Calculate the Interquartile Range
  return(IQR_value)
}

# Define a function to calculate the Coefficient of Variation
cv <- function(x) {
  mean_value <- get_mean(x)  # Calculate the mean
  sd_value <- sqrt(sum((x - mean_value)^2) / (length(x) - 1))  # Calculate the standard deviation
  coefficient_of_variation <- (sd_value / mean_value) * 100  # Calculate the Coefficient of Variation
  return(coefficient_of_variation)
}

printAll <- function() {
  vector = c(1,2,3,4,5,6,7,8,9,10)
  print(get_mean(vector))
  print(median(vector))
  standard_deviation = variance(vector) ^ 0.5
  print(standard_deviation)
  print(IQR(vector))
  print(cv(vector))
}

printAll()

# 3(a)
# Data
data <- c(55, 76,70,68,67,83,63,67,69,70,80,71,59,61,75,74,72,77,70,57,81,78,71,71,89,75,70,76,60,64,73,67,64,72,64,67,77,64,72,68)

# Histogram
hist(data, main = "Histogram of Data", xlab = "Values", ylab = "Frequency", col = "skyblue", border = "white")

# Density Plot
plot(density(data), main = "Density Plot of Data", xlab = "Values", ylab = "Density", col = "blue")

# Box Plot
boxplot(data, main = "Boxplot of Data", ylab = "Values", col = "lightgreen", border = "brown", horizontal = TRUE)



# 3(b)
x <- c(1, 2, 3, 4, 5)
y <- c(2, 3, 5, 4, 6)

n = length(x)
mean_x <- sum(x) / n
mean_y <- sum(y) / n

#  covariance
cov_xy <- sum((x - mean_x) * (y - mean_y)) / (n - 1)

# variance of x and y
var_x <- sum((x - mean_x)^2) / (n - 1)
var_y <- sum((y - mean_y)^2) / (n - 1)

# Pearson correlation coefficient
pearson_corr <- cov_xy / (sqrt(var_x) * sqrt(var_y))

# regression coefficient (slope)
beta <- cov_xy / var_x

# Print results
print(paste("Pearson correlation coefficient:", pearson_corr))
print(paste("Regression coefficient (slope):", beta))

# 3 (c)

calculate_grade <- function(mark) {
  if (mark >= 90) {
    grade_point <- 4.0
    letter_grade <- "A+"
  } else if (mark >= 80) {
    grade_point <- 4.0
    letter_grade <- "A"
  } else if (mark >= 70) {
    grade_point <- 3.0
    letter_grade <- "B"
  } else if (mark >= 60) {
    grade_point <- 2.0
    letter_grade <- "C"
  } else if (mark >= 50) {
    grade_point <- 1.0
    letter_grade <- "D"
  } else {
    grade_point <- 0.0
    letter_grade <- "F"
  }
  
  return(list(grade_point = grade_point, letter_grade = letter_grade))
}

# Example usage
mark <- 85
result <- calculate_grade(mark)
cat("Grade Point:", result$grade_point, "\n")
cat("Letter Grade:", result$letter_grade, "\n")






















