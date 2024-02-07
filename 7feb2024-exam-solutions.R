
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




