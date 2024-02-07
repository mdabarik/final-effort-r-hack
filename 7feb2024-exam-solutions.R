
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
  variance <- sqr_diff_sum / n
  return(variance)
}


