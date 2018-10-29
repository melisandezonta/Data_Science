library("ggplot2")
# Functions -------------------------------------
log_factorial <- function (n) {
  # Return the log of factorial(n) for any integer n > 0
  if (n <= 1)
    return (0)
  return (log(n) + log_factorial(n - 1))
}

sum_log_factorial <- function (n) {
  # Return the sum of log_factorial(i) for i in 1..n
  sum <- 0
  for(i in seq(1, n, 1)) {
    sum <- sum + log_factorial(i)
  }
  return (sum)
}

fibonacci <- function(n) {
  # Return nth Fibonacci number
  if (n <= 1)
    return (n)
  return (fibonacci(n - 1) + fibonacci(n - 2))
}

# Compare Results ---------------------------------------------------------
# In order to compare the 3 methods performances, the running time is computed. 

options(expressions = 50000) #Increase the number of nested recursions allowed
arr1 = c() # array for the log factorial
arr2 = c() # array for the sum of the log factorial
arr3 = c() # array for the fibonacci function

N1 = 2000 # Number of iterations for loop and R built in methods
a = seq(10,N1,100)
for (max in a){
  time_log_factorial = function(max){
    v1 = system.time(for (e in seq(1,max,100)) log_factorial(e))
    return(v1[1]) # access to user time
  }
  time_sum_log_factorial = function(max){
    v3 = system.time(for (e in seq(1,max,100)) sum_log_factorial(e))
    return(v3[1]) # access to user time
  }
  arr1 = c(arr1,time_log_factorial(max))
  arr3 = c(arr3, time_sum_log_factorial(max))
}
N2 = 35  # Number of iterations for Fibonacci function
a2 = seq(1,N2,1)
for (max in a2){
  time_fibonacci = function(max){
    v2 = system.time(for (e in seq(1,max)) fibonacci(e))
    return(v2[1]) # access to user time
  }
  arr2 = c(arr2,time_fibonacci(max))
}

## Curves construction ------- 

d1 = data.frame(x = a ,y = arr1)
d2 = data.frame(x = a2 ,y = arr2)
d3 = data.frame(x = a ,y = arr3)

qplot(x = a,
      y = arr1,
      data = d1,
      main = "Running Time of log factorial function",
      geom = "point",
      xlab = 'n',
      ylab = 't')
ggsave('plot_log_factorial.png',width = 5,height = 5,path = "/Users/jacquelineroudes/Documents/GTL_courses/Data_Visual_Analytics/Assignments/ac3")

qplot(x = a,
      y = arr3,
      data = d3,
      main = "Running Time of the sum of log factorial function",
      geom = "point",
      xlab = 'n',
      ylab = 't')
ggsave('plot_sum_log_factorial.png',width = 5,height = 5,path = "/Users/jacquelineroudes/Documents/GTL_courses/Data_Visual_Analytics/Assignments/ac3")

d2 = data.frame(x = a2 ,y = arr2)

qplot(x = a2,
      y = arr2,
      data = d2,
      main = "Running Time of Fibonacci function",
      geom = "point",
      xlab = 'n',
      ylab = 't')
ggsave('plot_fibonacci.png',width = 5,height = 5)

qplot(x = log(a2),
      y = log(arr2),
      data = d2,
      main = "Running Time of Fibonacci function",
      geom = "point",
      xlab = 'log(n)',
      ylab = 'log(t)')
ggsave('plot_fibonacci_log.png',width = 5,height = 5)


