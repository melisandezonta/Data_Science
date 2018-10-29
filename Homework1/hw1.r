##GT account name : mzr3


# Log Gamma (Loop) --------------------------------------------------------
# This function will compute the log Gamma for a value 
# of n which has to be positive. To make so, a loop is used.

log_gamma_loop = function(n){
if (n <= 0){
  return('n has to be positive')
}
if (n == 1){
  return(log(n))
}
else{
  a = 0
  for (i in seq(2,n, by = 1)){
    a = a + log(i-1)
    }
  return(a)
  }
}
print(log_gamma_loop(1))

# Log Gamma (Recursive) ---------------------------------------------------
# This function will compute the log Gamma for a value 
# of n which has to be positive. To make so, a recursive 
#function is used.
log_gamma_recursive = function(n){
  if (n <=  0){
    return('n has to be positive')
  }
  if (n == 1){
    return (0)}   
  else {
    return (log(n-1) + log_gamma_recursive(n-1))}
}
print(log_gamma_recursive(5))


# Sum of Log Gamma (Loop) --------------------------------------------------------
#Using the log_gamma_loop(), this function will compute the sum
# of the values until log_gamma_loop(n) is reached. A loop is used to do so.
sum_log_gamma_loop = function(n){
  if (n <= 0){
    return('n has to be positive')
  }
  a = 0
  for (i in seq(1,n, by = 1)){
    a = a + log_gamma_loop(i)
  }
return(a)
}
print(sum_log_gamma_loop(8))


# Sum of Log Gamma (Recursive) --------------------------------------------
#Using the log_gamma_recursive(), this function will compute the sum
# of the values until log_gamma_recursive(n) is reached. A recursive function
#is used to do so.
sum_log_gamma_recursive = function(n){
  if (n < 0){
    return('n has to be positive')
  }
  if (n == 1){
    return (0)}
  else {
    return (log_gamma_recursive(n) + sum_log_gamma_recursive(n-1))}
}
print(sum_log_gamma_recursive(8))


# Sum of lgamma ----------------------------------
#Using the R built in function lgamma(), this function will compute the sum
# of the values until lgamma is reached. A loop function
#is used to do so.
sum_lgamma = function(n){
  if (n < 0){
    return('n has to be positive')
  }
  a = 0
  for (i in seq(1,n, by = 1)){
    a = a + lgamma(i)
  }
  return(a)
}
print(sum_lgamma(8))


# Compare Results ---------------------------------------------------------
# In order to compare the 3 methods performances, the running time is computed. 

options(expressions = 50000) #Increase the number of nested recursions allowed
arr1 = c() # array for the loop method
arr2 = c() # array for the recursive method
arr3 = c() # array for the R built in method

N1 = 1000 # Number of iterations for loop and R built in methods
a = seq(10,N1,50)
for (max in a){
    time_loop = function(max){
      v1 = system.time(for (e in seq(1,max,100)) sum_log_gamma_loop(e))
      return(v1[1]) # access to user time
    }
    time_lgamma = function(max){
      v3 = system.time(for (e in seq(1,max,100)) sum_lgamma(e))
      return(v3[1]) # access to user time
    }
    arr1 = c(arr1,time_loop(max))
    arr3 = c(arr3, time_lgamma(max))
}
N2 = 1000 # Number of iterations for recursive method
a2 = seq(10,N2,50)
for (max in a2){
    time_recursive = function(max){
      v2 = system.time(for (e in seq(1,max,100)) sum_log_gamma_recursive(e))
      return(v2[1]) # access to user time
    }
  arr2 = c(arr2,time_recursive(max))
}
## Curves construction

names = c('Loop method','Recursive method','Built-in function')
max_arr = max(c(max(arr1),max(arr2),max(arr3)))
plot_colors <- c("blue","red","forestgreen")
plot(a,arr1, type="o", col=plot_colors[1], 
     ylim=c(0,max_arr),axes=FALSE, ann=FALSE)
y = seq(0,max_arr,by = 0.5)
axis(2, las=1, at=y)
axis(1, las=1 ,at=a)
box()
lines(a2,arr2, type="o", pch=22, lty=2, 
      col=plot_colors[2])
lines(a,arr3, type="o", pch=23, lty=3, 
      col=plot_colors[3])

title(main="Running time", col.main="red", font.main=4)
title(xlab= "Iteration number", col.lab=rgb(0,0.5,0))
title(ylab= "Time", col.lab=rgb(0,0.5,0))
legend("topleft", names, cex=0.8, col=plot_colors, 
       pch=21:23, lty=1:3)





