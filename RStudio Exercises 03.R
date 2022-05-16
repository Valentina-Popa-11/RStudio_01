##################################

#Define a function that returns the nth Fibonacci number
fib <- function(n){
  stopifnot(n > 0)
  if(n == 1){
    0
  } else if(n == 2){
    1
  } else {
    fib(n - 1) + fib(n - 2)
  }
}

#Define a function that returns Fibonacci number`s till the nth one
fibonacci <- function(n){
  x = c(1,n)
  x[1]=1
  x[2]=1
  for(i in 3:n) x[i] = x[i-1] + x[i-2]
  return(x)
}

#Construct a sequence of ratios of the form fn+1/fn, n = 1; 2; : : : ; 30.
n <-c(1:30)
fibonacciRatio = function(n) {
  x = fibonacci(n)
  y = x[(1:n)+1] / x[1:n]
  return(y)
}
fibonacciRatio(30)

# Does the sequence appear to be converging?
# Yes. A sequence converges when it keeps getting closer and closer to a
# certain value like we can see in our case.

#Compute the golden ratio 
golden_ratio <- (1+sqrt(5))/2

# Is the sequence converging to this ratio?
# Yes.

############### Construct a list of all twin primes less than 1000

# Implement the conditions for a prime number
isPrime <- function (n) n==2L || all (n %%  2L:max (2, floor(sqrt(n)))!=0)

# Find all prime numbers between 1 and 1000
primes_list <- vector(length = 0, mode = "integer")
for (i in 1:1000) {
  if (isPrime(i)) primes_list <- c(primes_list, i)
}

testThese <- 1:1000
primes<- testThese[sapply(testThese,isPrime)]

#Construct a list of all twin primes less than 1000
pairs.temp <- which(diff(primes)==2)

pairs<- sort(c(pairs.temp, pairs.temp+1))

matrix(primes[pairs], ncol=2, byrow=TRUE)

##################################

install.packages("NLRoot")
# upload NLRoot library from the package
library("NLRoot")

# define the function
func <- function(x) {
  x^3 + 2 * (x^2) - 7
}

#plot the function to check the interval containing the root.
curve(func, xlim=c(-3,3), col='red', lwd=3, lty=4)
abline(h=0)
abline(v=0)

#root is known to lie between 0 and 2, so we use the interval [0,2].
find_root<-BFfzero(func, 0, 2)

# Why do you think that your bisection algorithm is guaranteed to converge 
# for any continuous function which takes opposite-signed values at 0 and 2?
# bisection algorithm is guaranteed to converge for any continuous function 
# which takes opposite-signed values at 0 and 2 because theoretical underpinning
# of the algorithm, based on intermediate value theorem which states that if a 
# continuous function f takes values f(a) and f(b) at the end points of the
# interval [a,b], then f must take all values between f(a) and f(b) somewhere 
# in the interval.

#calculate how many loops it will take.
a=0
b=2
nr_loops <- ceiling((log(b-a)-log(0.0000005))/log(2))
nr_loops

#######################################

#Generate two vectors of 10000 values each having the uniform distribution (0; 1).
U1 <- c(runif(1000, min=0, max=1))
U2 <- c(runif(1000, min=0, max=1))

# Estimate E[U1 + U2] and estimate of E[U1] + E[U2]
add_vect <- U1 + U2
mean_add_vect <- mean(add_vect)
add_means <- mean(U1) + mean(U2)

true_value <- ((1-0)/2)*2

# E[U1 + U2] compared with true value
mean_add_vect < true_value 
mean_add_vect > true_value 
mean_add_vect == true_value 

# E[U1 + U2] compared with an estimate of E[U1] + E[U2]
mean_add_vect < add_means
mean_add_vect > add_means
mean_add_vect == add_means

#Estimate Var (U1 + U2) and Var (U1) + Var (U2). 
var_of_add <- var(U1+U2)
add_of_var <- var(U1) + var(U2)

# Are they equal?
var_of_add == add_of_var

#Should the true values be equal?
# no

########################################

#Generate 1000 integers ranging from 1 to 10 that are uniformly distributed.
int_un_dist <-sample(1:10, 1000, replace=TRUE)

#Use the table() function to check whether the observed frequencies for each
#value are close to what you expect.
table(int_un_dist)

#Calculate the observed proportions
N = sum(int_un_dist)
N
observed_freq = int_un_dist/N
observed_freq

#Determine expected frequences
hypothesized_proportion=1/10
expected_frequence= N * hypothesized_proportion

###############################################

# Simulate 10000 binomial pseudorandom numbers with parameters 2 and 0.3, 
# assigning them to a vector called binsim.
binsim<-rbinom(10000,20, 0.3)

################################################
# assign 10000 Poisson numbers with parameter ?? = 7.2 to a vector, a
a<-rpois(10000, 7.2)

# plot a bar chart of the numbers
barplot(table(a))

# The expected value of a Poisson random variable is given by E[V ] = ??
# The variance of a random variable is calculated from the formula: Var(X) = ??
lambda <- 7.2
lambda # Expected Value, and Variance

# calculate the mean and variance of the sample
mean(a)
var(a)

# Regarding theoretical values, the mean and variance are not equal for Poisson 
# random variables. In our estimation, both of them have the same value

