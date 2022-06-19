
#install.packages("modeest")
#install.packages("moments")

####################### 1st Exercise ###########################
# Simulate 1000 realizations of a standard normal random variable. Without the 
# additional arguments, rnorm gives the standard normal random variable, with mean 0 and sd 1
set.seed(1)
sim<-rnorm(1000)

#a 

#P(Z > 2.5)
simulated_p_less_2.5 <- sum(sim > 2.5) / length(sim)
simulated_p_less_2.5
theoretica_value_p_greater_2.5 <- 1-pnorm(2.5,0,1)
theoretica_value_p_greater_2.5

# Compare with the theoretical values.
simulated_p_less_2.5 < theoretical_value_p_greater_2.5
simulated_p_less_2.5 > theoretical_value_p_greater_2.5
simulated_p_less_2.5 == theoretical_value_p_greater_2.5

#b 
#P(0 < Z < 1.645)
#create a vector that contains TRUE at each index of the sample that is greater 
#than given number, and contains FALSE at each index of the sample that is less 
#than or equal to the given number
sim_vec1 <- sim < 1.645
sim_vec2 <- sim < 0
head(sim_vec1)
head(sim_vec2)
simulated_p_between_0_and_1.6<-sum(sim_vec1 == TRUE)/length(sim_vec1)-
              sum(sim_vec2 == TRUE)/length(sim_vec2)

theoretical_value_p_between_0_and_1.6 <- pnorm(1.645,0,1) - pnorm(0,0,1)

# Compare with the theoretical values
theoretical_value_p_between_0_and_1.6 == simulated_p_between_0_and_1.6
theoretical_value_p_between_0_and_1.6 > simulated_p_between_0_and_1.6
theoretical_value_p_between_0_and_1.6 < simulated_p_between_0_and_1.6

#c
#P(1.2 < Z < 1.45)
sim_vect1 <- sim < 1.45
sim_vect2 <- sim < 1.2
simulated_p_between_1.2_and_1.6<-sum(sim_vect1 == TRUE)/length(sim_vect1)-
              sum(sim_vect2 == TRUE)/length(sim_vect2)

theoretical_value_p_between_1.2_and_1.4 <- pnorm(1.45, 0, 1) - pnorm(1.2, 0, 1)

# Compare with the theoretical values
theoretical_value_p_between_1.2_and_1.4 == simulated_p_between_1.2_and_1.6
theoretical_value_p_between_1.2_and_1.4 < simulated_p_between_1.2_and_1.6
theoretical_value_p_between_1.2_and_1.4 > simulated_p_between_1.2_and_1.6

#d
#P(-1.2 < Z < 1.3)
sim_vecto1 <- z < 1.3 
sim_vecto2 <- z < -1.2
simulated_p_between_minus_1.2_and_1.3 <- sum(sim_vecto1 == TRUE)/length(sim_vecto1)-
                  sum(sim_vecto2 == TRUE)/length(sim_vecto2)

theoretical_value_p_between_minus_1.2_and_1.3 <- pnorm(1.3,0,1) - pnorm(-1.2,0,1)

#Compare with the theoretical values.
theoretical_value_p_between_minus_1.2_and_1.3 == simulated_p_between_minus_1.2_and_1.3
theoretical_value_p_between_minus_1.2_and_1.3 < simulated_p_between_minus_1.2_and_1.3
theoretical_value_p_between_minus_1.2_and_1.3 > simulated_p_between_minus_1.2_and_1.3

###################### 2nd Exercise ###########################
# sum up random sample of 8 values [0,1]
r.s <- rnorm(8)
sum(r.s^2)

# Simulate a chi square random variable on 8 degrees of freedom by
# repeating random chi square variable generation 1000 times
simulated_chi_sq <- rep(NA, 10000)
for (i in 1:10000){
  simulated_chi_sq[i] <-sum(rnorm(8)^2)}
hist(simulated_chi_sq)

# estimate mean and variance
mean_simulated_chi_sq <-mean(simulated_chi_sq)
variance_simulated_chi_sq <- var(simulated_chi_sq)

# distribution's mean is equal to the number of degrees of freedom
real_mean = 8

# The variance is equal to two times the number of degrees of freedom
real_variance = 16

# Compare estimated mean with real mean
mean_simulated_chi_sq < real_mean
mean_simulated_chi_sq > real_mean
mean_simulated_chi_sq == real_mean

# Compare estimated variance with real variance
variance_simulated_chi_sq < real_variance
variance_simulated_chi_sq > real_variance
variance_simulated_chi_sq == real_variance

# Plot the PDF
density_values <- hist(simulated_chi_sq, plot =F)['density']
plot(density(simulated_chi_sq))

#include density distribution to plot of simulated_chi_sq 
hist(simulated_chi_sq, freq = FALSE,  main="density included", col="green")
lines(density(simulated_chi_sq), col="black")

# Plot the CDF
plot(ecdf(simulated_chi_sq))

#include cumulative distribution to plot of simulated_chi_sq 
hist(simulated_chi_sq, freq = FALSE,  main="cumulativ distribution included")
lines(ecdf(simulated_chi_sq), col="red")


###################### 3rd Exercise ###########################
#loading iris dataset
data(iris)

# install "modeest" package
install.packages("modeest")

#load "modeest" package
library("modeest")

#compute mean, median, mode for "Petal.Length"
mean(iris$Petal.Length)
median(iris$Petal.Length)
mfv(iris$Petal.Length)

#range, variance, standard deviation, IQR for "Sepal.Length" for the species "setosa"
setosa_data<-iris[iris$Species == "setosa", ]
range(setosa_data$Sepal.Length)
var(setosa_data$Sepal.Length)
sd(setosa_data$Sepal.Length) #square root of variance
IQR(setosa_data$Sepal.Length)

#kurtosis, skewness for "Petal.Width"
#check visually by ploting the histogram
hist(iris$Petal.Width, col = 'pink')

# install "moments" package
install.packages("moments")

#load "moments" package
library("moments")

skewness(iris$Petal.Width) #symmetry
kurtosis(iris$Petal.Width)

# is the distribution symmetric?
# Skewness belongs to [-0.5, 0.5] interval, that means a symmetric distribution 

# is the distribution mesokurtic?
# Kurtosis's value is less than 3, which means a platykurtic distribution

###################### 4th Exercise ###########################
#Construct the histogram of the variable "Petal.Length" in "iris"
hist(iris$Petal.Length, xlab="petal lenght",
     main="Histogram of iris petal length")

#change the bins in an appropriate manner.
hist(iris$Petal.Length, col="tomato3", breaks = 6, xlab="petal lenght",
     main="Histogram of iris petal length")

#Add the density function to the graph.
#set the option prob=TRUE -> the probability densities are plotted
hist(iris$Petal.Length, col="goldenrod1", breaks = 6, xlab="petal lenght",
                main="Histogram of iris petal length", prob=TRUE)
lines(density(iris$Petal.Length), lty="dotted", col="gray4", lwd=2) 

#Construct the boxplot of a variable of your choice with respect to each species.

#extract all iris species by extracting unique elements of column species
unique(iris[['Species']])

setosa <- iris[Species=='setosa',]$Petal.Length
setosa

versicolor <- iris[Species=='versicolor',]$Petal.Length
versicolor 

virginica <- iris[Species=='virginica',]$Petal.Length
virginica

boxplot(setosa, versicolor, virginica, main = "Petal.Length",
        names=c("setosa", "versicolor", "virginica"), col = c("yellow"),
        border = "darkgreen", horizontal =FALSE)

boxplot(setosa, versicolor, virginica, main = "Petal.Length",
        names=c("setosa", "versicolor", "virginica"), col = c("yellow"),
        border = "darkgreen", horizontal =TRUE)
