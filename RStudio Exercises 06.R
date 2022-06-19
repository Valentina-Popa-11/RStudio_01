################## 1st Exercise #####################
attach(infert)

#install and load gmodels package
install.packages("gmodels")
library("gmodels")

# a
# contingency tables: for the variables "education" and "spontaneous"
cont_table<-table(education,spontaneous)
cont_table

cont_tabs<-xtabs(~education+spontaneous,data=infert)
cont_tabs

cont_cross_table<-CrossTable(education,spontaneous)
cont_cross_table

# b
# contingency tables: for the variables "education", "induced" and "spontaneous".
cont_table_b<-table(education,induced,spontaneous)
cont_table_b

cont_tabs_b<-xtabs(~education+induced+spontaneous,data=infert)
cont_tabs_b

cont_cross_table_b<-CrossTable(education,induced,spontaneous)
cont_cross_table_b

# c
# add the marginal frequencies to both crosstabs and extract the ones
# computed on rows.
addmargins(cont_table)
margin.table(cont_table,1)

addmargins(cont_tabs)
margin.table(cont_tabs,1)

addmargins(cont_table_b)
margin.table(cont_table_b,1)

addmargins(cont_tabs_b)
margin.table(cont_tabs_b,1)

detach(infert)

################## 2nd Exercise ##################
# Compare the covariance of "Petal.Length" and "Sepal.Length" for the species 
# "virginica" to the one of the same variables for the species "setosa".
head(iris)

virginica<- subset(iris, Species=="virginica")
plot(virginica$Petal.Length,virginica$Sepal.Length)
virginica_cov<-cov(virginica$Petal.Length,virginica$Sepal.Length)
virginica_cov

setosa<- subset(iris, Species=="setosa")
plot(setosa$Petal.Length,setosa$Sepal.Length)
setosa_cov<-cov(setosa$Petal.Length,setosa$Sepal.Length)
setosa_cov

setosa_cov == virginica_cov
setosa_cov > virginica_cov
setosa_cov < virginica_cov 
# both species have a positive covariance values, but it doesn't indicate a 
# a positive linear relationship between setosa's variables because covariance 
# value is near zero(0.01) -> there is no clear positive or negative relationship
# Regarding virginica's covariance, we can say that there is a positive relationship

# Compute the covariance of the variables "Petal.Width" and
# "Sepal.Width" and comment on the direction of the association.

virginica_width<- subset(iris, Species=="virginica")
plot(virginica_width$Petal.Width,virginica_width$Sepal.Width)
virginica_width_cov<-cov(virginica_width$Petal.Width,virginica_width$Sepal.Width)
virginica_width_cov

setosa_width<- subset(iris, Species=="setosa")
plot(setosa_width$Petal.Width,setosa_width$Sepal.Width)
setosa_width_cov<-cov(setosa_width$Petal.Width,setosa_width$Sepal.Width)
setosa_width_cov

setosa_width_cov == virginica_width_cov
setosa_width_cov > virginica_width_cov
setosa_width_cov < virginica_width_cov
# covariance value is near zero, we may interpret there is no clear positive or
# negative relationship

############### 3rd Exercise ###################
# Compute the correlation coefficient for the same variables as in the
# lecture example for the species "setosa";
setosa<- subset(iris, Species=="setosa")
setosa_cor<-cor(setosa$Petal.Length, setosa$Sepal.Length)
setosa_cor

# do the same for the variables "Petal.Width" and "Sepal.Width" for the species
# "verginica".
virginica<- subset(iris, Species=="virginica")
virginica_cor<-cor(virginica$Petal.Width, virginica$Sepal.Width)
virginica_cor

# Explain your findings.
# A correlation coefficient is a numerical measure of a statistical relationship
# between two variables. The correlation coefficient takes values in the interval
# [-1; 1]. A value close to 1 means that the two data sets are strongly and
# positively correlated (they both linearly grow in the same way). A value close
# to -1 indicates that the data sets are strongly but negatively correlated (when
# one increases, the other decreases in value). A value of 0 means there is no 
# linear relationship between the data sets. 
# Analyzing setosa Petal.Length and  setosa Sepal.Length correlation we notice 
# a value of 0.267 which means a weak uphill (positive) linear relationship. 
# Analyzing virginica Petal.Width and  virginica Sepal.Width correlation we 
# notice a value of 0.537 which means a moderate uphill (positive) linear relationship.
# We do not know the p value which is a measure of the probability that the observed
# result could be occurred just by random choice. P should be <.05 which can be
# translated as less that 5% change that the observed result could be occurred
# by random choice. Without p, our correlation is incomplete. By using cor.test()
# we can also see the p value
cor.test(setosa$Petal.Length, setosa$Sepal.Length) #p>.05
cor.test(virginica$Petal.Width, virginica$Sepal.Width) #p<.01
# !!! Correlation does not imply causation!

################### 4th Exercise ##################
#Use functions from the ggplot2 package (ggplot()).
install.packages("ggplot2")
library("ggplot2")

# Plot a histogram of the "earconch" measurements for the "possum" data in DAAG.
# The distribution should appear bimodal (two peaks).This is a simple indication 
# of clustering, possibly due to sex differences. Obtain side-by-side boxplots 
# of the male and female earconch measurements. How do these measurement 
# distributions differ?
install.packages("DAAG")
library("DAAG")
head(possum)

# plot the histogram
ggplot(data=possum, aes(x=earconch)) + geom_histogram(color="black", fill="white")

# plot side-by-side boxplots for both genders
ggplot(data=possum, aes(x=sex,y=earconch, color=sex)) +
  geom_boxplot()

# Can you predict what the corresponding histograms would look like? 
# as we can see in boxplots there are no outsiders, the boxes give an indication 
# of the location and spread of the central portion of the data which is quite 
# close for both genders, but the median values differ. Also, the extent of the 
# lines (the “whiskers”) show a difference between the range of the bulk of the 
# data for genders. In conclusion, it won't be a big difference between histograms,

# Plot them to check your answer.
# Change histogram plot line colors by groups using ggplot
ggplot(data=possum, aes(x=earconch, color=sex)) + geom_histogram(fill="white")

# Plot them by using basic histogram function
par(mfrow = c(1, 2))
hist(possum$earconch[possum$sex == "f"], border = "red", col = "white",
     xlab= "ear conch", main = "Female")
hist(possum$earconch[possum$sex == "m"], border = "blue",col = "white",
     xlab="ear conch",main = "Male")

# The histograms make it clear that sex differences are not the whole of
# the explanation for the bimodality













