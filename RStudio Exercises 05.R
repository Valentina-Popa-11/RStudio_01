##################### 1st Exercise #######################

#Consider the built in "islands" vector
head(islands)

# a Compare the histograms that result when using breaks based on Sturges' and
# Scott's rules. Make this comparison on the log scale and on the original scale.

#The mfrow parameter of the par() function is giving a 1 × 4 layout
par(mfrow = c(1,4))

hist(islands,xlab = "Area", main="original scale with Sturges' break",
          breaks = "Sturges")
hist(islands,xlab = "Area", main="original scale with Scott's break",
          breaks = "Scott")

hist(log(islands, 10), xlab = "Area", main="log scale with Sturges' break",
          breaks = "Sturges")
hist(log(islands, 10), xlab = "Area", main="log scale with Scott's break", 
          breaks = "Scott")

# b Construct a normal QQ plot, and compare the result with the plots in Figure 
# 3.13; which one is most similar, and what does this tell you about this data set?
par(mfrow = c(1,5))

#plots in Figure 3.13
X <- rnorm(1000)
A <- rnorm(1000)
qqplot(X, A, main = "A and X are the same")
B <- rnorm(1000, mean = 3, sd = 2)
qqplot(X, B, main = "B is rescaled X")
C <- rt(1000, df = 2)
qqplot(X, C, main = "C has heavier tails")
D <- rexp(1000)
qqplot(X, D, main = "D is skewed to the right")

#normal QQ plot
qqnorm(islands)

# The normal QQ plot is similar to the forth plot from the figure 3.13, which 
# tell us that our data are skewed to the right.

# c Construct a boxplot for these data on the log scale as well as the original
# scale.
par(mfrow = c(1,2))
boxplot(log(islands,10), main="log scale")
boxplot(islands, main="original scale")

# d Construct a dot chart of the areas. Is a log transformation needed here?
dotchart(islands, xlab = "Area", cex=0.5, main="original scale")
dotchart(log(islands), xlab = "Area", cex=0.5, main="log scale")
# Using log transformation, our data are more dispersed and it easier to identify
# the corresponding area.

# e Which form of graphic do you think is most appropriate for displaying these 
# data?
# All of them are good, depends what we want to display.The one who contains
# the most info is boxplot(log(islands)); it gives us a quick visual display of 
# the main features of the data set. The box gives an indication of the location 
# and spread of the central portion of the data, a horizontal line is drawn at
# the median, interquartile range are pointed, while the extent of the lines
# (the “whiskers”) provides an idea of the range of the bulk of the data.

######################## 2nd Exercise #############################
head(stackloss)

# (a) Use scatterplots to explore possible relationships between acid
# concentration, water temperature, and air flow and the amount of ammonia
# which escapes.
# Do these relationships appear to be linear or nonlinear?
par(mfrow = c(1,1))

scatter.smooth(stackloss$Acid.Conc.~ stackloss$Water.Temp, data=stackloss,
               lpars = list(col = "brown", lwd = 3, lty = 3))
abline(lm(stackloss$Acid.Conc. ~ stackloss$Water.Temp), col="green", lwd=3)
# It is not a straight line, but is close enough

scatter.smooth(stackloss$Acid.Conc.~ stackloss$Air.Flow,data=stackloss,
               lpars = list(col = "brown", lwd = 3, lty = 3))
abline(lm(stackloss$Acid.Conc. ~ stackloss$Air.Flow), col="green", lwd=3)
# The relationship appear as nonlinear

scatter.smooth(stackloss$Acid.Conc.~ stackloss$stack.loss,data=stackloss,
               lpars = list(col = "brown", lwd = 3, lty = 3))
abline(lm(stackloss$Acid.Conc. ~ stackloss$stack.loss), col="green", lwd=3)
# The relationship appear as nonlinear

scatter.smooth(stackloss$Water.Temp ~ stackloss$Air.Flow,data=stackloss,
               lpars = list(col = "brown", lwd = 3, lty = 3))
abline(lm(stackloss$Water.Temp ~ stackloss$Air.Flow), col="green", lwd=3)
# The relationship appear as nonlinear

scatter.smooth(stackloss$Water.Temp ~ stackloss$stack.loss,data=stackloss,
               lpars = list(col = "brown", lwd = 3, lty = 3))
abline(lm(stackloss$Water.Temp ~ stackloss$stack.loss), col="green", lwd=3)
# The relationship appear as nonlinear

scatter.smooth(stackloss$Air.Flow ~ stackloss$stack.loss, type="p",data=stackloss,
               lpars = list(col = "brown", lwd = 3, lty = 3))
abline(lm(stackloss$Air.Flow ~ stackloss$stack.loss), col="green", lwd=3)
# The relationship appear as linear

# (b) Use the pairs() function to obtain all pairwise scatterplots among
# the four variables.
pairs(~Acid.Conc.+Water.Temp+Air.Flow+stack.loss,data=stackloss)

# Identify pairs of variables where there might be linear or nonlinear 
# relationships.

scatter.smooth(stackloss$Acid.Conc.~ stackloss$Water.Temp, data=stackloss,
               lpars = list(col = "brown", lwd = 3, lty = 3))
abline(lm(stackloss$Acid.Conc. ~ stackloss$Water.Temp), col="green", lwd=3)
# It is not a straight line, but is close enough

scatter.smooth(stackloss$Acid.Conc.~ stackloss$Air.Flow,data=stackloss,
               lpars = list(col = "brown", lwd = 3, lty = 3))
abline(lm(stackloss$Acid.Conc. ~ stackloss$Air.Flow), col="green", lwd=3)
# The relationship appears as nonlinear

scatter.smooth(stackloss$Acid.Conc.~ stackloss$stack.loss,data=stackloss,
               lpars = list(col = "brown", lwd = 3, lty = 3))
abline(lm(stackloss$Acid.Conc. ~ stackloss$stack.loss), col="green", lwd=3)
# The relationship appears as nonlinear

scatter.smooth(stackloss$Water.Temp ~ stackloss$Air.Flow,data=stackloss,
               lpars = list(col = "brown", lwd = 3, lty = 3))
abline(lm(stackloss$Water.Temp ~ stackloss$Air.Flow), col="green", lwd=3)
# The relationship appears as nonlinear

scatter.smooth(stackloss$Water.Temp ~ stackloss$stack.loss,data=stackloss,
               lpars = list(col = "brown", lwd = 3, lty = 3))
abline(lm(stackloss$Water.Temp ~ stackloss$stack.loss), col="green", lwd=3)
# The relationship appears as nonlinear

scatter.smooth(stackloss$Air.Flow ~ stackloss$stack.loss, type="p",data=stackloss,
               lpars = list(col = "brown", lwd = 3, lty = 3))
abline(lm(stackloss$Air.Flow ~ stackloss$stack.loss), col="green", lwd=3)
# The relationship appears as linear

######################## 3rd Exercise ############################
# Write code that produces the figure in the right panel of Figure 3.15.
head(Orange)

plot(circumference ~ age, pch = as.numeric(as.character(Tree)),
     data = Orange)
lines(circumference ~ age, data = Orange, type = "l", subset = Tree == "1",
      lty = 1, pch = 1)
lines(circumference ~ age, data = Orange, type = "l", subset = Tree == "2",
      lty = 2, pch = 2)
lines(circumference ~ age, data = Orange, type = "l", subset = Tree == "3",
      lty  =3, pch = 3, lwd=2)
lines(circumference ~ age, data = Orange, type = "l", subset = Tree == "4",
      lty = 4, pch = 4)
lines(circumference ~ age, data = Orange, type = "l", subset = Tree == "5",
      lty = 5, pch = 5)

# Add the legend to the bottom right corner
legend("bottomright", legend = paste("Tree", 1:5), lty = 1:5, pch = 1:5, cex=0.7, 
       lwd = c(1, 1, 2, 1, 1))

###################### 4th Exercise ##########################
# Obtain an analogous set of plots to those displayed in Figure 3.18,
# but with enough room for the respective titles: \Histogram-Sturges,"
#\Histogram-FD," \Density Estimate," \Boxplot," \QQ Plot," and
#\Trace Plot."
par(mfrow = c(3,2))
x<- rnorm(1000)

hist(x, main="Histogram-Sturges", breaks="Sturges", col="white")
hist(x, main="Histogram-FD", breaks="FD", col="white")
plot(density(x), main="Density Estimate")
boxplot(x, main="Boxplot")
qqnorm(x, main = "QQ Plot")
    qqline(x)
ts.plot(x, main="Trace Plot", type = "l")

#################### 5th Exercise #########################
# Construct another set of six plots as in Figure 3.18, but this time applied 
# to the data in EuStockMarkets.
par(mfrow = c(3,2))

head(EuStockMarkets)

z<-EuStockMarkets

hist(z, main="Histogram-Sturges", breaks="Sturges", col="white")
hist(z, main="Histogram-FD", breaks="FD", col="white")
plot(density(z), main="Density Estimate")
boxplot(z, main="Boxplot")
qqnorm(z, main = "QQ Plot")
qqline(z)
ts.plot(z, main="Trace Plot", type = "l")

# Comment on the results, and use the summary() function to gain further insight.
# Which of the six plots are useful descriptors of this data set, and which may
# be limited in their usefulness?
summary(z) 

# Histogram
# It is useful if we need information about data distribution, symmetry and shape.
# In our case has limited use because it doesn't show the data distribution 
# per categories but if we analyze both histograms for the while data
# (FD and Sturges brakes)we can see that both are positive skewed and leptocurtic. 

# Density plot
# In our case, density plot it is more useful than a histogram because it doesn`t
# break the data into intervals, and we have a better view at the data evolution

# Box plot
# It contains information that complete the previous graphs and it gives us a 
# quick visual display of the main features of the data set: the box gives an 
# indication of the location and spread of the central portion of the data,
# a horizontal line is drawn at the median, interquartile range are pointed, 
# the extent of the lines (the “whiskers”) provides an idea of the 
# range of the bulk of the data and we see many outliers pointed out.

# QQ plot
# Trace Plot can be useful if we want to see similarities between data set
# distributions. It shows that our data are more spread out.

# Trace plot
# This one is the most useful graph in our case, because it provides evidence 
# for data convergence and mixing, showing up data evolution over time for all
# four categories


