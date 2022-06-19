### Valentina Popa-Chipesu


?LifeCycleSavings
head(LifeCycleSavings)
LifeCycleSavings
attach(LifeCycleSavings)

country <-rownames(LifeCycleSavings)

### Descriptive statistics
Lcs <- LifeCycleSavings

summary(Lcs) #get a brief description about data set

#check the symmetry of distribution by comparing the mean and median per each variable

mean(sr) == median(sr) #symmetric distribution
mean(sr)> median(sr) # skewed to the right (positive)
mean(sr)< median(sr) # skewed to the left (negative)

mean(pop15) == median(pop15) #symmetric distribution
mean(pop15)> median(pop15) # skewed to the right (positive)
mean(pop15)< median(pop15) # skewed to the left (negative)

mean(pop75) == median(pop75) #symmetric distribution
mean(pop75)> median(pop75) # skewed to the right (positive)
mean(pop75)< median(pop75) # skewed to the left (negative)

mean(dpi) == median(dpi) #symmetric distribution
mean(dpi)> median(dpi) # skewed to the right (positive)
mean(dpi)< median(dpi) # skewed to the left (negative)

mean(ddpi) == median(ddpi) #symmetric distribution
mean(ddpi)> median(ddpi) # skewed to the right (positive)
mean(ddpi)< median(ddpi) # skewed to the left (negative)

skewness(sr)
skewness(pop15)
skewness(pop75)
skewness(dpi)
skewness(ddpi)

install.packages("moments")
library(moments)

kurtosis(sr) # <3 - platykurtic
kurtosis(pop15) # <3 - platykurtic
kurtosis(pop75) # <3 - platykurtic
kurtosis(dpi) # around 3, so it is mezokurtic
kurtosis(ddpi) # >3 - leptokurtic

# computing the interquartile range for each variable

IQR(sr)
IQR(pop15)
IQR(pop75)
IQR(dpi)
IQR(ddpi)

# usging boxplot for pointed out the outliers
par(mfrow = c(1,1))
boxplot(sr)
boxplot(pop15)
boxplot(pop75)
boxplot(dpi)
boxplot(ddpi) # has got 2 outliers


# histogram
hist(sr, main="Histogram-Sr", breaks="Sturges", col="white")
hist(pop15, main="Histogram-Pop15", breaks="FD", col="white")
hist(pop75, main="Histogram-Pop75", breaks="FD", col="white")
hist(dpi, main="Histogram-Dpi", breaks="FD", col="white")
hist(ddpi, main="Histogram-DDpi", breaks=5, col="white")



pairs(~sr+pop15+pop75+dpi+ddpi,data=LifeCycleSavings)


# H1
plot(dpi, ddpi)
cor(dpi, ddpi)
cor.test(dpi, ddpi)


# H2
plot(dpi, pop15)
cor(dpi,pop15)
cor.test(dpi, pop15)

#H3
plot(sr, pop15)
cor(sr,pop15)
cor.test(sr, pop15)


# H4
less15 = subset(LifeCycleSavings, pop15 >= 40, select = c(dpi))
more15 = subset(LifeCycleSavings, pop15 <= 30, select = c(dpi))
t.test(less15,more15, alternative = "greater")

#H5
young = subset(LifeCycleSavings, select = pop15:dpi)
old = subset(LifeCycleSavings, select = pop75:dpi)
t.test(young,old, alternative = "greater")

# Regression
help(lm)
pairs(LifeCycleSavings)
reg <-lm(sr ~ pop15 + pop75 + ddpi, data = LifeCycleSavings )
summary(reg)
par(mfrow = c(2,2))
plot(reg)
vcov(reg) # standard error for the estimate

kmeans(LifeCycleSavings, centers = 10, iter.max = 10, nstart =1 )


summary(aov(sr~pop15+pop75+dpi))

