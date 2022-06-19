##################### 1st Exercise ##############################
## One-sample test.
# Hollander & Wolfe (1973), Hamilton depression scale factor measurements in 9 
# patients with mixed anxiety and depression, taken at the first (x) and second
# (y) visit after initiation of a therapy (administration of a tranquilizer).
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)

wilcox.test(x, y, paired = TRUE, alternative = "greater", conf.level = 0.95)

#################### @nd Exercise ################################
library(MASS)
data("quine")
?quine

# we can tally the student ethnicity against the gender with the table function.
# As the result shows, within the Aboriginal student population, 38 students are
# female. Whereas within the Non-Aboriginal student population, 42 are female.
table(quine$Eth, quine$Sex)

# We apply the prop.test function to compute the difference in female proportions.
 prop.test(table(quine$Eth, quine$Sex), correct=FALSE)

# The 95% confidence interval estimate of the difference between the female 
# proportion of Aboriginal students and the female proportion of Non-Aboriginal 
# students is between -15.6% and 16.7%.
 
##################### 3rd Exercise ##############################
# We are going to use a data set called InsectSprays. 6 different insect sprays 
# (1 Independent Variable with 6 levels) were tested to see if there was a 
# difference in the number of insects found in the field after each spraying 
# (Dependent Variable).
 data(InsectSprays) 
 ?InsectSprays
 head(InsectSprays)
 attach(InsectSprays)
 str(InsectSprays)
 
# We will use the tapply() function which is a helpful shortcut in processing 
# data, basically allowing you to specify a response variable, a factor 
# (or factors) and a function that should be applied to each subset of the 
# response variable defined by each level of the factor. I.e. Instead of doing:
 mean(count[spray=="A"])   # and the same for B, C, D etc.
 
# We use tapply(response,factor,function-name) as follows
 # Let`s look at the means:
tapply(count, spray, mean)
 
# The variances:
tapply(count, spray, var)

# And sample sizes
tapply(count, spray, length)

# And a boxplot:
boxplot(count ~ spray) 

# If we want to check that a variable is a factor (especially for variables with
# numbers as factor levels), we can use:
is.factor(spray)

# We can test for homogeneity of variance for more than two groups using:
 bartlett.test(count ~ spray)
 
# !!!! In this case the significant result suggests variances are not 
# homogeneous, which violates one assumption of ANOVA.
# For the purposes of today`s session we will go ahead with an ANOVA. This is 
# not entirely unjustified, as ANOVA is moderately robust against the violation 
# of equal variances if the number in each group is equal; and it is possible 
# to include a correction.
 aov.out = aov(count ~ spray, data=InsectSprays)
 summary(aov.out)

 
