################ FORBES2000 DATASET #####################

# install HSAUR package
install.packages("HSAUR")

# load HSAUR package
library("HSAUR")

# load Forbes2000 data set from the HSAUR package
data("Forbes2000")

# use help() to find out what the variables represent
help(package = "HSAUR")

### Calculate the median profit for the companies 
#install dplyr package
install.packages("dplyr")

# load dplyr package
library(dplyr, warn.conflicts = FALSE)

## find the US companies' subset 
usdata<-Forbes2000[Forbes2000$country == "United States", ]

# find the median
us_prof_med<-median(usdata$profits, na.rm = TRUE)

## find the UK companies subset
ukdata<-Forbes2000[Forbes2000$country == "United Kingdom", ]

# find the median
uk_prof_med<-median(ukdata$profits, na.rm = TRUE)

## find France companies subset
frdata<-Forbes2000[Forbes2000$country == "France", ]

# find the median
fr_prof_med<-median(frdata$profits, na.rm = TRUE)

## find German companies subset
gerdata<-Forbes2000[Forbes2000$country == "Germany", ]

# find the median
ger_prof_med<-median(gerdata$profits, na.rm = TRUE)

## find UK, Germany, France companies subset
gukfdata<- c("Germany", "United Kingdom", "France")
eudata <-filter(Forbes2000, country %in% gukfdata)

# find the median for all three countries together
eu_prof_med<-median(eudata$profits, na.rm = TRUE)

######## Find all German companies with negative profit

# find German companies subset
gerdata<-Forbes2000[Forbes2000$country == "Germany", ]

#finding all German companies with negative profit
negative<-gerdata[gerdata$profits < 0, ]

# print all German companies with negative profit
negative

####### Which business category are most of the companies situated at the
# Bermuda island working in?
  
# find Bermuda companies subset
ber_data=sort(table(Forbes2000[Forbes2000$country == "Bermuda", "category" ]), decreasing=TRUE)

# show the most common business category 
ber_data[which.max(ber_data)]

######### plot sales against assets for the 50 companies with the highest profits

# order Forbes data set against profit
Forbes.ord<-Forbes2000[order(Forbes2000$profits, decreasing=TRUE),]

# extract first 50 companies with the highest profit
Forbes.50<-Forbes.ord[1:50, ]

# plot sales against assets
plot(log(sales)~log(assets), data = Forbes.50[,c(7,5)], main="Sales and Assets for first 50 countries", type="p", cex=1, pch=19, col="yellow", xlab = "assets per country", ylab="sales per country")

#labeling each point with the appropriate country name using abbreviate
text(log(sales)~log(assets), data=Forbes.50, labels=abbreviate(Forbes.50$country, minlength = 2), font=1, cex=0.5)

# Look up the plot function and its options in help
help(plot)

##############

#Find the average value of sales for the companies in each country using the mean function
mean_sales <- Forbes2000 %>% group_by(country) %>% summarise( Forbes2000 = mean(sales)) 

# print the average values of sales data set 
mean_sales

#finding the subset with companies with profits above 5 billion US dollars
above5<-Forbes2000[Forbes2000$profits > 5.0,]

#group companies with profits above 5 billion US dollars by countries
above5 %>% count(above5$country)

#################### The nth triangular number is given by n * (n + 1) / 2. Create a
# sequence of the first 20 triangular numbers.

#Define the numbers we are going to work on
n <- seq(from = 1, to = 20, by = 1)

#Find first 20 triangular numbers.
first.20<-n * (n + 1) / 2

#Associate elements of the vector that you just created with the first 20 letters of the alphabet
roman <- letters[1:20]

num_let<-data.frame(numbers=c(first.20), rom_let=c(roman))
num_let

#Select the triangular numbers where the name is a vowel.
voewls<- c("a","e","i","o","u")

name_is_vowel <-filter(num_let, rom_let %in% voewls)
name_is_vowel[,1]

################# Create a 21-by-21 matrix with the sequence 10 to 0 to 11
#Find the vector to be used on the diagonal
v1<-seq.int(10, 0, -1)
v2<-seq.int(1, 11, 1)
vec<-c(v1, v2)

#create a square matrix using diag()
sq_matrix<-diag(vec, 21, 21)

################## Create a 20-by-21 matrix with ones on the main diagonal.
ones_matrix<-diag(1, 20, 21)

#add a row of zeros above this to create a 21-by-21 square matrix
rbind(ones_matrix,0)


