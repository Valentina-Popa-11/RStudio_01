####################################

# checking which are the available build-in datasets in R
data()

################# IRIS DATASET ###################

# loading iris dataset
data(iris)

# printing iris dataset
iris

# printing the head of iris dataset
head(iris)

# checking variables structure
str(iris)

# install dplyr package
install.packages("dplyr")

# load the package
library("dplyr")

# create a new data frame that consists of the numeric columns of the iris dataset
nn<-dplyr::select_if(iris, is.numeric)

# showing new variables structure
str(nn)

# calculate the means of the new data frame columns
data(nn)
mean(nn[["Petal.Length"]])
mean(nn[,"Petal.Length"])
mean(nn[["Sepal.Width"]])
mean(nn[,"Sepal.Width"])


################ BEAVERS DATASET #########################

# loading beavers dataset
data(beavers)

# printing beaver1 & beaver2 dataset
beaver1
beaver2

# add a column named id to the beaver1 dataset, where the value is always 1.
beaver1$id=1

# reprinting beaver1 dataset
beaver1

# add an id column to beaver2, with value 2
beaver2$id=2

# reprinting beaver2 dataset
beaver2

# Add datasets vertically

beavers<-rbind(beaver1, beaver2)

# install dplyr package
install.packages("dplyr")

# load the package
library("dplyr")

# find the subset where either beaver is active
active<-beavers[beavers$activ == 1, ]

# print the active subset
active



################# ORINGS DATAFRAME #################################

# install faraway package
install.packages("faraway")

# load the package
library("faraway")

# load dataset
data(orings)

# print dataset
orings

# install dplyr package
install.packages("dplyr")

# load the package
library("dplyr")

# extracting pre-launch rows from orings
pre_launch<- orings %>% slice(1:2, 4, 11, 13, 18)

# plot total incidents against temperature from pre-launch dataset
plot(pre_launch,type="b", col="green", main = "plot(pre_launch)")

# plot total incidents against temperature from orings dataset
plot(orings,type="l", col="red", main = "plot(orings)")


################## SOCSUPPORT DATAFRAME ################################

#Install package DAAG
install.packages("DAAG")

# Load package
library("DAAG")

#Load & print socsupport dataset
data(socsupport)
socsupport

#str on socsupport
str(socsupport)
plot(BDI ~ unclass(age), data=socsupport)
plot(BDI~ age, data=socsupport)

# For examination of cases where the score seems very high, explain which plot is more useful?
# Explanation: I would choose the graph shaped in this form:
# plot(BDI ~ unclass(age), data=socsupport) because it shows data in 
# a more useful manner. We got data  distribution intervals, data spreading,
# data conglomeration, mean and outliers. Knowing that there are outliers values
# in my data makes me aware about possible influence in my results. I can 
# solve it by removing them.

# Why is it necessary to be cautious in making anything of the plots for 
# students in the three oldest age categories (25-30, 31-40,40+)?
# Explanation: 
# Number of representatives of these three oldest age categories
# is extremely low.
# By analysing only this three samples we might get a wrong impressions 
# about the whole dataset.

######################### AIS DATA FRAME ##########################

# install DAAG package
install.packages("DAAG")

# load the package
library("DAAG")

# loading ais dataframe
data(ais)

# printing ais dataframe
ais

# printing the head of ais dataset
head(ais)

# getting information for each of the columns
str(ais)

# determine whether any of the columns hold missing values
sapply(ais, function(x) sum(is.na(x)))

#extract all sports names by extracting unique elements of colomn sports
singular_sports <- unique(ais[['sport']])

#Creating a new dataframe with 2 row and 11 columns named as sports name
result_df<-setNames(data.frame(matrix(ncol = length(singular_sports)+1,
nrow = 2)), singular_sports)

#rename 11th column for gender
colnames(result_df)[length(singular_sports)+1]<-'sex'

#updating 11th column with gender names
result_df[1,'sex']='f'
result_df[2,'sex']='m'

# For each sport name add in dataframe number of participants for each gender
for (i in singular_sports) {
result_df[1,i]<-apply((ais['sport']==i & ais['sex']=='f'),2,sum)
result_df[2,i]<-apply((ais['sport']==i & ais['sex']=='m'),2,sum)
}
result_df


