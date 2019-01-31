# ---------------------------- SECTION 1: R Basics, Functions, and Data Types -----------------------

# Setting the working directory and listing the files in the directory
setwd("/Users/annick/OneDrive/Documents/2_Data_analysis_research/R-for-DS/Data_Science_HavardX/Working Directory")
list.files()

# Installing the packages we will work with for this course:
install.packages("ggplot2")
library(ggplot2)
install.packages("dslabs")

# Access the data set we will be working with :
library(dslabs)
data("murders")

# See the class of the data set
class(murders)

# See the first 6 rows of the data frame
head(murders)

# Structure of the dataframe
str(murders)

# Extract the column names from a data frame.
names(murders)

# ------------ Class of the variables in the dataset

# This is a numeric variable
pop <- murders$population
class(pop)              # Numerical variable
class(murders$state)    # character variable

# this is a categorical variable (called "Factor" in R)
class(murders$region)
levels(murders$region)


# ----------- Multiple ways to access variables ----

# If you instead try to access a columns with just one bracket,
# R returns a subset of the original data frame containing just this column.
a <- murders["population"]
class(a)        # Class of "a" is a dataframe 

# To extract the population, we use double [[ ]] i.e. [["the variable"]] :
murders[["population"]]

# We can see the class of the region variable by using class
class(murders$region)

# Determine the number of regions included in this variable
length(levels(murders[["region"]]))

# Tables

# The function table takes a vector as input and returns the frequency of each unique
# element in the vector.

# Writing one line of code to show the number of states per region
table(murders["region"])

# ---------------------------- SECTION 2 : Vectors and Sorting ---------------------------------------

# Numeric Vectors
# Associate the cost values with its corresponding food item
cost <- c(50, 75, 90, 100, 150)
food <- c("pizza", "burgers", "salads", "cheese", "pasta")
names(cost) <- food

# You already wrote this code
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Associate the temperature values with its corresponding city
names(temp) <- city

# Subsetting vectors

# cost of the last 3 items in our food list:
cost[3:5]

# temperatures of the first three cities in the list:
temp[1:3]

# Access the cost of pizza and pasta from our food list 
cost[c(1,5)]

# Access the temperatures of Paris and San Juan
temp[c(3, 5)]

# Sequences

# Create a vector with the multiples of 7, smaller than 50.
seq(7, 49, 7) 

# Create a vector containing all the positive odd numbers smaller than 100.
# The numbers should be in ascending order
seq(1, 99, 2)

# Create a sequence of numbers from 6 to 55, with 4/7 increments and determine its length
length(seq(6, 55, 4/7))

# Sequences of certain length

# Store the sequence in the object a
a <- seq(1, 10, length.out = 100)
# Determine the class of a
class(a)

# Coercion
# Typecasting
# Define the vector x
x <- c(1, 3, 5,"a")
# Note that the x is character vector
x
# Typecast the vector to get an integer vector
# You will get a warning but that is ok
x <- as.numeric(x)


# Sorting (smallest -> Biggest)
library(slabs)
data("murders")
sort(murders$total)

# Order (Gives the index needed to obtain the sorted data)
# 1. obtain the index that orders according to murder total
# 2. Indexing the state names or abbreviation using that index
index <- order(murders$total)
index
murders$abb[index]

# Access population from the dataset and store it in pop
pop <- murders$population
# Use the command order, to order pop and store in object o
o <- order(pop)
# Find the index number of the entry with the smallest population size
o[1]

# Define the variable i to be the index of the smallest state
i <- which.min(murders$population)
# Define variable states to hold the states
states <- murders$state
# Use the index you just defined to find the state with the smallest population
states[i]

# Define a variable states to be the state names 
states <- murders$state
# Define a variable ranks to determine the population size ranks 
ranks <- rank(murders$population)
# Create a data frame my_df with the state name and its rank
my_df <- data.frame(name = states, ranks)

# ---- Exercice 
# Define a variable states to be the state names from the murders data frame
states <- murders$state
# Define a variable ranks to determine the population size ranks 
ranks <- rank(murders$population)
# Define a variable ind to store the indexes needed to order the population values
ind <- order(murders$population)
# Create a data frame my_df with the state name and its rank and ordered from least populous to most 
my_df <- data.frame(states = states[ind], ranks = ranks[ind])

# To get the max
max(murders$total)
i_max <- which.max(murders$total)
i_max
murders$state[i_max]

# We can do the same for the min
min(murders$total)
i_min <- which.min(murders$total)
i_min
murders$state[i_min]

# rank :  gives the rank of the original vector

# ----------------------------- SECTION 3 : Indexing, Data Wrangling, Plots --------------------------
# ----------------------------- Data Transformation ---- 
# ----------------------------- Basic Data Wrangling ----


# Loading the dplyr package and the murders dataset.

library(dplyr)
library(dslabs)
data(murders)

# --------------- Adding columns ---------------

# Redefine murders so that it includes column named rate with the per 100,000 murder rates
murders <- mutate(murders, rate = total/population*100000)

# Note that if you want ranks from highest to lowest you can take 
# the negative and then compute the ranks 
x <- c(88, 100, 83, 92, 94, 3, 8, 120)
rank(-x)        # This will give you a vector stating the positions of the variables

# Defining rate
rate <-  murders$total/ murders$population * 100000

# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
murders <- mutate(murders, rank = rank(-rate))

# --------------- Selecting -----------------

# Use select to only show state names and abbreviations from murders
select(murders, state, abb)

# You can look at the first ten rows of the selected variables
head(select(murders, state, rate), n = 10)

# --------------- filtering --------------------
# Add the necessary columns
murders <- mutate(murders, rate = total/population * 100000, rank = rank(-rate))

# Filter the dataset to show the top 5 states with the highest murder rates
top5 <- filter(murders, rank <= 5)

# --------------- Removing rows ----------------
# We can remove rows using the != operator
# Use filter to create a new data frame no_south

no_south <- filter(murders, region != "South")

# Use nrow() to calculate the number of rows
nrow(murders)
nrow(no_south)

# Create a new data frame called murders_nw with only the states from the northeast and the west
# %in% returns a logical vector indicating if there is a match or not for its left operand.

murders_nw <- filter(murders, region %in% c("Northeast", "West"))

# To verify is all is good :
# Number of states (rows) in this category 
nrow(murders_nw)
nrow(murders)

levels(murders_nw$region)
levels(murders$region)

# add the rate column
#murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# We can create a table, my_states, that satisfies both the conditions in Northeast and West with a rate < 1 : 
my_states <- filter(murders, region %in% c("Northeast", "West") & rate < 1)

# Use select to show only the state name, the murder rate and the rank
select(my_states, state, rate, rank)

# --------------------- Using the pipe operator  %>% =  une pierre d'un coup -------------------
# Using the pipe %>%
# The pipe %>% can be used to perform operations sequentially without having to define 
# intermediate objects. After redefining murder to include rate and rank.

# Load library
library(dplyr)

## Define the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# show the result and only include the state, rate, and rank columns, all in one line
mutate(filter(murders, region %in% c("Northeast", "West") & rate < 1)) %>% select(state, rate, rank)

# Mutate, filter and select

# Loading the libraries
library(dplyr)
data(murders)

# Create new data frame called my_states (with specifications in the instructions)
my_states <- murders %>% 
        mutate(rate =  total / population * 100000, rank = rank(-rate)) %>%
        filter(region %in% c("Northeast", "West") & rate < 1) %>%
        select(state, rate, rank)

# ------------------------------ Basic plots ---------------
# Scatter plot 
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)

# histogram
hist(murders$rate)
# let's see what is the extrem value
murders$state[which.max(murders$rate)]

# Boxplot
boxplot(rate ~ region, data = murders)

# Load the datasets and define some variables
library(dslabs)
data(murders)

population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total

plot(population_in_millions, total_gun_murders)

# Transform population using the log10 transformation and save to object log10_population
log10_population <- log10(murders$population)

# Transform total gun murders using log10 transformation and save to object log10_total_gun_murders
log10_total_gun_murders <- log10(murders$total)

# Create a scatterplot with the log scale transformed population and murders 
plot(log10_population, log10_total_gun_murders)


# Store the population in millions and save to population_in_millions 
population_in_millions <- murders$population/10^6


# Create a histogram of this variable
hist(population_in_millions)

# Create a boxplot of state populations by region for the murders dataset
boxplot(data = murders, population ~ region)



# ------------------------------ Missing values -------------------------------------------------

# Using new dataset 
library(dslabs)
data(na_example)

# Checking the structure 
str(na_example)

# Find out the mean of the entire dataset 
mean(na_example)
# mean returns NA if it encounters at least one NA


# Use is.na to create a logical index that tells which entries are missing (NA)
ind <- is.na(na_example)

# Determine how many NA ind has using the sum function
sum(ind)        # There is 145 missing values in this logical vector.

# Create a missing_values vector : it's full of TRUE/FALSE statements
missing_values <- is.na(na_example)

# Compute the average, for entries of na_example that are not NA 
mean(na_example[!missing_values])

# But, we can do the computation with the na.rm = TRUE
mean(na_example, na.rm = TRUE)



# ----------------------------- SECTION 4: Programming Basics -------------------------------
# Conditionnal
# for-loops
# functions

# Conditionnal if- else
# Basic idea :
"""
if(boolean condition){
        expressions
}else{
        alternative expression
}
"""

a <- 2
if(a !=0){
        print(1/a)
}else{
                print("No reciprocal of 0.")
        }
        





library(dslabs)
library(murders)

murder_rate <- murders$total/murders$population *100000

# Which state has a murder rate of 0.5
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
        print(murders$state[ind])
} else{
        print("No state has a murder rate that low!")}

# We can use functions to deal with missing values -------
# The na_example is included in the dslab package :

data("na_example")
sum(is.na(na_example)) # This will give the number of NAs in the vector

# Using the ifelse function we can replace all the NAs with 0 :
no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))



# Average function example :
avg <- function(x){
        s <- sum(x)     # These values are not saved in the workplace
        n <- length(x)
        s/n }
x <- 1:99
avg(x)

# my_function <- function(x){
#       operation that operate on x
#       which is defined by user of function
#       value final line is returned
# }


# my_function <- function(x, y, z){
#       operation that operate on x
#       which is defined by user of function
#       value final line is returned
# }

avg <- function(x, arithmetic = TRUE){
        n <- length(x)
        ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

# For loops
compute_s_n <-  function(n)
        {
        x <- 1:n
        sum(x)
}


m <- 25
# Creating a empty vector :
s_n <- vector(length = m)
for(n in 1:m){
        s_n[n] <- compute_s_n(n)
}

# Verify 
n <- 1:m
plot(n, s_n)

lines(n, n*(n+1)/2)

"""
for(i in range of values){
        operation that use i,
        which is changing across the rage of values
}
"""
for(i in 1:10){
        print(i)
}

m <- 25
# Create an empty vector :
s_n <- vector(length = m)
for(n in 1:m){
        s_n[n] <- compute_s_n(n)
}
# let's verify by making a plot :
n <- 1:m
plot(n, s_n)

# -------- Apply familly used instead of for loops -----------
apply()
sapply()
tapply()
#mapply(function, ...)

# ---- Exercice on fuctions -----
# ifelse
# The function 'nchar' tells you how many characters long a character vector is. 
# For example:
char_len <- nchar(murders$state)
head(char_len)

# The function ifelse is useful because you convert a vector of logicals into something else. 
# For example, some datasets use the number -999 to denote NA. A bad practice! You can convert the -999 in a vector to NA using the following ifelse call:
x <- c(2, 3, -999, 1, 4, 5, -999, 3, 2, 9)
ifelse(x == -999, NA, x)

# If the entry is -999 it returns NA, otherwise it returns the entry.


