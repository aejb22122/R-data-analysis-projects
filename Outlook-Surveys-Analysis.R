# ------------------ Data analysis and interpreation --------------

### Analysis of survey data by Dr Annick Eudes Jean-Baptiste
### July 2018
### The 2012 Outlook Surveys, that I am using in my analysis has been conducted by GfK Knowledge Networks 
### on behalf of the University of California Irvine

# ---- Preliminairies ----

# Installing all the packages we will need
install.packages("tidyverse")
install.packages("ggplot2")
install.packages('plyr')        # This package is used for the count of categorical variables

# Loading the packages :
library(tidyverse)
library(ggplot2)
library(plyr)

# ------------------ PART 1. DATA ANALYSIS AND VISUALISATION --------------

# The research question is :
# To what extent is the perception of the US situation (W1_G2) associated with the level of income
# (W1_P20)?
# The variables of interest in our research question


# Importing the dataset
df <- read.csv("data/ool_pds.csv")

# Have a feel of the data we are working with
str(df)

# I've removed the View() and head() function here because the file is too big.
# head(df)
# View(df) # I've removed the view function here because the file is too big.

# Determining the number of rows (units of obserations) and columns (variables) in the dataset
nrow(df)
length(df)

ncol(df)

# This dataset has 2294 obserations and 436 variables.
dim(df)

# -------------------------- Section 1.1 Basis descriptive data analysis -------------------------

# ---------------------------- Examination of counts and frequency tables 

# -------- One-Way Tables
# Explatory data analysis ~ starting with one variable
# Univariate analysis
# Making simple frequency tables [counts and frequencies]. 

# Counts of personal annual income 
# W1_P20 == Which of the following income groups includes YOUR personal annual income (Do
        #not include the income of other members of your household)?

table_income <- table(df$W1_P20, useNA = "always")
table_income
summary(factor(df$W1_P20))

# Counts of perseption
# W1_F1 == when you think about your future, are you generally optimistic, 
#       pessimistic, or neither optimistic nor pessimistic?
table_perception <- table(df$W1_F1, useNA = "always")
table_perception
summary(factor(df$W1_F1))       # Because it's a categorical variable! we use the summary(factor())

# Counts of the W1_G2 variable 
# W1_G2 == US economic situation 1 = Better, 2 = About the same, 3 = Worse, 
# -1 = Refused to answer.
# Table de la variable avec les niveaux de la variable:
table_US_eco_situation <- table(df$W1_G2, useNA = "always")     # Create a table of counts
table_US_eco_situation
summary(factor(df$W1_G2))

# Frequency

# Frequency = count(i)/n*100
# To have the frequency, the code is simmilar, we just need to had the count/sum(counts of the variable)*100
sum(table_US_eco_situation)             # Get the sum of counts (total number of observations)
length(table_US_eco_situation)          # Get the length of the table (number of levels)


# W1_P20
feq_income <- table_income/sum(table_income)*100
feq_income

# W1_F1
feq_perception <- table_perception/sum(table_perception)*100
feq_perception

# W1_G2
feq_us_eco_situation <- table_US_eco_situation/sum(table_US_eco_situation)*100
feq_us_eco_situation

# An better, cleaner way to get the proportions:
# Proportions for a single variable table
prop.table(table(df$W1_P20, useNA = "always"))
prop.table(table(df$W1_F1, useNA = "always"))
prop.table(table(df$W1_G2, useNA = "always"))

# -------------- Section 1.2 Making data management decisions -----------

# Data Management
# Steps in data management include :

# 1. Decide on coding or not missing data
# 2. Code or not valide data that has been coded to missing
# 3. Creating secondary variables

# We will not be working with a subset of the data, because our research question, and
# our hypothesis, needs to have a view of the optimism in regards to the income level.
# 1rst we will code the missing values
# we are going to set responses of (-1 ~ Refused to Answer)  for these variables to missing,
# so that Python disregards these values. We will code the missing values (NA).

# ------------------ Data management decisions # 1 Coding or recoding missing values to NA 
# Let's start the Data Management = decision about the data, missing values 
# and creating secondary variables

# Replace the data in a field (ici -1) based on equal '==' to NA
df$W1_P20[df$W1_P20 == -1] <- NA
df$W1_F1[df$W1_F1 == -1] <- NA
df$W1_G2[df$W1_G2 == -1] <- NA

# It's a good idea to verify with a table for the newly managed variables, if the transformation is succesful

# Let's have a look at the variables with the new managed variables compared to the original variables
# The '(useNA = "always")' argument will display the missing values:
table(df$W1_P20, useNA = "always")
table(df$W1_F1, useNA = "always")
table(df$W1_G2, useNA = "always")

# We chose to group values within individual variables for the W1_P20 variable representing the income level.
# categorize quantitative variable based on customized splits are done by using the cut function
# we split the variable into 4 groups (1-7, 8-11, 12-15, 16-19)
# remember that Python starts counting from 0, not 1

# ------------------ Data management decisions # 2 Grouping values within individual variables

print("The income level is divided into 4 groups : 1-7 (5k- 24k), 8-11(25k-49k)")
print("12-15(50k-99k), 16-19 (100k-175k or more))")

df$W1_P20 <- cut(df$W1_P20, breaks = c(0, 7, 11, 15, 19))

# ... we can specify the number of brakes needed and R will compute it for us:
# df$W1_P20 <- cut(df$W1_P20, breaks = 4) ; but this can give weird results.
# With the cut function, we can also specify the labels if needed...see ?cut for more details.
# Always verify after a data management that we have what we wanted

table(df$W1_P20, useNA = "always")
summary(factor(df$W1_P20))

# We have provided data management for the variables of interest to our research question.

# ------------------ Descriptive statistics for the categorical variables

# We can also have the descriptive statistics for the categorical variables bellow with 
# the describe function, after setting these variables as categorical in python. 
# This gives us the appropriate descriptive statistics for categorical variables witch are : 
#the count, number of unique values, highest and the frequency of the highest value 
# (the spread, quantile, minimum and maximum, does not make sence for categorical variables).

# First verify that these variables are factor (categorical variables) or not?
# Factor variables are categorical variables that can be either numeric or string variables. 
is.factor(df$W1_P20)    # This is a categorical variables we did the data managment previously.
is.factor(df$W1_F1)     # This is a numerical variable (tested by doing is.numeric(df$W1_F1))
is.factor(df$W1_G2)     # This is a numerical variable.

# Converting these variables to categorical:
# df$W1_P20 <- factor(df$W1_P20)
df$W1_F1 <- factor(df$W1_F1)
df$W1_G2 <- factor(df$W1_G2)

# to verify - it should give the value TRUE
is.factor(df$W1_P20)    # This is a categorical variables when dit the data managment previously.
is.factor(df$W1_F1)
is.factor(df$W1_G2)

# Counts of categorical variables (from the 'plyr' package)
t1 <- count(df, "W1_P20")
t1
t2 <- count(df, "W1_F1")
t2
t3 <- count(df, "W1_G2")
t3

# -------------------------- Section 1.3 Creating graphs for the variables ----
# UNIVARIATE GRAPH FOR CATEGORICAL VARIABLES
bar_plot1 <- ggplot(df, aes(x = W1_P20)) + 
        geom_bar() +
        xlab("Interval of annual income :1-7 (5k- 24k), 8-11(25k-49k) 12-15(50k-99k), 16-19 (100k-175k or more)") + 
        ylab("Counts") + 
        ggtitle("Income groups reported by respondents")

bar_plot2 <- ggplot(df, aes(x = W1_F1)) + 
        geom_bar() + 
        xlab("-1 = refused, 1 = optimistic, 2 = neither optimistic nor pessimistic, 3 = pessimistic") + 
        ylab("Counts") + 
        ggtitle("Respondants views regarding their future")

bar_plot3 <- ggplot(df, aes(x = W1_G2)) + 
        geom_bar() + 
        xlab("-1 = refused, 1 = better, 2 = about the same, or 3 = worse") + 
        ylab("Counts") + 
        ggtitle("Respondants views on the nation's economy compared to one year ago")

bar_plot1
bar_plot2
bar_plot3

# BI VARIATE GRAPH :

# Use the original data frame, but put factor() directly in the plot specification
# This is the Categorical -> Categorical graph of US economy's situtation vs Personnal annual income

bar_plot1_bivariate <- ggplot(df, aes(as.factor(x = W1_P20), y = as.factor(W1_G2))) +
        geom_bar(stat="identity") +
        xlab("Personnal annual income")+
        ylab("The US economy's situation")


bar_plot2_bivariate <- ggplot(df, aes(x = W1_P20, y = W1_F1)) +
        geom_bar(stat="identity")+
        xlab("Personnal annual income")+
        ylab("How the respondants think about the future")


bar_plot1_bivariate
bar_plot2_bivariate

# ------------------ PART 2. DATA ANALYSIS --------------

# Now that we have a research question, selected the data set and managed our variables
# of interest and visualized their relationship graphically, we are ready to
# test those relationships statistically.

# A partir d'ici, toutes les variables doivent être numériques.


# -------------------------- Section 2.1 ANOVA F-Statistics ----------------------------------
# C --> Q

# If we have a bi-variate statistical analysis tools for two variables i.e. [y = ax + b + et]
# Analysis of variace Quantiative response variable (y) and Explanatory Categorical variable (x)...
# Using the 'lm' function for computing of the F-statistic and associated p value.

# As a reminder :
# W1_D1 is the variable is How would you rate the president at the time (Barack Obama)
# W1_P20 is the Personnal Annual income
# The income level is divided into 4 groups : 1-7 (5k- 24k), 8-11(25k-49k)
# 12-15(50k-99k), 16-19 (100k-175k or more))

boxplot(W1_D1 ~ W1_P20, data = df)

# ANOVA F-test : are the differences amoung the SAMPLE MEANS due to the TRUE DIFFERENCES amongs
# the population of due to the SAMPLING VARIABILITY?

# ANOVA for equality of means for two groups
anova(lm(df$W1_D1 ~ factor(df$W1_P20)))

# Or we can just use the lm function
regression1 <- lm(df$W1_D1 ~ factor(df$W1_P20)) # This will give basic informations about the regression.
summary(regression1)    # This will give the residual standard error

# To interpret this finding fully, we need to examine the actual means for the variables
# We will create a new data frame with the quatitative response variable and the categorical
# explanatory variable.
# Apply a function to each cell of a ragged array

tapply(df$W1_D1, df$W1_P20, mean)
tapply(df$W1_D1, df$W1_P20, sd)

# Fit an analysis of variance model by a call to lm for each stratum.
# ao() Fit an analysis of variance model by a call to lm for each stratum.
# This is to compare each comparaison of the means.

a1 <- aov(df$W1_D1 ~ df$W1_P20)
posthoc <- TukeyHSD(x=a1, 'df$W1_P20', conf.level = 0.95)         # df$W1_P20 == Xt
posthoc

# This gives the residual vs Fittled plot
# The normal q-q plot
# The Scale-Location plot
# The Residual vs Leverage

# Because ploting the object of a ANOVA or a Regression will give 4 plots we will request
# that the 4 plots of the regression diagnostic will be in 4 plots in on page:
# We made a two by two layout, because we know that the plot(model) will produce four plots
par(mfrow=c(2,2))
plot(a1)

par(mfrow=c(1,1))       # We go back to a single pane plot layout.

# %%
# -------------------------- Section 2.2 Chi square test of independance ------------------------------------
# C --> C

# The Chi-square test is intended to test how likely it is that an observed distribution 
# is due to chance. It is also called a "goodness of fit" statistic, because it measures 
# how well the observed distribution of data fits with the distribution that is expected 
# if the variables are independent.

# Two categorical variable

# Is the perception of the us economic situation dependent or indedendent of
# the income levels?

# In reference to our research question, the explanatory variable is the income
# level (W1_P20) the perception of the and the response variable nation’s
# economic situation (measured by W1_G2 and/or W1_F1).

# Explanatory variable, idependant (x) variable
# W1_P20: is the Personnal Annual income

# The response variables (Y):
# W1_G2: is the US economy's situation
# W1_F1: is the Percentage of how the respondants think about the future

# 1) request the contengency table of observerved counts
# Chi-square test

# The process we will use for the test of independence of the two categorical variables

# 1. Request the contingency table of observer counts;
# 2. Generate the column percentage that will show the percentage of
# 3. Calculation of the Chi-square test of independence;

contengency_table <- table(df$W1_G2, df$W1_P20)      # Contengency table (x vs y)
contengency_table

barplot(contengency_table, beside = TRUE, legend = TRUE)

# Chi-Square test :
chisq.test(contengency_table)

# Post-Hoc test

# -------------------------- Section 2.3 Pearson Correlation ------------------------------------
# Q --> Q
# ---------------------------- Relationship between two quantitative variables
# Now let's find the correlation coefficients. 
# The scatter plot on the other hand will not be useful. In general the scatterplot is not useful
# for discrete variables (i.e. those that take on a limited number of values)
?cor()
cor(x = as.numeric(df$W1_P20), y = df$W1_N1A, method = "pearson", use = "complete.obs")


# ------------------ PART 3. REGRESSION MODELING IN PRACTICE --------------
# -------------------------- Section 3.1. Testing a Basic Linear Regression Model
# Preparation and data management prior to the linear regression:
# Data preparation and data management :

# 1) Since our explanatory variable is categorical with more than two categories, 
# you will need to collapse it down to two categories, 

# Let's create a new variable to characterize the social economic status 
# For revenues less than $50,000 to $59,999 (labeled 12), they will be coded has 0; 
# and 1 otherwise

# Note: We had to re-import the dataset because of the data management done on the "Revenu" 
# variable ("W1_P20") that we have cut it in previous analysis - I don't know (yet) 
# how to bring it back to it's original state ...
# Importing the dataset and doing the data managements for the NAs :
df <- read.csv("data/ool_pds.csv")

# Replace the data in a field (ici -1) based on equal '==' to NA
df$W1_P20[df$W1_P20 == -1] <- NA
df$W1_F1[df$W1_F1 == -1] <- NA
df$W1_G2[df$W1_G2 == -1] <- NA

# It's a good idea to verify with a table for the newly managed variables, if the transformation is succesful
# Let's have a look at the variables with the new managed variables compared to the original variables
# The '(useNA = "always")' argument will display the missing values:
table(df$W1_P20, useNA = "always")
table(df$W1_F1, useNA = "always")
table(df$W1_G2, useNA = "always")

# Recoding variables --- Let's create a 2 levels variable derived form the revenues
df$SocioEcoStatus <- ifelse((df$W1_P20 > 12),
                            c(0), (1))

# Verification
table(df$SocioEcoStatus, useNA = "always")


# The results of our linear regression analysis
regression1 = lm(df$W1_D1 ~ df$SocioEcoStatus, data = df)
summary(regression1)

par(mfrow = c(2,2))
plot(regression1)

# -------------------------- Section 3.2 Testing a Multiple Regression Model ----

# We will start by doing a few data management and change the variables that 
# we'll be working with to two categories

# Let's create a new variable caracterising the level of interest in politics
# Let's create a new variable to characterize the interest in politics 

# As a reminder
# "W1_P20 is the 2 level Personnal Annual income has been transformed to a variable called SocioEcoStatus")
# "W1_G2 is the US economy's situation has been transformed to economy_situation")
# "W1_F1 is the perception of how the respondants think about the future is now : future_perception")
# 'W1_D1 is the rating of the former US president')
# "W1_A12: Do you approve or disapprove of the way Barack Obama is handling his job as President?: is now approval")

# This is doing the recoding the 'hard' way :
df$PolInt[df$W1_A1 == 1] <- 1
df$PolInt[df$W1_A1 == 2] <- 1
df$PolInt[df$W1_A1 == 3] <- 1
df$PolInt[df$W1_A1 == 4] <- 0
df$PolInt[df$W1_A1 == 5] <- 0

# Simpler to do the conditionnal :
df$PolInt <- ifelse((df$W1_A1 <= 3),
                    c(1), c(0))

df$gender <- ifelse((df$PPGENDER == 2),
                    c(0), c(1))

df$economy_situation <- ifelse((df$W1_G2 == 1),
                               c(1), c(0))

df$future_perception <- ifelse((df$W1_F1 == 1),
                               c(1), c(0))

df$approval <- ifelse((df$W1_A12 == 2),
                      c(0), c(1))

# To verify :
table(df$PolInt, useNA = "always")
table(df$gender, useNA = "always")
table(df$economy_situation, useNA = "always")
table(df$future_perception, useNA = "always")
table(df$approval, useNA = "always")

regression2 = lm(W1_D1 ~ SocioEcoStatus + PolInt, data = df)
regression2
summary(regression2)

par(mfrow = c(2,2))
plot(regression2)

par(mfrow = c(1,1))


# Sauvegarde du fichier 
save(df, file = "rda/my_data_analysis.rda")
