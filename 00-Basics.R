# R Basics

# https://www.rstudio.com/wp-content/uploads/2016/01/rstudio-IDE-cheatsheet.pdf

# Workspace Environment
# When saving your R working session, these are the components along with the script files that will be saved in your working directory

# returns path for the current working directory
getwd()
# set the working directory to a specified directory
setwd("/Users/jose/Documents/GitHub/master_data_science")   


# The workspace environment will also list your user defined objects such as vectors, matrices, data frames, lists, and functions
# For example
x <- 2
y <- 3

# list all objects
ls()              

# identify if an R object with a given name is present
exists("x")        

# remove defined object from the environment
rm(x)            

# you can remove multiple objects
rm(x, y)  

# basically removes everything in the working environment 
rm(list = ls())       


# Getting Help ------------------------------------------------------------
help(mean)      # provides details for specific function 
?mean           # provides same information as help(functionname) 
example(mean)



# Working with packages ---------------------------------------------------

# Installing and loading Packages 

# The most common place to get packages from is CRAN

install.packages("dplyr")

# load the package to use in the current R session
library(dplyr)      

# use a particular function within a package without loading the package: packagename::functionname  
dplyr::select(iris, 
              'Sepal.Length')

help(package = "dplyr")      # provides details regarding contents of a package
vignette(package = "dplyr")
vignette("dplyr")      # view specific vignette

# Installing for GitHub
install.packages("devtools")
library(devtools)
install_github("hadley/dplyr")

# Quick list of useful R packages: https://support.rstudio.com/hc/en-us/articles/201057987-Quick-list-of-useful-R-packages


# Exercises
# ggplot2 is an extremely popular package for data visualization and is available from CRAN. Perform the following tasks:
#   
# Install the ggplot2 package.
# Load the ggplot2 package.
# Access the help documentation for the ggplot2 package.
# Check out the vignette(s) for ggplot2



# Assignment & Evaluation -------------------------------------------------

# assignment
x <- 3

# evaluation
x


y <- 3 
z <- 4
x * y * z

# R as a Calculator -------------------------------------------------------
8 + 9 / 5 ^ 2

1 / 7

options(digits = 3)
1 / 7

42 / 4          # regular division
42 %/% 4        # integer division
42 %% 4         # modulo (remainder)


# Miscellaneous Mathematical Functions

x <- 10

abs(x)      # absolute value
sqrt(x)     # square root
exp(x)      # exponential transformation
log(x)      # logarithmic transformation
cos(x)      # cosine and other trigonometric functions


# Infinite, and NaN Numbers
1 / 0           # infinity
Inf - Inf       # infinity minus infinity (res: not a number)
-1 / 0          # negative infinity
0 / 0           # not a number
sqrt(-9)        # square root of -9



# Vectorization -----------------------------------------------------------

x <- c(1, 3, 4)
y <- c(1, 2, 4)

x
y

# empty vector 
z <- as.vector(NULL)

# `for` loop to add corresponding elements in each vector
for (i in seq_along(x)) {
  z[i] <- x[i] + y[i]
  print(z)
}

z

# in R, + is a vectorized function
# add each element in x and y
x + y

# multiply each element in x and y
x * y

# compare each element in x to y
x > y

# Note that there are no scalars in R, so c is actually a vector of length 1
c <- 3


######################
# Data types
######################

# Dealing with Numbers ----------------------------------------------------
dbl_var <- c(1, 2.5, 4.5)  
dbl_var

# placing an L after the values creates a string of integers
int_var <- c(1L, 6L, 10L)
int_var

# identifies the vector type (double, integer, logical, or character)
typeof(dbl_var)
typeof(int_var)

# Converting Between Integer and Double Values

as.numeric(int_var)
as.integer(dbl_var)


# Generating Non-random Numbers
# create a vector of integers between 1 and 10
1:10     

# Generating Regular Sequences
# generate a sequence of numbers from 1 to 21 by increments of 2
seq(from = 1, to = 21, by = 2)  

# generate a sequence of numbers from 1 to 21 that has 15 equal incremented numbers
seq(0, 21, length.out = 15)

# Generating Repeated Sequences
# replicates the values in x a specified number of times in a collated fashion
rep(1:4, times = 2) 
# replicates the values in x in an uncollated fashion
rep(1:4, each = 2)


# Comparing Numeric Values

x <- c(1, 4, 9, 12)
y <- c(4, 4, 9, 13)
x == y
# How many pairwise equal values are in vectors x and y
sum(x == y) 

# Where are the pairwise equal values located in vectors x and y
which(x == y)    

x[which(x == y)] # Subsetting: more on this later

# Exact Equality

x <- c(4, 4, 9, 12)
y <- c(4, 4, 9, 13)

identical(x, y)

# Rounding numeric Values

x <- c(1, 1.35, 1.7, 2.05, 2.4, 2.75, 3.1, 3.45, 3.8, 4.15, 4.5, 4.85, 5.2, 5.55, 5.9)

# Round to the nearest integer
round(x)

# Round up
ceiling(x)

# Round down
floor(x)

# Round to a specified decimal
round(x, digits = 1)


# Dealing with Characters -------------------------------------------------

# Creating Strings

a <- "learning to create"    # create string a
b <- "character strings"     # create string

# paste together string a & b
paste(a, b)

# paste character and number strings (converts numbers to character class)
paste("The life of", pi)           

# paste multiple strings
paste("I", "love", "R")            

# paste multiple strings with a separating character
paste("I", "love", "R", sep = "-")  

# use paste0() to paste without spaces btwn characters
paste0("I", "love", "R")

# paste objects with different lengths
paste("R", 1:5, sep = " v1.")     


x <- "print strings"

# substitute a single string/variable
sprintf("Learning to %s in R", x)   
# substitute multiple strings/variables
y <- "in R"
sprintf("Learning to %s %s", x, y)   

# For integers, use %d or a variant:
version <- R.version$major
version <- as.numeric(version)
sprintf("This is R version: %d", version)

# Counting string elements and characters
length("How many elements are in this string?")
length(c("How", "many", "elements", "are", "in", "this", "string?"))

nchar("How many characters are in this string?")
nchar(c("How", "many", "characters", "are", "in", "this", "string?"))

# String manipulation with stringr

# install stringr package
install.packages("stringr")

# load package
library(stringr)

# str_c() is equivalent to the paste() functions
str_c("Learning", "to", "use", "the", "stringr", "package", sep = " ")

# str_length() is similiar to the nchar() but: (compare)
# some text with NA
text = c("Learning", "to", NA, "use", "the", NA, "stringr", "package")
nchar(text)
str_length(text)

# str_sub() is similar to substr()
x <- "Learning to use the stringr package"

# alternative indexing
str_sub(x, start = 10, end = 15)

# Replacement
str_sub(x, end = 15) <- "I know how to use"
x

# Remove Leading and Trailing Whitespace
text <- c("Text ", "  with", " whitespace ", " on", "both ", " sides ")
str_trim(text, side = "both")

# Set operatons for character strings
set_1 <- c("lagunitas", "bells", "dogfish", "summit", "odell")
set_2 <- c("sierra", "bells", "harpoon", "lagunitas", "founders")

union(set_1, set_2)
intersect(set_1, set_2)
# returns elements in set_1 not in set_2
setdiff(set_1, set_2)
identical(set_1, set_2)

# Identifying if Elements are Contained in a String
'sierra' %in% set_2

# Sorting a String
sort(set_2, decreasing = TRUE)



# Dealing with Regular Expressions ----------------------------------------

# substitute $ with !
sub(pattern = "\\$", "\\!", "I love R$")

# substitute \\ with whitespace
gsub(pattern = "\\\\", " ", "I\\need\\space")


# TODO
# http://uc-r.github.io/regex

# Dealing with Factors
# http://uc-r.github.io/factors/


# Dealing with Logicals ---------------------------------------------------

x <- 5

x > 13

x <- c(5, 14, 10, 22)

x > 13

12 == 12

12 <= c(12, 11)

12 %in% c(12, 11, 8)

x <- c(12, NA, 11, NA, 8)
is.na(x)

x <- c(5, 14, 10, 22)
sum(x > 13)


# Dealing with Dates ------------------------------------------------------

# Getting current date & time
Sys.timezone()

Sys.Date()

Sys.time()

install.packages('lubridate')
library(lubridate)

now()

# Converting strings to dates
x <- c("2015-07-01", "2015-08-01", "2015-09-01")

as.Date(x)

y <- c("07/01/2015", "07/01/2015", "07/01/2015")

as.Date(y, format = "%m/%d/%Y")

library(lubridate)
ymd(x)
mdy(y)

# Create Dates by Merging Data

yr <- c("2012", "2013", "2014", "2015")
mo <- c("1", "5", "7", "2")
day <- c("02", "22", "15", "28")

# ISOdate converts to a POSIXct object
ISOdate(year = yr, month = mo, day = day)

# truncate the unused time data by converting with as.Date
as.Date(ISOdate(year = yr, month = mo, day = day))

# Extract & manipulate parts of dates

x <- c("2015-07-01", "2015-08-01", "2015-09-01")

year(x)

# default is numerical value
month(x)

# show abbreviated name
month(x, label = TRUE)

# Creating date sequences
seq(as.Date("2010-1-1"), as.Date("2015-1-1"), by = "years")
seq(as.Date("2015/1/1"), as.Date("2015/12/30"), by = "quarter")

# Calculations with dates

x <- Sys.Date()
x

y <- as.Date("2015-09-11")
x > y
x - y

# last leap year
x <- as.Date("2012-03-1")
y <- as.Date("2012-02-28")

x - y

# example with time zones
x <- as.POSIXct("2017-01-01 01:00:00", tz = "US/Eastern")
y <- as.POSIXct("2017-01-01 01:00:00", tz = "US/Pacific")

y == x
y - x


x + days(4)
x - hours(4)



# Dealing with Missing Values ---------------------------------------------

x <- c(1:4, NA, 6:7, NA)
x

is.na(x)
which(is.na(x))
x[is.na(x)]
x[!is.na(x)]
x[is.na(x)] <- mean(x, na.rm = TRUE)
x


# Data Structure Basics ---------------------------------------------------

# Identifying the Data Structure
vector <- 1:10
list <- list(item1 = 1:10, item2 = LETTERS[1:18])
matrix <- matrix(1:12, nrow = 4)   
df <- data.frame(item1 = 1:18, item2 = LETTERS[1:18])

str(vector)
str(list)
str(matrix)
str(df)

class(vector)
class(list)
class(matrix)
class(df)

# Understanding Attributes
attributes(df)
attributes(matrix)

names(df)
dim(matrix)
length(vector)
length(list)
length(df)

# Managing Vectors

# Creating Vectors
x <- c(0.5, 0.6, 0.2)
x

y <- c(TRUE, FALSE, FALSE)
y

z <- c("a", "b", "c") 
z

seq(from = 1, to = 21, by = 2) 
rep(1:4, times = 2) 

# adding additional elements to a pre-existing vector

v1 <- 8:17

c(v1, 18:22)

# Subsetting Vectors
v1
v1[2]
v1[2:4]
v1[c(2, 4, 6, 8)]
v1[-1]

# Subsetting with logical values

v1[c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)]
v1[c(TRUE, FALSE)]


# Managing Lists

# Creating Lists
l <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.5, 4.2))
str(l)

# adding names to a pre-existing list

l1 <- list(1:3, "a", c(TRUE, FALSE, TRUE))
names(l1) <- c("item1", "item2", "item3")

# Subsetting Lists

l1[1]
l1[[1]]
l1$item1

# Managing Data Frames

# Creating Data Frames
# read_csv

df <- data.frame(col1 = 1:3, 
                 col2 = c("this", "is", "text"), 
                 col3 = c(TRUE, FALSE, TRUE), 
                 col4 = c(2.5, 4.2, pi))
str(df)

# number of rows
nrow(df)

# number of columns
ncol(df)

# Adding Attributes to Data Frames
rownames(df) <- c("row1", "row2", "row3")
colnames(df) <- c("col_1", "col_2", "col_3", "col_4")
df

