#Latika Yadav
#ANLY 506-52- B-2019/Summer - Exploratory Data Analytics
#Code Portfolio - Week-4 - practicing data manipulation and understanding R data structures


x <- 1:5
y <- 6:10
z <- 11:15

# Create a matrix where x, y and z are columns
cbind(x, y, z)

# Create a matrix where x, y and z are rows
rbind(x, y, z)

# Creating a matrix with numeric and character columns will make everything a character
cbind(c(1, 2, 3, 4, 5),
      c("a", "b", "c", "d", "e"))

#The matrix() function creates a matrix form a single vector of data. 
#The function has 4 main inputs: data – a vector of data, 
#nrow – the number of rows you want in the matrix, and ncol – the number of columns you want in the matrix
#and byrow – a logical value indicating whether you want to fill the matrix by rows. 

# Create a matrix of the integers 1:10, with 5 rows and 2 columns
matrix(data = 1:10,nrow = 5,ncol = 2)

# Now with 2 rows and 5 columns
matrix(data = 1:10,nrow = 2, ncol = 5)

# Now with 2 rows and 5 columns, but fill by row instead of columns
matrix(data = 1:10, nrow = 2,ncol = 5,byrow = TRUE)

# Create a dataframe of survey data
survey <- data.frame("index" = c(1, 2, 3, 4, 5),"sex" = c("m", "m", "m", "f", "f"),"age" = c(99, 46, 23, 54, 23))
survey

#structure of the survey dataframe
str(survey)
#withour factors
survey <- data.frame("index" = c(1, 2, 3, 4, 5),"sex" = c("m", "m", "m", "f", "f"),"age" = c(99, 46, 23, 54, 23), stringsAsFactors = FALSE)
survey

#dataframes columns
# Give me a table of the supp column of ToothGrowth.
table(ToothGrowth$supp)

# Give me the len AND supp columns of ToothGrowth
head(ToothGrowth[c("len", "supp")])

# Create a new dataframe called survey
survey <- data.frame("index" = c(1, 2, 3, 4, 5),"age" = c(24, 25, 42, 56, 22))

# Add a new column called sex to survey
survey$sex <- c("m", "m", "f", "f", "m")

# Change name of 1st column of df to "a"
names(survey)[1] <- "participant.number"
survey

# Change the column name from age to age.years
#using logical indexing to change the name of the column survey$age to survey$years 
names(survey)[names(survey) == "age"] <- "years"
survey

# Create a new df with only the rows of ToothGrowth where supp equals VC
ToothGrowth.VC <- ToothGrowth[ToothGrowth$supp == "VC", ]

# Create a new df with only the rows of ToothGrowth where supp equals OJ and dose < 1
#need the , since refering to the selected rows and all columns
ToothGrowth.OJ.a <- ToothGrowth[ToothGrowth$supp == "OJ" & ToothGrowth$dose < 1, ]

# Get rows of ToothGrowth where len < 20 AND supp == "OJ" AND dose >= 1
subset(x = ToothGrowth,subset = len < 20 & supp == "OJ" & dose >= 1)

# Get rows of ToothGrowth where len > 30 AND supp == "VC", but only return the len and dose columns
subset(x = ToothGrowth, subset = len > 30 & supp == "VC", select = c(len, dose))

#all in one line subsetting and mean
mean(ToothGrowth$len[ToothGrowth$supp == "OJ"])

#Using with()
health <- data.frame("age" = c(32, 24, 43, 19, 43),
                     "height" = c(1.75, 1.65, 1.50, 1.92, 1.80),
                     "weight" = c(70, 65, 62, 79, 85))

#Calculate bmi using formula health$weight / health$height ^ 2
health$weight / health$height ^ 2

# Easier funtion is with() whic helps save some typing
with(health, height / weight ^ 2)

#practice 2: vectors -R data structures

library(tidyverse)

#vector has two key properties: 1.type and 2.length
#1. type using typeof()
typeof(letters)
typeof(1:10)
#2. length using length()
x <- list("a", "b", 1:10)
length(x)

#Important types of atomic vectors

#1. Logical
1:10 %% 3 == 0
c(TRUE, TRUE, FALSE, NA)

#2. Numeric
typeof(1)
#To make an integer we can place an L after the number
typeof(1L)
#Integers have one special value: NA, while doubles have four: NA, NaN, Inf and -Inf
1/NA
c(-1, 0, 1) / 0

#3. Character
x <- "This is a reasonably long string."
pryr::object_size(x)
#R uses a global string pool, string is stored once and pointer points to it to be referenced saving mem space
y <- rep(x, 1000)
pryr::object_size(y)

#4. Missing Values
NA            # logical
NA_integer_   # integer
NA_real_      # double
NA_character_ #chracter

#creating vectors of different types: using c() - the most complex type always wins
#int vs logical
typeof(c(TRUE, 1L))
#"integer"
#int vs double
typeof(c(1L, 1.5))
#"double"
#int vs char
typeof(c(1.5, "a"))
#"character"

#creating tibbles
#book mentioned rep() but using rep_along() since it is deprecated as of rlang 0.3.0.
tibble(x = 1:4, y = rep_along(x,1:2))

#naming vectors using set_names
#using purrr::set_names() - purrr library is functional programming tools, loaded by default
set_names(1:3, c("a", "b", "c"))

#Subsetting vectors 4 ways

#1 A numeric vector containing only integers. The integers must either be all positive, all negative, or zero.
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]
#dropping values using - sign
x[c(-1, -3, -5)]

#2 Subsetting with a logical vector keeps all values corresponding to a TRUE value.
x <- c(10, 3, NA, 5, 8, 1, NA)
#displaying all non-NA values of x vector
x[!is.na(x)]
#checking for x mod 2, will also display NA's
x[x %% 2 == 0]

#3 subsetting named vector with a character vector
x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

#4 x[1, ] selects the first row and all the columns, and x[, -1] selects all rows and all columns except the first  
health <- data.frame("age" = c(32, 24, 43, 19, 43),
                     "height" = c(1.75, 1.65, 1.50, 1.92, 1.80),
                     "weight" = c(70, 65, 62, 79, 85))
health[1,]
health[,-1]

# Creating Recursive vectors (lists)
x <- list(1, 2, 3)
x
#understanding structure of x list
str(x)
#describing the contents of the list
x_named <- list(a = 1, b = 2, c = 3)
str(x_named)
#creating a mixed data type list
y <- list("a", 1L, 1.5, TRUE)
str(y)

#subsetting lists 3 ways
#create a new list
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))

#1. using [ extracts a sub-list
str(a[1:2]) #list of 2
str(a[4]) #list of 1

#2. [[ extracts a single component from a list. It removes a level of hierarchy from the list.
str(a[[1]])
str(a[[4]]) 
#3. referecing a named component in the list using $
a$a
# using [[]] is the same as $
a[["a"]]

#Creating attributes using attr() (attributes: named list of vectors that can be attached to any object
x <- 1:10
attr(x, "greeting")
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
#seeing all attributes of x at once
attributes(x)

#Augmented vectors: Factors, Dates, Date-times, Tibbles
#Creating factors
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)
#"integer"
attributes(x)
#displays levels and class

#Dates and date-times (POSIXct” stands for “Portable Operating System Interface”, calendar time)
x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
#"double"
attributes(x)
x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
#attaching time zone info as attributes
attr(x, "tzone") <- "US/Pacific"
attr(x, "tzone") <- "US/Eastern"

#Tibbles - The difference between a tibble and a list is that all the elements of a data frame must be vectors with the same length.
#Tibbles are augmented lists - contain class “tbl_df” + “tbl” + “data.frame”, and names (column) and row.names attributes
tb <- tibble::tibble(x = 1:5, y = 5:1)
typeof(tb)
#"list"
attributes(tb)       