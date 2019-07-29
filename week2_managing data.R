#Latika Yadav
#ANLY 506-52- B-2019/Summer - Exploratory Data Analytics
#Code Portfolio - Week 2 - Importing, saving and managing data

# Print my current working directory
getwd()

## Change my working directory to the following path setwd(dir = "path")

setwd(dir = "/Users/ylatika/Documents/Harrisburg/EDA 506 52 B/R/Workshop-R-Workflow/R code")

# Print all the objects in my workspace
ls()

# Create some objects that we'll save later
study1.df <- data.frame(id = 1:5, sex = c("m", "m", "f", "f", "m"), score = c(51, 20, 67, 52, 42))
score.by.sex <- aggregate(score ~ sex, FUN = mean, data = study1.df)
study1.htest <- t.test(score ~ sex, data = study1.df)

# Save two objects as a new .RData file in the data folder of my current working directory
save(study1.df, score.by.sex, study1.htest, file = "data/study1.RData")

# Save my (all objects in) workspace to complete_image.RData in the data folder of my working directory
save.image(file = "data/projectimage.RData")

# Load objects in study1.RData into my workspace
load(file = "data/study1.RData")

# Load all objects in projectimage.RData into my workspace
load(file = "data/projectimage.RData")

# Remove huge.df from workspace
#rm(huge.df)

# Remove ALL objects from workspace
rm(list = ls())

# Write the pirates dataframe object to a tab-delimited text file called pirates.txt in my working directory
pirates <- c(12,13,14,"no", "yes")
write.table(x = pirates,file = "pirates.txt",  # Save the file as pirates.txt
                                    sep = ",") # Make the columns tab-delimited using "\t"

###The three critical arguments to read.table() are file, sep, header and stringsAsFactors

# Read a tab-delimited text file called mydata.txt from the data folder in my working directory into R and store as a new object called mydata

mydata <- read.table(file = 'data/pirates.txt',    # file is in a data folder in my working directory
                     sep = "," ,                 # file is tab--delimited
                     header = TRUE,               # the first row of the data is a header row
                     stringsAsFactors = FALSE)    # do NOT convert strings to factors!!

# Read a text file from the web
fromweb <- read.table(file = 'http://goo.gl/jTNf6P',
                      sep = '\t',
                      header = TRUE)
#practice
a <- data.frame("sex" = c("m", "f", "m"),
                "age" = c(19, 43, 25),
                "favorite.movie" = c("Moon", "The Goonies", "Spice World"))
b <- mean(a$age)

c <- table(a$sex)
#objects in workspace
ls()

#read file from URL
club.df<- read.table(file='http://nathanieldphillips.com/wp-content/uploads/2015/12/club.txt', sep = '\t', header = TRUE)
#write that file
write.table(x = club.df,file = "club.txt", sep = "\t") 
#save objects
save(a,b,c,club.df, file = "data/myobjects.RData")
#cleanup
rm(list = ls())
ls()


####Practice to load from the week2, chapter 9 - AnalyzingObjects.R
#load(file = "data/myobjects.RData")
#ls()
#m_time<-mean(club.df$time)
#m_drinks<-mean(club.df$drinks)
#save.image(file = "data/myobjects.RData")

#checking objects loaded in myobjects.Rdata
load(file = "data/myobjects.RData")

