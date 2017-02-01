

initial_setup <- function(directory) {
  # set working directory
  
 setwd(directory)

}


download_zip <- function(file_name) {
  # download and unzip file
  
  dest <- "data.zip"
  download.file(file_name, dest)
  
  unzip("data.zip")
  
}

read_data <- function(group) { 
  # read in data from file
  
  path <- paste("UCI HAR Dataset/", group, sep = "")
  
  dat_features <<- read.table("UCI HAR Dataset/features.txt", fill = TRUE)
  dat_features[, 2] <- as.character(dat_features[, 2])
  
  x_ <- read.table(paste(path, "/X_", group, ".txt", sep = ""), fill = TRUE)
  y_ <- read.table(paste(path, "/y_", group, ".txt", sep = ""), fill = TRUE)
  subject_ <- read.table(paste(path, "/subject_", group, ".txt", sep = ""), fill = TRUE)
  
  cbind(x_, y_, subject_)
  
}

merge_data <- function() {
  # merge test and train data
  
  dat_test <- read_data("test")
  dat_train <- read_data("train")
  rbind(dat_test, dat_train)
  
}

get_measurements_loc <- function() {
  # find the rows of mean/std variables in the data features set
  # the row corresponds to the column in the merged dataset
  
  loc_mean <- which(sapply(dat_features[, 2], grepl, pattern = "mean()"))
  loc_std <- which(sapply(dat_features[, 2], grepl, pattern = "std()"))
  
  loc_mean_std <<- c(loc_mean, loc_std)
  loc_mean_std
}


get_measurements <- function(dataset) {
  # get the values of the mean/std variables
  
  Activity <- dataset[, 562]
  Subject <- dataset[, 563]
  dataset_mean_std <- dataset[, get_measurements_loc()]
  
  cbind(Subject, Activity, dataset_mean_std)

}

name_activities <- function (dataset) {
  # get the names of the activities from the Activity Labels file
  # change the activities in the dataset from numbers to words
  
  dat_activity <- read.table("UCI HAR Dataset/activity_labels.txt", fill = TRUE)
  for (i in 1:nrow(dataset)) {
    dataset[i, 2] <- as.character(dat_activity[dataset[i, 2], 2])
  }
  dataset
  
}

name_variables <- function(dataset) {
  # using the location of the mean/std variables, rename the variables to their 
  # mean/std variable name
  
  names(dataset) <- c("Subject", "Activity", as.character(dat_features[loc_mean_std, 2]))
  dataset
}

main <- function() {
  
  # initial setup
  directory <- "~/datasciencecoursera/Getting-and-Cleaning-Data-Course-Project"
  file_name <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  
  initial_setup(directory)
  download_zip(file_name)
  
  ### "1. [Merge] the training and the test sets to create one data set." ###
  
  dat <- merge_data() 

  ### "2. [Extract] only the measurements on the mean and standard deviation for each 
  # +     measurement." ###
  
  
  dat <- get_measurements(dat)
  
  ### "3. [Use] descriptive activity names to name the activities in the data set." ###
  dat <- name_activities(dat)
  
  ### "4. Appropriately [label] the data set with descriptive variable names." ###
  dat <- name_variables(dat)
  
  ### "5. ... [create] a second, independent tidy data set with the average of each variable
  # +     for each activity and each subject." ###
  tidy_dat <- aggregate(dat[, 3:ncol(dat)], by = list(dat$Subject, dat$Activity), mean)
  names(tidy_dat)[1] <- "Subject"
  names(tidy_dat)[2] <- "Activity"
  for(i in 3:length(names(tidy_dat))) {names(tidy_dat)[i] <- paste("Average of", names(tidy_dat)[i])}
  
  # Cache datasets
  dat <<- dat
  tidy_dat <<- tidy_dat
  
  # write txt file of tidy_dat for submission 
  write.table(tidy_dat, "tidy_data.txt", row.names = FALSE)
}


### RUN CODE ###

  main()
  
  
  