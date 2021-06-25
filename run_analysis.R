## Merges the train and test dataset
merge_train_test_dataset <- function(directory = "UCI HAR Dataset"){
  # params:
  # directory: path where train and test directories are stored
  # return: 
  # Merged train and test dataset including subject and activity (label)
  # with descriptive variable names.
  
  # Variable names/feature names
  features <- read.table(paste("./", "UCI HAR Dataset", "/features.txt", sep = ""))
  variable_names <- c("subject", as.character(features[, 2]), "activity")
  
  # Train set
  subject_train <- read.table(paste("./", directory, "/train/subject_train.txt", sep = ""))
  train_set_X <- read.table(paste("./", directory, "/train/X_train.txt", sep = ""))
  train_set_Y <- read.table(paste("./", directory, "/train/y_train.txt", sep = ""))
  train_set <- cbind(subject_train, train_set_X, train_set_Y)
  
  # Test set
  subject_test <- read.table(paste("./", directory, "/test/subject_test.txt", sep = ""))
  test_set_X <- read.table(paste("./", directory, "/test/X_test.txt", sep = "" ))
  test_set_Y <- read.table(paste("./", directory, "/test/y_test.txt", sep = "" ))
  test_set <- cbind(subject_test, test_set_X, test_set_Y)
  
  complete_dataset <- rbind(train_set, test_set)
  
  names(complete_dataset) <- variable_names
  
  return(complete_dataset)
}

# Transform the activities into Descriptive activities

activities_transforming_into_descriptive_activities <- function(directory = "UCI HAR Dataset"){
  # params:
  # directory: path containing train and test directories
  # return:
  # A merged train and test dataset with descriptive variable names as well as descriptive
  # activities name
  complete_dataset <- merge_train_test_dataset(directory)
  activities <- read.table(paste("./", directory, "/activity_labels.txt", sep = ""))
  activities[, 2] <- as.character(activities[, 2])
  for(i in 1:6){
    complete_dataset$activity <- gsub(i, activities[, 2][i], complete_dataset$activity)
  }
  
  return(complete_dataset)
  
}

# Extracts only the measurements on the mean and standard deviation for each measurement.
extract_measurements_mean_std <- function(directory = "UCI HAR Dataset"){
  # params:
  # directory: path containing train and test directories
  # return:
  # A Dataframe containing the measurements on the mean and 
  # standard deviation for each measurement.
  complete_dataset <- activities_transforming_into_descriptive_activities(directory)
  indexes_mean_std <- grep('mean|std', names(complete_dataset))
  measurements_mean_std <- complete_dataset[, c(1, indexes_mean_std, 563)]
  return(measurements_mean_std)
}

average_activity_subject <- function(directory = "UCI HAR Dataset"){
  # params:
  # directory: path containing train and test directories
  # return:
  # A Dataframe containing average of each variable for each activity and each subject.
  require(dplyr)
  complete_df <- extract_measurements_mean_std(directory)
  summarized <- complete_df %>%
    group_by(activity, subject) %>%
    summarise_at(vars(names(complete_df)[2]: names(complete_df)[80]), mean)
}


summary <- average_activity_subject()
write.table(summary, './tidy_dataset.txt', row.names=FALSE)
