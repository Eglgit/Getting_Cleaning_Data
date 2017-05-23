##----------------------------------------------------------------------------
##----------------------------------------------------------------------------
##
##  File name:  run_analysis.R
##  Date:       23May2017
##
##  This script does the following:
##  1.  Merges the training and the test sets to create one data set.
##  2.  Extracts only the measurements on the mean and standard deviation 
##      for each measurement
##  3.  Uses descriptive activity names to name the activities in the data set.
##  4.  Appropriately labels the data set with descriptive variable names.
##  5.  Creates a second, independent tidy data set with the average of each
##      variable for each activity and each subject.
##
##----------------------------------------------------------------------------
##----------------------------------------------------------------------------

##----------------------------------------------------------------------------
## Read tables for test and train data
##      We will now set the WD, download the files, unzip the file, read the file and assign each variable to
##      a R object
##----------------------------------------------------------------------------


## Set WD
        setwd("C:./Getting_Cleaning_Data")

## Create working folder
        If (!files.exists("Project")) 
        { dir.create("Project") }

## Download and read files
        zipfile <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(zipfile, "./Project/project.zip")

## Unzip the file
        unzip(zipfile = "./Project/project.zip", exdir = "./Project")

## Check the list of the files in the folder
        Files_list = list.files("./Project/UCI HAR Dataset", recursive=TRUE)

## Load libraries (reshape2 - will hep to transform data between wide and long formats + dplyr + data.table)
        library(data.table)
        library(dplyr)
        library(reshape2)

## Define objects and variables (get the files to be used into R object)
        training_x_file <- read.table("./Project/UCI HAR Dataset/train/X_train.txt", comment.char = "", colClasses="numeric")
        training_y_file <- read.table("./Project/UCI HAR Dataset/train/Y_train.txt", comment.char = "", colClasses="numeric")       
        test_x_file <- read.table("./Project/UCI HAR Dataset/test/X_test.txt", comment.char = "", colClasses="numeric")
        test_y_file <- read.table("./Project/UCI HAR Dataset/test/Y_test.txt", comment.char = "", colClasses="numeric")
        
        training_labels <- read.table("./Project/UCI HAR Dataset/train/y_train.txt", comment.char = "", colClasses="numeric")
        test_labels <- read.table("./Project/UCI HAR Dataset/test/y_test.txt", comment.char = "", colClasses="numeric")
        
        training_subject_file <- read.table("./Project/UCI HAR Dataset/train/subject_train.txt", comment.char = "", colClasses="numeric")
        test_subject_file <- read.table("./Project/UCI HAR Dataset/test/subject_test.txt", comment.char = "", colClasses="numeric")

        activity_labels <- read.table("./Project/UCI HAR Dataset/activity_labels.txt")
        features <- read.table("./Project/UCI HAR Dataset/features.txt", sep=" ", comment.char = "", colClasses="character")
        
##----------------------------------------------------------------------------
##  Q1.  Merges the training and the test sets to create one data set.
##----------------------------------------------------------------------------
        ## combine test data into one single data frame
        test_data <- cbind(test_x_file,test_subject_file,test_y_file)
        
        ## combine test data into one single data frame
        training_data <- cbind(training_x_file,training_subject_file,training_y_file)
        
        # merge test_data and training_data to create one data set called analysis.data
        analysis_data <- rbind(test_data,training_data)

##----------------------------------------------------------------------------
##  Q2.  Extracts only the measurements on the mean and standard deviation 
##      for each measurement
##----------------------------------------------------------------------------
        ## We have created 2 objects called "features" and "activity_labels".
        ## We can extract the "mean" only measurements and the std ones based on the features name
        
        ## Get "mean"
        mean_label <- which(grepl("mean(",features$V2,fixed=TRUE))
        
        ## Get "std"
        std_label <- which(grepl("std",features$V2))
        
        ## combine names which contain "mean" and names that contain "std"
        ##   into a single vector called features_label and add the last
        ##  two columns for subject and activity to vector
        features_label <- c(mean_label,std_label,562,563)
        
        
        ## extract the columns from analysis_data based on the location indicated in
        ##  feature_label vector. This was to be performed
        mean_std_data <- analysis_data[,features_label]
        
##----------------------------------------------------------------------------
##  Q3.  Uses descriptive activity names to name the activities in the data set.
##----------------------------------------------------------------------------
        ## Will be completted in several iterative steps:
          # substitute "walking" for all 1's in column V1.2 of mean_std_data
          activity <- gsub(1,"walking",mean_std_data[,"V1.2"])
          
          # substitute "walking upstairs" for all 2's in activity vector
          activity <- gsub(2,"walking upstairs",activity)
          
          # substitute "walking downstairs" for all 3's in activity vector
          activity <- gsub(3,"walking downstairs", activity)
          
          # substitute "sitting" for all 4's in activity vector
          activity <- gsub(4,"sitting",activity)
          
          # substitute "standing" for all 5's in activity vector
          activity <- gsub(5,"standing",activity)
          
          # substitute "laying" for all 6's in activity vector
          activity <- gsub(6,"laying",activity)
          
          #  Add activity as a new column to mean_std_data
          mean_std_data$activity <- activity
          
          # Remove V1.2 as a column from mean_std_data
          mean_std_data$V1.2 <- NULL
        
##----------------------------------------------------------------------------
##  Q4.  Appropriately labels the data set with descriptive variable names.
##----------------------------------------------------------------------------
          
          # create a list of column name, col_features, based on the features
          col_features <- features[features_label,"V2"]
          
          # add the labels subject and activity to col_feautures
          col_features[67:68] <- c("subject","activity")
     
          # substitute the column name prefixes of tBody, fBody, and tGravity
          col_features <- gsub("tBody","time_body_",col_features,fixed=TRUE)
          col_features <- gsub("fBody","frequency_body_",col_features,fixed=TRUE)
          col_features <- gsub("tGravity","time_gravity_",col_features,fixed=TRUE)
          
          # substitute the column name descriptors of Acc, Gyro, Jerk, Mag, and Body
          col_features <- gsub("Acc","acceleration_",col_features,fixed=TRUE)
          col_features <- gsub("Gyro","gyroscopic_",col_features,fixed=TRUE)
          col_features <- gsub("Jerk","jerk_",col_features,fixed=TRUE)
          col_features <- gsub("Mag","magnitude_",col_features,fixed=TRUE)
          col_features <- gsub("Body","body_",col_features,fixed=TRUE)
          
          # substitute the column name descriptors of mean() and std()
          col_features <- gsub("-mean()","mean_",col_features,fixed=TRUE)
          col_features <- gsub("-std()","standarddeviation_",col_features,fixed=TRUE)
          
          # substitute the column name prefixes of X, Y, and Z
          col_features <- gsub("-X","x",col_features,fixed=TRUE)
          col_features <- gsub("-Y","y",col_features,fixed=TRUE)
          col_features <- gsub("-Z","z",col_features,fixed=TRUE)
          
          # name columns in mean_std_data with the names from col_features
          colnames(mean_std_data) <- col_features
        
        
##----------------------------------------------------------------------------
##  5.  From the data set in step 4, creates a second, independent tidy data set with the average of each
##      variable for each activity and each subject.
##----------------------------------------------------------------------------
          
          ## reshape2 functions will be used
          
          # convert mean_std_data into a new data frame
          data_new <- melt(mean_std_data,id=c("subject","activity"),measure.vars=col_features[1:66])        
          
          # create tidy data set with the average of each variable for each activity and 
          #    each subject
          tidy_data <- dcast(data_new,activity + subject ~ variable,mean)
          
          # write tidy data set to a .txt file in working directory folder "UCI HAR Dataset"
          write.table(tidy_data,file="./Project/UCI HAR Dataset/tidydata.txt", sep = ",",row.names = FALSE, col.names=colnames(tidy_data))
          
          

